setup_environment <- function(
    pkgs = c("devtools", "tidyverse", "data.table", "arrow", "duckdb", "sf",
             "reticulate", "cowplot", "ggthemes", "kableExtra",
             "shiny", "shinyWidgets",
             "optimal2050/merra2ools", "energyRt/energyRt@v0.50",
             "ideea-model/IDEEA@v0.50"),
    log_file = "setup_log.txt",
    required_r_version = "4.2.0",
    prompt_create_project = TRUE
) {
  # --- helpers ---------------------------------------------------------------
  tstamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_conn <- file(log_file, open = "wt")
  write_log <- function(msg) {
    line <- sprintf("[%s] %s", tstamp(), msg)
    message(line)
    writeLines(line, log_conn)
  }
  wrap_try <- function(expr, on_error = NULL) {
    tryCatch(expr, error = function(e) {
      if (!is.null(on_error)) on_error(e)
      structure(FALSE, reason = e$message)
    })
  }

  on.exit(close(log_conn), add = TRUE)
  write_log("=== R Environment Setup (pak) ===")

  # --- R version check -------------------------------------------------------
  if (getRversion() < required_r_version) {
    warn <- sprintf("R %s detected; recommended >= %s.", getRversion(), required_r_version)
    warning(warn)
    write_log(paste("⚠", warn))
  } else {
    write_log(sprintf("✔ R version OK: %s", getRversion()))
  }

  # --- Ensure pak is installed (bootstrap via CRAN URL) ----------------------
  if (!requireNamespace("pak", quietly = TRUE)) {
    write_log("Installing 'pak' (bootstrap)...")
    wrap_try(
      install.packages(
        "pak",
        repos = "https://r-lib.github.io/p/pak/stable"
      ),
      on_error = function(e) write_log(paste("✘ Failed to install 'pak':", e$message))
    )
  }

  if (!requireNamespace("pak", quietly = TRUE)) {
    write_log("✘ 'pak' not available after bootstrap. Aborting package installs.")
  } else {
    write_log("✔ 'pak' is available.")
  }

  # --- Windows: Rtools check (after pak ready so we can install pkgbuild if needed)
  if (.Platform$OS.type == "windows") {
    if (!requireNamespace("pkgbuild", quietly = TRUE)) {
      write_log("Installing 'pkgbuild' to check Rtools...")
      wrap_try(pak::pkg_install("pkgbuild", ask = FALSE),
               on_error = function(e) write_log(paste("✘ Failed to install 'pkgbuild':", e$message)))
    }
    has_rtools <- FALSE
    if (requireNamespace("pkgbuild", quietly = TRUE)) {
      has_rtools <- isTRUE(wrap_try(pkgbuild::has_rtools()))
    }
    if (isTRUE(has_rtools)) {
      write_log("✔ Rtools detected and configured.")
    } else {
      write_log("✘ Rtools not found or misconfigured. Install from: https://cran.r-project.org/bin/windows/Rtools/")
    }
  }

  # --- Install requested packages with pak -----------------------------------
  results <- setNames(as.list(rep(FALSE, length(pkgs))), pkgs)

  for (pkg in pkgs) {
    write_log(sprintf("Checking package: %s", pkg))
    if (requireNamespace(pkg, quietly = TRUE)) {
      write_log(sprintf("✔ '%s' already installed.", pkg))
      results[[pkg]] <- TRUE
    } else if (requireNamespace("pak", quietly = TRUE)) {
      write_log(sprintf("Installing '%s' with pak...", pkg))
      ok <- wrap_try(
        { pak::pkg_install(pkg, ask = FALSE); TRUE },
        on_error = function(e) write_log(sprintf("✘ Failed to install '%s': %s", pkg, e$message))
      )
      results[[pkg]] <- isTRUE(ok)
      if (isFALSE(ok)) next
      if (requireNamespace(pkg, quietly = TRUE)) {
        write_log(sprintf("✔ '%s' installed successfully.", pkg))
        results[[pkg]] <- TRUE
      } else {
        write_log(sprintf("✘ '%s' appears unavailable after install.", pkg))
      }
    }
  }

  # --- RStudio detection & optional project prompt ---------------------------
  is_rstudio <- FALSE
  rstudio_reason <- NULL
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    is_rstudio <- isTRUE(wrap_try(rstudioapi::isAvailable()))
    rstudio_reason <- if (is_rstudio) "rstudioapi available & session detected" else "rstudioapi not available/connected"
  } else {
    is_rstudio <- identical(Sys.getenv("RSTUDIO"), "1")
    rstudio_reason <- if (is_rstudio) "RSTUDIO env=1" else "RSTUDIO env!=1"
  }

  if (is_rstudio) {
    write_log(paste("✔ RStudio detected (", rstudio_reason, ").", sep = ""))
  } else {
    write_log(paste("ℹ RStudio not detected (", rstudio_reason, ").", sep = ""))
    write_log("   Download: https://posit.co/download/rstudio-desktop/")
  }

  if (prompt_create_project && interactive()) {
    # ensure usethis for project creation
    if (!requireNamespace("usethis", quietly = TRUE) && requireNamespace("pak", quietly = TRUE)) {
      write_log("Installing 'usethis' for project creation...")
      wrap_try(pak::pkg_install("usethis", ask = FALSE),
               on_error = function(e) write_log(paste("✘ Failed to install 'usethis':", e$message)))
    }
    can_create <- requireNamespace("usethis", quietly = TRUE)

    if (is_rstudio && can_create) {
      write_log("Prompting to create an RStudio project (interactive)...")
      ans <- readline("Create an RStudio project in the current directory? [y/N]: ")
      if (tolower(trimws(ans)) %in% c("y", "yes")) {
        path_ans <- readline("Enter project path (blank = current directory): ")
        proj_path <- if (nzchar(trimws(path_ans))) normalizePath(path_ans, mustWork = FALSE) else getwd()
        write_log(sprintf("Creating project at: %s", proj_path))
        ok <- wrap_try(
          { usethis::create_project(proj_path, open = TRUE); TRUE },
          on_error = function(e) write_log(paste("✘ Project creation failed:", e$message))
        )
        if (isTRUE(ok)) write_log("✔ Project created and opened in RStudio.")
      } else {
        write_log("Skipped project creation.")
      }
    } else if (prompt_create_project && !can_create) {
      write_log("✘ 'usethis' not available; cannot prompt for project creation.")
    }
  } else if (prompt_create_project && !interactive()) {
    write_log("ℹ Non-interactive session: skipping project prompt.")
  }

  # --- Summary ---------------------------------------------------------------
  write_log("=== Installation Summary ===")
  for (pkg in names(results)) {
    if (isTRUE(results[[pkg]])) {
      write_log(sprintf("✔ %s: Installed/Available", pkg))
    } else {
      write_log(sprintf("✘ %s: Installation failed or unavailable", pkg))
    }
  }

  failed <- names(results)[!unlist(results)]
  if (length(failed)) {
    write_log(paste("Some packages failed:", paste(failed, collapse = ", ")))
    write_log("Suggestions:")
    write_log(" - Check internet connectivity and proxy settings.")
    write_log(" - Try pak::pkg_install(c(...)) explicitly; examine the printed error.")
    write_log(" - Ensure system build tools are present (Rtools on Windows; Xcode CLT on macOS; build-essential/libcurl-dev on Linux).")
    write_log(" - For 'arrow', ensure system libs are available or use prebuilt binaries where supported.")
    write_log(" - For 'duckdb', retries sometimes help if a binary is temporarily unavailable.")
  } else {
    write_log("All requested packages are ready to use. ✔✔✔")
  }

  invisible(results)
}

# Helper: best-available folder picker (RStudio -> Windows -> tcltk -> svDialogs -> console)
prompt_project_dir <- function(caption = "Select a folder for your RStudio project",
                               default = getwd()) {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
    sel <- tryCatch(rstudioapi::selectDirectory(caption = caption, path = default), error = function(e) NULL)
    return(if (is.null(sel)) "" else sel)
  }
  if (.Platform$OS.type == "windows" &&
      exists("choose.dir", envir = asNamespace("utils"), inherits = FALSE)) {
    sel <- tryCatch(utils::choose.dir(default = default, caption = caption), error = function(e) NA)
    return(ifelse(is.na(sel), "", sel))
  }
  if (requireNamespace("tcltk", quietly = TRUE)) {
    sel <- tryCatch(tcltk::tk_choose.dir(default = default, caption = caption), error = function(e) NA)
    return(ifelse(is.na(sel), "", sel))
  }
  if (requireNamespace("svDialogs", quietly = TRUE)) {
    sel <- tryCatch(svDialogs::dlgDir(default = default, title = caption)$res, error = function(e) "")
    return(ifelse(is.null(sel), "", sel))
  }
  message("No GUI chooser available. Type a path (blank to cancel).")
  ans <- readline(sprintf("%s\nPath [%s]: ", caption, default))
  ans <- if (nzchar(trimws(ans))) ans else ""
  if (!nzchar(ans)) return("")
  normalizePath(ans, mustWork = FALSE)
}

setup_environment <- function(
    pkgs = c("devtools", "tidyverse", "data.table", "arrow", "duckdb"),
    log_file = "setup_log.txt",
    required_r_version = "4.2.0",
    create_project = c("ask", "yes", "no"),
    project_path = NULL,
    prompt_caption = "Pick or create a folder for the RStudio project"
) {
  create_project <- match.arg(create_project)

  # --- logging helpers -------------------------------------------------------
  tstamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_conn <- file(log_file, open = "wt")
  on.exit(close(log_conn), add = TRUE)
  write_log <- function(msg) {
    line <- sprintf("[%s] %s", tstamp(), msg)
    message(line)
    writeLines(line, log_conn)
  }
  wrap_try <- function(expr, on_error = NULL) {
    tryCatch(expr, error = function(e) { if (!is.null(on_error)) on_error(e); structure(FALSE, reason = e$message) })
  }

  write_log("=== R Environment Setup (pak) ===")

  # --- R version -------------------------------------------------------------
  if (getRversion() < required_r_version) {
    warn <- sprintf("R %s detected; recommended >= %s.", getRversion(), required_r_version)
    warning(warn); write_log(paste("⚠", warn))
  } else write_log(sprintf("✔ R version OK: %s", getRversion()))

  # --- pak bootstrap ---------------------------------------------------------
  if (!requireNamespace("pak", quietly = TRUE)) {
    write_log("Installing 'pak' (bootstrap from r-lib repo)...")
    wrap_try(install.packages("pak", repos = "https://r-lib.github.io/p/pak/stable"),
             on_error = function(e) write_log(paste("✘ Failed to install 'pak':", e$message)))
  }
  if (!requireNamespace("pak", quietly = TRUE)) {
    write_log("✘ 'pak' not available after bootstrap. Package installation will be skipped.")
  } else write_log("✔ 'pak' is available.")

  # --- Windows: Rtools -------------------------------------------------------
  if (.Platform$OS.type == "windows") {
    if (!requireNamespace("pkgbuild", quietly = TRUE) && requireNamespace("pak", quietly = TRUE)) {
      write_log("Installing 'pkgbuild' to check Rtools...")
      wrap_try(pak::pkg_install("pkgbuild", ask = FALSE),
               on_error = function(e) write_log(paste("✘ Failed to install 'pkgbuild':", e$message)))
    }
    has_rtools <- FALSE
    if (requireNamespace("pkgbuild", quietly = TRUE)) {
      has_rtools <- isTRUE(wrap_try(pkgbuild::has_rtools()))
    }
    if (isTRUE(has_rtools)) write_log("✔ Rtools detected and configured.")
    else write_log("✘ Rtools not found/misconfigured. Install: https://cran.r-project.org/bin/windows/Rtools/")
  }

  # --- Install requested packages with pak -----------------------------------
  results <- setNames(as.list(rep(FALSE, length(pkgs))), pkgs)
  for (pkg in pkgs) {
    write_log(sprintf("Checking package: %s", pkg))
    if (requireNamespace(pkg, quietly = TRUE)) {
      write_log(sprintf("✔ '%s' already installed.", pkg)); results[[pkg]] <- TRUE; next
    }
    if (requireNamespace("pak", quietly = TRUE)) {
      write_log(sprintf("Installing '%s' with pak...", pkg))
      ok <- wrap_try({ pak::pkg_install(pkg, ask = FALSE); TRUE },
                     on_error = function(e) write_log(sprintf("✘ Failed to install '%s': %s", pkg, e$message)))
      results[[pkg]] <- isTRUE(ok) && requireNamespace(pkg, quietly = TRUE)
      if (isTRUE(results[[pkg]])) write_log(sprintf("✔ '%s' installed successfully.", pkg))
      else write_log(sprintf("✘ '%s' unavailable after install.", pkg))
    }
  }

  # --- RStudio detection ------------------------------------------------------
  is_rstudio <- FALSE
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    is_rstudio <- isTRUE(wrap_try(rstudioapi::isAvailable()))
  } else {
    is_rstudio <- identical(Sys.getenv("RSTUDIO"), "1")
  }
  if (is_rstudio) write_log("✔ RStudio detected.") else {
    write_log("ℹ RStudio not detected. Download: https://posit.co/download/rstudio-desktop/")
  }

  # --- Project creation flow --------------------------------------------------
  want_project <- FALSE
  if (identical(create_project, "yes")) {
    want_project <- TRUE
  } else if (identical(create_project, "ask")) {
    if (interactive()) {
      ans <- readline("Create an RStudio project now? [y/N]: ")
      want_project <- tolower(trimws(ans)) %in% c("y","yes")
    } else {
      write_log("ℹ Non-interactive session; skipping project prompt.")
    }
  }

  if (want_project) {
    # ensure usethis
    if (!requireNamespace("usethis", quietly = TRUE) && requireNamespace("pak", quietly = TRUE)) {
      write_log("Installing 'usethis' for project creation...")
      wrap_try(pak::pkg_install("usethis", ask = FALSE),
               on_error = function(e) write_log(paste("✘ Failed to install 'usethis':", e$message)))
    }
    if (!requireNamespace("usethis", quietly = TRUE)) {
      write_log("✘ 'usethis' not available; cannot create project.")
    } else {
      target <- project_path
      if (is.null(target) || !nzchar(target)) {
        if (!interactive()) {
          write_log("✘ project_path not provided and no interactivity to prompt; skipping project creation.")
          target <- ""
        } else {
          target <- prompt_project_dir(prompt_caption, getwd())
        }
      }
      if (!nzchar(target)) {
        write_log("Project creation canceled.")
      } else {
        target <- normalizePath(target, winslash = "/", mustWork = FALSE)
        write_log(sprintf("Selected project directory: %s", target))

        if (!dir.exists(target)) {
          write_log("Creating new project directory...")
          ok <- wrap_try({ usethis::create_project(target, open = FALSE); TRUE },
                         on_error = function(e) write_log(paste("✘ Project creation failed:", e$message)))
          if (isTRUE(ok)) write_log("✔ Project directory created.")
        } else {
          # Existing folder: add .Rproj if missing
          rproj_exists <- length(list.files(target, pattern = "\\.Rproj$", all.files = TRUE, ignore.case = TRUE)) > 0
          if (rproj_exists) {
            write_log("✔ Folder already contains an .Rproj file.")
          } else {
            write_log("Adding RStudio project support to existing folder...")
            old <- setwd(target); on.exit(setwd(old), add = TRUE)
            ok <- wrap_try({ usethis::use_rstudio(); TRUE },
                           on_error = function(e) write_log(paste("✘ use_rstudio() failed:", e$message)))
            if (isTRUE(ok)) write_log("✔ .Rproj file created.")
          }
        }

        # --- Open project in new RStudio window if possible ---
        if (requireNamespace("rstudioapi", quietly = TRUE) &&
            isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
          write_log("Opening project in new RStudio window...")
          wrap_try(rstudioapi::openProject(target, newSession = TRUE),
                   on_error = function(e) write_log(paste("✘ Could not open project in RStudio:", e$message)))
        } else {
          write_log("ℹ RStudio not detected or rstudioapi unavailable — cannot auto-open project.")
        }
      }
    }
  }

  # --- Summary ---------------------------------------------------------------
  write_log("=== Installation Summary ===")
  for (pkg in names(results)) {
    write_log(sprintf("%s %s: %s",
                      if (isTRUE(results[[pkg]])) "✔" else "✘",
                      pkg,
                      if (isTRUE(results[[pkg]])) "Installed/Available" else "Failed/Unavailable"))
  }
  failed <- names(results)[!unlist(results)]
  if (length(failed)) {
    write_log(paste("Some packages failed:", paste(failed, collapse = ", ")))
    write_log("Suggestions:")
    write_log(" - Check internet/proxy settings.")
    write_log(" - Try pak::pkg_install(c(...)) explicitly and read errors.")
    write_log(" - Ensure build tools (Rtools/CLT/build-essential).")
    write_log(" - For 'arrow': consider prebuilt binaries or system libs per OS.")
  } else write_log("All requested packages are ready to use 🎉")

  invisible(results)
}

