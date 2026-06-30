#' Download and install the Tesseract OCR engine (Windows helper)
#'
#' @description
#' Installs the Tesseract OCR engine required by the automatic image pre-processing
#' of [km2bayes()]. On Windows it fetches the latest 64-bit installer from the
#' UB-Mannheim mirror and runs it.
#'
#' On macOS it uses Homebrew when available; on Linux it prints the command for the
#' detected package manager.
#'
#' @details
#' Two modes on Windows:
#' \itemize{
#'   \item `silent = FALSE` (default): launches the official GUI installer; the user
#'     clicks Next / Install / Finish with the default folder. The app then
#'     auto-detects `C:\\Program Files\\Tesseract-OCR`.
#'   \item `silent = TRUE`: performs an unattended install into a user-writable
#'     folder (`%LOCALAPPDATA%\\Programs\\Tesseract-OCR`, no admin needed). A single
#'     UAC prompt may still appear depending on the installer build.
#' }
#' The bundled `kmdig.py` auto-detects both locations, so no `PATH` editing is needed.
#'
#' @param silent Logical; attempt an unattended install (Windows only). Default `FALSE`.
#' @param mirror Base URL of the installer directory. Default is the UB-Mannheim mirror.
#'
#' @return (Invisibly) the path to the downloaded installer (Windows), or `NULL`.
#' @seealso [km2_setup_python()], [km2bayes()]
#' @export
km2_install_tesseract <- function(silent = FALSE,
                                  mirror = "https://digi.bib.uni-mannheim.de/tesseract/") {
  os <- tolower(Sys.info()[["sysname"]])

  if (os != "windows") {
    if (nzchar(Sys.which("tesseract"))) {
      message("Tesseract already installed at: ", Sys.which("tesseract"))
      return(invisible(NULL))
    }
    if (os == "darwin") {
      if (nzchar(Sys.which("brew"))) {
        message("Installing Tesseract via Homebrew ...")
        ok <- tryCatch(identical(system2("brew", c("install", "tesseract")), 0L),
                       error = function(e) FALSE)
        if (ok || nzchar(Sys.which("tesseract"))) {
          message("Tesseract installed."); return(invisible(NULL))
        }
      }
      message("Install Tesseract manually:\n  brew install tesseract\n",
              "(first install Homebrew from https://brew.sh if you don't have it).")
    } else {
      # Detect the available Linux package manager and print the exact command.
      mgr <- c(apt    = "sudo apt-get install -y tesseract-ocr",
               dnf    = "sudo dnf install -y tesseract",
               yum    = "sudo yum install -y tesseract",
               pacman = "sudo pacman -S --noconfirm tesseract tesseract-data-eng",
               zypper = "sudo zypper install -y tesseract-ocr",
               apk    = "sudo apk add tesseract-ocr")
      have <- names(mgr)[vapply(names(mgr), function(m) nzchar(Sys.which(m)), logical(1))]
      cmd  <- if (length(have)) mgr[[have[1]]] else mgr[["apt"]]
      message("Linux detected. Install Tesseract with:\n  ", cmd,
              "\nThen restart R. (Auto-run is skipped because it needs sudo.)")
    }
    return(invisible(NULL))
  }

  # Already installed in a location kmdig.py knows about?
  known <- c(
    file.path(Sys.getenv("ProgramFiles"), "Tesseract-OCR", "tesseract.exe"),
    file.path(Sys.getenv("ProgramFiles(x86)"), "Tesseract-OCR", "tesseract.exe"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Tesseract-OCR", "tesseract.exe")
  )
  if (any(file.exists(known)) || nzchar(Sys.which("tesseract"))) {
    message("Tesseract already installed at: ",
            c(known[file.exists(known)], Sys.which("tesseract"))[1])
    return(invisible(NULL))
  }

  # 1. Discover the latest w64 installer filename from the mirror index
  message("Looking up the latest Tesseract installer ...")
  html <- tryCatch(paste(readLines(mirror, warn = FALSE), collapse = "\n"),
                   error = function(e) NULL)
  if (is.null(html)) {
    stop("Could not reach the download mirror (", mirror, ").\n",
         "Install manually from https://digi.bib.uni-mannheim.de/tesseract/ ",
         "(file tesseract-ocr-w64-setup-*.exe), keeping the default folder.",
         call. = FALSE)
  }
  files <- unique(unlist(regmatches(
    html, gregexpr("tesseract-ocr-w64-setup-[0-9.]+\\.exe", html))))
  if (!length(files)) {
    stop("No w64 installer found on the mirror. Install manually from ", mirror, call. = FALSE)
  }
  vers <- sub("tesseract-ocr-w64-setup-([0-9.]+)\\.exe", "\\1", files)
  pick <- files[which.max(numeric_version(vers))]
  url  <- paste0(sub("/?$", "/", mirror), pick)

  # 2. Download
  exe <- file.path(tempdir(), pick)
  message("Downloading ", pick, " ...")
  utils::download.file(url, exe, mode = "wb", quiet = FALSE)

  # 3. Run
  if (silent) {
    target <- file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Tesseract-OCR")
    message("Installing silently into: ", target)
    # NSIS: /S = silent, /D = install dir (must be last, unquoted, no spaces issues here)
    code <- shell(paste0("\"", exe, "\" /S /D=", target), wait = TRUE)
    ok <- file.exists(file.path(target, "tesseract.exe"))
    if (ok) message("Tesseract installed. Restart R and run km2bayes().")
    else message("Silent install did not complete (exit ", code,
                 "). Re-run with silent = FALSE to use the GUI installer.")
  } else {
    message("Launching the Tesseract installer.\n",
            "Click Next / I Agree / Next / Install / Finish and DO NOT change the folder.\n",
            "When it finishes, restart R and run km2bayes().")
    shell.exec(exe)
  }

  invisible(exe)
}
