# ==============================================================================
# Utility Functions for ExplanatoryGraph
# ==============================================================================
#
# This module provides utility functions:
# - run_app(): Launch the interactive Shiny application
# - install_dependencies(): Install all required packages (including Bioconductor)

#' Launch the Explanatory Accounts Shiny Application
#'
#' Starts the interactive web application for building and analyzing
#' explanatory accounts using Bayesian inference.
#'
#' @param port Port number for the Shiny server (default: random available port)
#' @param launch.browser Whether to open the app in a web browser (default: TRUE)
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#'
#' @return This function does not return; it runs the Shiny app until stopped.
#'
#' @examples
#' \dontrun{
#' # Launch the app with default settings
#' run_app()
#'
#' # Launch on a specific port
#' run_app(port = 3838)
#'
#' # Launch without opening browser
#' run_app(launch.browser = FALSE)
#' }
#'
#' @export
run_app <- function(port = NULL, launch.browser = TRUE, ...) {
  # Check that we're in the project directory

  if (!file.exists(file.path(getwd(), "app.R"))) {
    stop("Could not find app.R. Make sure you are in the project directory.")
  }

  args <- list(appDir = getwd(), launch.browser = launch.browser, ...)
  if (!is.null(port)) args$port <- port
  do.call(shiny::runApp, args)
}

#' Install All Dependencies for ExplanatoryGraph
#'
#' Installs both CRAN and Bioconductor packages required by this package.
#' This function is idempotent - safe to run multiple times as it only
#' installs packages that are not already installed.
#'
#' The Bioconductor packages (gRain, graph, RBGL) require BiocManager for
#' installation. This function handles that automatically.
#'
#' @param quiet Logical; if TRUE, suppress installation messages (default: FALSE)
#'
#' @return Invisible NULL. Called for its side effect of installing packages.
#'
#' @examples
#' \dontrun{
#' # Install all dependencies
#' install_dependencies()
#'
#' # Install quietly
#' install_dependencies(quiet = TRUE)
#' }
#'
#' @export
install_dependencies <- function(quiet = FALSE) {
  # CRAN packages
  cran_pkgs <- c(
    "shiny",
    "shinydashboard",
    "shinyjs",
    "R6",
    "igraph",
    "visNetwork",
    "jsonlite",
    "DT",
    "ggplot2"
  )

  installed <- installed.packages()[, "Package"]
  missing_cran <- cran_pkgs[!cran_pkgs %in% installed]

  if (length(missing_cran) > 0) {
    if (!quiet) {
      message("Installing CRAN packages: ", paste(missing_cran, collapse = ", "))
    }
    install.packages(missing_cran, quiet = quiet)
  } else if (!quiet) {
    message("All CRAN packages already installed.")
  }

  # Bioconductor packages
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    if (!quiet) {
      message("Installing BiocManager...")
    }
    install.packages("BiocManager", quiet = quiet)
  }

  bioc_pkgs <- c("graph", "RBGL", "gRain")
  installed <- installed.packages()[, "Package"]
  missing_bioc <- bioc_pkgs[!bioc_pkgs %in% installed]

  if (length(missing_bioc) > 0) {
    if (!quiet) {
      message("Installing Bioconductor packages: ", paste(missing_bioc, collapse = ", "))
    }
    BiocManager::install(missing_bioc, ask = FALSE, update = FALSE, quiet = quiet)
  } else if (!quiet) {
    message("All Bioconductor packages already installed.")
  }

  if (!quiet) {
    message("\nAll dependencies installed successfully!")
    message("To start the app, run: source('R/package_utils.R'); run_app()")
  }

  invisible(NULL)
}
