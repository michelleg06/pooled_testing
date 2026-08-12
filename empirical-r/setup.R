# ===========================================================================================
# WELFARE MAXIMIZING POOLED TESTING
# ===========================================================================================
# Script: setup.R
# Project: Empirical analysis for the C-SEF randomized trial
# Author: Anonymous
# Purpose: Set the project paths, packages, and common output settings.
# Inputs: data/baseline.csv and data/endline.csv.
# Outputs: Path helpers and shared settings used by the R scripts.
#
# Reproducibility notes:
#   - R and package versions are recorded in renv.lock. On a new machine, install renv
#     with install.packages("renv"), then run renv::restore().
# ===========================================================================================

# ---- Project root ---------------------------------------------------------------

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Install it with install.packages('here').",
       call. = FALSE)
}

here::i_am("setup.R")
project_path <- normalizePath(here::here(), mustWork = TRUE)

# ---- Packages -------------------------------------------------------------------

attached_packages <- c(
  "dplyr",
  "data.table",
  "ggplot2",
  "sandwich",
  "lmtest",
  "fishmethods",
  "TOSTER",
  "kableExtra"
)

namespace_packages <- c(
  "here",
  "clubSandwich",
  "modelsummary",
  "MASS"
)

required_packages <- unique(c(attached_packages, namespace_packages))
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required R packages: ", paste(missing_packages, collapse = ", "),
    ". Restore the project library with renv::restore().",
    call. = FALSE
  )
}

for (package in attached_packages) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

options(modelsummary_format_numeric_html = "plain")

# ---- Paths ----------------------------------------------------------------------

data_file <- function(...) {
  here::here("data", ...)
}

figure_file <- function(...) {
  here::here("output", "figures", ...)
}

table_file <- function(...) {
  here::here("output", "tables", ...)
}

baseline_path <- data_file("baseline.csv")
endline_path <- data_file("endline.csv")

required_inputs <- c(baseline_path, endline_path)
missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0L) {
  stop("Required input not found: ", paste(missing_inputs, collapse = ", "),
       call. = FALSE)
}

output_dirs <- c(figure_file(), table_file())
invisible(lapply(output_dirs, dir.create, showWarnings = FALSE, recursive = TRUE))

# ---- Setup summary ---------------------------------------------------------------

message("project path: ", project_path)
message("baseline data: ", baseline_path)
message("endline data: ", endline_path)
message("tables: ", table_file())
message("figures: ", figure_file())
message("R version: ", R.version.string)
