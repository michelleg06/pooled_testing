# ===========================================================================================
# WELFARE MAXIMIZING POOLED TESTING
# ===========================================================================================
# Script: run_analysis.R
# Project: Empirical analysis for the C-SEF randomized trial
# Author: Michelle González Amador, Simon Finster
# Purpose: Run the complete empirical reproduction pipeline.
# Inputs: setup.R, R/analysis.R, R/utility_figure.R, data/baseline.csv, and data/endline.csv.
# Outputs: Manuscript tables in output/tables/ and the utility figure in output/figures/.
#
# Reproducibility notes:
#   - From the project root, run Rscript run_analysis.R.
# ===========================================================================================

# ---- Setup ----------------------------------------------------------------------

source("setup.R")

# ---- Run analysis ---------------------------------------------------------------

message("Running empirical analysis...")
source(file.path(project_path, "R", "analysis.R"))
message("Analysis complete.")
