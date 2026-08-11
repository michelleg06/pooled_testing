# R reproduction package

This directory reproduces the tables and utility figure used in the pilot section of the Welfare Maximizing Pooled Testing manuscript.

## Run

The environment uses R 4.6.0. From this directory, run:

```sh
Rscript run_analysis.R
```

On a new machine, first run `install.packages("renv")` followed by `renv::restore()`.
The analysis writes the manuscript tables and `utilities_by_condition.tikz/.pdf`.
`PAPER_OUTPUT_MAP.md` links the generated files to their locations in the manuscript.

The PDF figure requires `pdflatex`, the LaTeX `standalone` class, TikZ, and `pgfplots` (compatibility level 1.18). The verified toolchain is macOS arm64, TeX Live 2025, pdfTeX 1.40.28, and `pgfplots` 1.18.2. Source-package installation on another platform may require its standard system libraries; `renv::sysreqs()` lists them for supported Linux distributions.

## Contents

- `R/analysis.R`: empirical analysis and table exports.
- `R/utility_figure.R`: TikZ and PDF utility figure.
- `data/baseline.csv` and `data/endline.csv`: approved analytic inputs.
- `data/schema/variables.csv`: data dictionary.
- `setup.R` and `renv.lock`: paths, dependencies, and locked environment.
- `run_analysis.R`: public entry point.
- `output/`: manuscript-facing tables and figure.
- `PAPER_OUTPUT_MAP.md`: manuscript-to-output map.

## Data boundary

The analytic inputs contain no names, contact details, timestamps, or roster fields. They are pseudonymized participant-level data, not anonymous data. `user_id` is the only participant identifier. The non-identifying `randomization_stage` and `randomization_unit` fields reproduce the published design, and `cluster_robustness_sample` identifies the initial-stage robustness sample. Name-bearing enrollment and randomization rosters are not included.

The manuscript's recruitment count (141 - including non-consents), missing-cell totals from the unreleased non-anonymized survey exports (24 at baseline and 22 at endline), and laboratory-positive counts (1) come from study and laboratory reports that do not form part of the released survey data or trial analysis. They are therefore not reproduced by this package.

The stress score is the mean of all answered items, with items 2 and 3 reverse-scored as `6 - response`; at least one answered item is sufficient. Raw Performance is the reported Performance outcome. The primary models use the full endline sample, covariate-adjusted ANCOVA and change-score specifications, and HC1 standard errors. CR2 models are robustness checks.

The PDF omits date metadata and uses a fixed source date.
