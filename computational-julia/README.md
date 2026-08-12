# Computational Experiments Companion (Julia)

This directory contains the public reproduction materials for computational experiments 1--4. It includes the active Julia source, package-environment files, an identifier-free pilot input, stored aggregate results, and table and figure sources.

## Release notes

- The original `id` field in `pilotdata.csv` was removed. In the recovered code it served only as an opaque dictionary key; it was not a model covariate, grouping variable, or reported output.
- `experiments.jl` now creates opaque internal keys from CSV row indices. The original IDs were exactly `1:130` in row order, so this preserves the model input ordering while excluding the column.
- Pilot experiments 1 and 2 use approximation parameter `K=25`.
- Synthetic experiments 3 and 4 use approximation parameter `K=20`.
- Experiment 6, unused SCIP code, personal tooling configuration, AWS deployment notes, and manuscript PDFs are outside this focused exp1--exp4 companion.


## Contents

- `experiments.jl`: runs experiments 1--4 and writes aggregate CSV results.
- `analysis.jl`: creates summary tables and figures from the stored aggregate CSVs.
- `optimisation.jl` and `models/`: optimization and welfare routines used by the experiment driver.
- `Project.toml` and `Manifest.toml`: Julia package environment.
- `pilotdata.csv`: 130 rows containing only `baseline_utility` and `health_probability`; it has no identifier column.
- `data/exp1-data.csv` through `data/exp4-data.csv`: stored aggregate experiment results.
- `tables/`: manuscript-facing summary-table sources.
- `figs/`: manuscript-facing PGFPlots/TikZ figure sources for experiments 3 and 4.

## Experiment definitions

- Experiment 1: pilot population, pool-size constraint `G=5`, approximation parameter `K=25`.
- Experiment 2: pilot population, pool-size constraint `G=10`, approximation parameter `K=25`.
- Experiment 3: 20 synthetic populations of size 200, `G=5`, approximation parameter `K=20`.
- Experiment 4: 20 synthetic populations of size 200, `G=10`, approximation parameter `K=20`.

Pilot budgets are `2, 6, ..., 34`; synthetic budgets are `2, 4, ..., 12`.

## Requirements

- Julia 1.12.5, as recorded in `Manifest.toml`.
- Gurobi and a valid local Gurobi license for the approximation benchmark.
- MOSEK and a valid local MOSEK license for the greedy/conic optimization path.
- A LaTeX distribution such as TeX Live or MacTeX to render PDF previews from `analysis.jl`.

Solver license files and machine-specific configuration must remain outside the repository.

## Install dependencies

From this directory, run:

```sh
julia --project=. -e 'using Pkg; Pkg.instantiate()'
```

## Recreate tables and figures from stored results

This postprocessing step reads the aggregate CSVs already under `data/`:

```sh
julia --project=. analysis.jl
```

Use `--experiments` to select a subset:

```sh
julia --project=. analysis.jl --experiments 1,3
```

The analysis writes summary tables under `tables/` and experiment 3/4 ratio figures under `figs/`. PDF rendering requires `pdflatex`.

## Run the experiments

Running experiments requires both commercial solver installations and licenses:

```sh
julia --project=. experiments.jl
```

To run a subset:

```sh
julia --project=. experiments.jl --experiments 1,3
```

Each selected experiment overwrites its corresponding `data/expN-data.csv`. Preserve the distributed reference CSVs before running the driver if they are needed for comparison.

## Output interpretation

The experiment CSVs contain aggregate welfare, approximation error, execution time, welfare differences, and welfare ratios. They do not contain public-input row keys or participant-indexed allocation vectors.
