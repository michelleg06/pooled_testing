# Welfare-Maximizing Pooled Testing: Reproduction Package

This repository contains the public reproduction materials for the empirical analysis and computational experiments.

## Contents

- `empirical-r/`: pseudonymized baseline and endline data, R code, dependency records, manuscript tables and figures.
- `computational-julia/`: Julia code and environment files, an identifier-free pilot input, stored aggregate results, and tables and figures for computational experiments 1--4.

Each directory has its own README with requirements and reproduction instructions.

## Run

From `empirical-r/`, run:

```sh
Rscript run_analysis.R
```

From `computational-julia/`, instantiate the Julia environment and recreate the tables and figures from the stored results:

```sh
julia --project=. -e 'using Pkg; Pkg.instantiate()'
julia --project=. analysis.jl
```

Running the full computational experiments requires local Gurobi and MOSEK installations and licences.

## Data boundary

The released survey data are pseudonymized and contain no names, contact details, timestamps, or roster fields. `user_id` is an opaque study identifier. Name-bearing enrollment and randomization rosters, original randomization inputs, and roster ordering are not included. Solver licences and machine-specific configuration are also excluded.
