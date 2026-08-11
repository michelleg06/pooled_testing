# Run the test-allocation experiments, writing results to data/expN-data.csv.
# Analysis (tables and figures) lives separately in analysis.jl, which reads
# these CSVs back.
#
#     julia --project=. experiments.jl                   # experiments 1-4 (default)
#     julia --project=. experiments.jl --experiments 1,3 # a subset
#
# Each experiment overwrites its own data/expN-data.csv.

using CSV, DataFrames, Statistics, Dates, Distributions, ProgressBars, DataStructures, Gurobi, ArgParse

include("optimisation.jl")

const ROOT = @__DIR__

"""
Convenience function to run experiments with specified algorithms, populations, budgets and poolsizes.
"""
function run_experiments(algs, populations, budgets, poolsizes)
    df = DataFrame(:budget=>Int[], :population=>Int[], :poolsize => Int[])  # for storing results
    for alg in algs  # add columns for all algorithms
        df[!, "$(alg.name)_welfare"] = Float64[]
        df[!, "$(alg.name)_error"] = Float64[]
        df[!, "$(alg.name)_time"] = Millisecond[]
    end
    for (i, pop) in ProgressBar(enumerate(populations))
        for T in budgets
            for G in poolsizes
                result = Dict{Symbol, Any}(:budget => T, :population => i, :poolsize => G)
                for (name, fn, args) in algs
                    start = Dates.now()
                    w, pools, error = fn(pop; T=T, G=G, args...)
                    time = Dates.now() - start
                    result[Symbol("$(name)_welfare")] = w
                    result[Symbol("$(name)_error")] = error
                    result[Symbol("$(name)_time")] = time
                end
                push!(df, result)
            end
        end
    end
    return df
end


"""
Compute comparisons between the two algorithms specified in `algs`.
"""
function add_comparisons!(df, algs)
    symb1, symb2 = ["$(alg.name)_welfare" for alg in algs]
    df[!, :diff] = df[!,symb1] - df[!,symb2]
    df[!, :ratio] = df[!,symb1] ./ df[!,symb2]
end


"Extract population data from a CSV containing utility and health probability."
function extract_population(filename, utility_upper_bound)
    df = DataFrame(CSV.File(filename))
    scaling_factor = utility_upper_bound / maximum(df.baseline_utility)
    trial_population = Population{Int}()  # with integral utilities
    for (person_index, row) in enumerate(eachrow(df))
        q = row.health_probability
        u = Int(round(row.baseline_utility*scaling_factor))
        # Public input files contain no participant identifier. Row indices are
        # opaque internal keys and are not used as model covariates or outputs.
        trial_population[person_index] = (q, u)
    end
    return trial_population, scaling_factor .* df.baseline_utility
end


## EXPERIMENT DEFINITIONS

const UTIL_UPPER_BOUND = 50
const PILOT_BUDGETS = [2, 6, 10, 14, 18, 22, 26, 30, 34]

const SYNTHETIC_N = 200
const SYNTHETIC_REPS = 20
const SYNTHETIC_BUDGETS = [2, 4, 6, 8, 10, 12]

"Pilot population from pilotdata.csv, utilities scaled to integers in [1, UTIL_UPPER_BOUND]."
function pilot_population()
    pop, _ = extract_population(joinpath(ROOT, "pilotdata.csv"), UTIL_UPPER_BOUND)
    return pop
end

"`SYNTHETIC_REPS` synthetic populations of size `SYNTHETIC_N`, fitted to the pilot utility distribution."
function synthetic_populations()
    _, baseline_utilities = extract_population(joinpath(ROOT, "pilotdata.csv"), UTIL_UPPER_BOUND)
    utility_distribution = fit(Normal, baseline_utilities)
    health_probs = Uniform(0.5, 1)
    return [generate_instance(SYNTHETIC_N, health_probs, utility_distribution) for _ in 1:SYNTHETIC_REPS]
end

const PILOT_APPROX_VS_GREEDY = [
    (name=:approx, fn=approximate, args=Dict(:K => 25, :verbose => false)),
    (name=:greedy, fn=greedy, args=Dict()),
]

const SYNTHETIC_APPROX_VS_GREEDY = [
    (name=:approx, fn=approximate, args=Dict(:K => 20, :verbose => false)),
    (name=:greedy, fn=greedy, args=Dict()),
]

"""
Run experiment `num` and write its results to data/expN-data.csv.

Experiments 1-4 compare the approximate (MILP) and greedy algorithms:
  1: pilot data,     G=5
  2: pilot data,     G=10
  3: synthetic data, G=5
  4: synthetic data, G=10
"""
function run_experiment(num)
    if num == 1
        df = run_experiments(PILOT_APPROX_VS_GREEDY, [pilot_population()], PILOT_BUDGETS, [5])
        add_comparisons!(df, PILOT_APPROX_VS_GREEDY)
    elseif num == 2
        df = run_experiments(PILOT_APPROX_VS_GREEDY, [pilot_population()], PILOT_BUDGETS, [10])
        add_comparisons!(df, PILOT_APPROX_VS_GREEDY)
    elseif num == 3
        df = run_experiments(SYNTHETIC_APPROX_VS_GREEDY, synthetic_populations(), SYNTHETIC_BUDGETS, [5])
        add_comparisons!(df, SYNTHETIC_APPROX_VS_GREEDY)
    elseif num == 4
        df = run_experiments(SYNTHETIC_APPROX_VS_GREEDY, synthetic_populations(), SYNTHETIC_BUDGETS, [10])
        add_comparisons!(df, SYNTHETIC_APPROX_VS_GREEDY)
    else
        error("Unknown experiment: $(num)")
    end
    CSV.write(joinpath(ROOT, "data", "exp$(num)-data.csv"), df)
    return df
end

"Warm up Julia so experiment timings aren't polluted by first-call compilation."
function warmup()
    println("\nWARMING UP THE ENGINE")
    pop = generate_instance(10, 0:0.1:1, 1:10)
    greedy(pop; T=2, G=5)
    approximate(pop; T=2, G=5, K=15)
    println("READY TO RUMBLE\n")
end


## COMMAND LINE INTERFACE

const EXPERIMENT_NUMBERS = [1, 2, 3, 4]
const DEFAULT_EXPERIMENTS = "1,2,3,4"

function parse_commandline(args)
    s = ArgParseSettings(description="Run the test-allocation experiments.")
    @add_arg_table! s begin
        "--experiments"
            help = "Comma-separated experiments to run. Available: $(join(EXPERIMENT_NUMBERS, ", ")). Default: $(DEFAULT_EXPERIMENTS)."
            arg_type = String
            default = DEFAULT_EXPERIMENTS
    end
    return parse_args(args, s)
end

function main(args)
    parsed = parse_commandline(args)
    warmup()
    for s in split(parsed["experiments"], [',', ' ']; keepempty=false)
        num = tryparse(Int, strip(s))
        if num === nothing || !(num in EXPERIMENT_NUMBERS)
            @warn "Unknown experiment: $(s). Available: $(join(EXPERIMENT_NUMBERS, ", "))"
            continue
        end
        println("\nSTARTING EXPERIMENT $(num)")
        run_experiment(num)
        @info "Completed experiment $(num)"
    end
    @info "All requested experiments done. Analyse with: julia --project=. analysis.jl"
end

if abspath(PROGRAM_FILE) == @__FILE__
    main(ARGS)
end
