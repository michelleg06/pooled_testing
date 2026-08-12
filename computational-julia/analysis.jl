# Analyse data/expN-data.csv files: build summary tables (tables/) and ratio
# figures (figs/). Safe to run on whatever experiments have been run so far.
#
#     julia --project=. analysis.jl                  # all experiments with data on disk
#     julia --project=. analysis.jl --experiments 3   # a subset
#
# Output: tables/expN-summary.{tex,pdf} for every experiment with data on
# disk, plus, for experiments 3 and 4 (the ones with multiple populations per
# budget), two styles of ratio figure:
#   figs/expN-ratiodots.{tikz,pdf}, figs/expN-ratiohist.{tikz,pdf}
#       plain style, with a rendered .pdf for quick viewing
#   figs/expN-ratiodots_paper.tikz, figs/expN-ratiohist_paper.tikz
#       the manuscript style the paper \input{}s: \columnwidth sizing, split
#       grey axis lines, and a dashed parity line
# Tables are written as a .tex `table` environment (with caption/label, for
# \input{} into a paper); figures are written as a raw .tikz tikzpicture (no
# preamble, also for \input{} into a paper). Tables and plain figures also get
# a rendered standalone .pdf alongside them (for quick viewing). Rendering the
# .pdf requires a local LaTeX installation (pdflatex is shelled out to).
#
# The _paper figures assume the including document loads pgfplots with the
# groupplots library (and tikz's calc library, which pgfplots pulls in).

using CSV, DataFrames, Statistics, PrettyTables, ArgParse, Printf, PGFPlotsX, Random

const ROOT = @__DIR__

## EXPERIMENT CONFIGURATION

# `algs`: CSV column-name prefixes being compared (first algorithm is the one
# with a Guarantee column). `names`: how the two algorithms are labelled in
# the table header/caption/note. `budgets`: x-axis values for figures.
# `caption`/`note`: table caption and threeparttable note text.
const ANALYSES = Dict(
    "1" => (algs=["approx", "greedy"], names=["MILP", "Greedy"], budgets=[2, 6, 10, 14, 18, 22, 26, 30, 34],
        caption=raw"Performance of the MILP and \greedy{} on pilot data ($G=5$).",
        note=raw"Summary showing welfare and computation time for the MILP and \greedy{} on the pilot data with a population of $n=130$ and pool size constraint $G=5$, with testing budgets $B \in \{2, 6, \ldots, 34\}$. Welfare figures are deterministic values computed on the pilot population."),
    "2" => (algs=["approx", "greedy"], names=["MILP", "Greedy"], budgets=[2, 6, 10, 14, 18, 22, 26, 30, 34],
        caption=raw"Performance of the MILP and \greedy{} on pilot data ($G=10$).",
        note=raw"Summary showing welfare and computation time for the MILP and \greedy{} on the pilot data with a population of $n=130$ and pool size constraint $G=10$, with testing budgets $B \in \{2, 6, \ldots, 34\}$. Welfare figures are deterministic values computed on the pilot population."),
    "3" => (algs=["approx", "greedy"], names=["MILP", "Greedy"], budgets=[2, 4, 6, 8, 10, 12],
        caption=raw"Synthetic-population performance of the MILP and \greedy{} ($G=5$).",
        note=raw"Summary showing welfare and computation time for the MILP and \greedy{} on synthetic data with populations of size $n=200$ and pool size constraint $G=5$, with testing budgets $B \in \{2, 4, 6, 8, 10, 12\}$. Welfare and times are averaged over 20 randomly generated populations."),
    "4" => (algs=["approx", "greedy"], names=["MILP", "Greedy"], budgets=[2, 4, 6, 8, 10, 12],
        caption=raw"Synthetic-population performance of the MILP and \greedy{} ($G=10$).",
        note=raw"Summary showing welfare and computation time for the MILP and \greedy{} on synthetic data with populations of size $n=200$ and pool size constraint $G=10$, with testing budgets $B \in \{2, 4, 6, 8, 10, 12\}$. Welfare and times are averaged over 20 randomly generated populations."),
)


## FORMATTING HELPERS

fmt2(x) = @sprintf("%.2f", x)
fmt4(x) = @sprintf("%.4f", x)
latex_algorithm_name(name) = name == "Greedy" ? raw"\greedy{}" : name
figure_algorithm_name(name) = name == "Greedy" ? raw"\textsc{Greedy}" : name

function human_time(ms::Real)
    ms < 1_000 && return @sprintf("%.0f ms", ms)
    ms < 60_000 && return @sprintf("%.2f s", ms / 1_000)
    return @sprintf("%.2f min", ms / 60_000)
end

roundmean(x) = sum(x) / length(x)

"Parse a `Millisecond` column as written by CSV.write (e.g. \"348 milliseconds\") back into a numeric ms value."
parse_ms(s::AbstractString) = parse(Float64, first(split(s)))
parse_ms(x::Real) = x


## SUMMARY TABLES

"""
Mean welfare, (human-formatted) mean time, and mean a priori approximation
guarantee (the first algorithm's `_error` column) per budget, for the two
named algorithms. Also computes "Apx To Optimal": the upper bound on the
ratio between optimal non-overlapping welfare and the second algorithm's
welfare, i.e. (first algorithm's welfare + guarantee) / second algorithm's
welfare.
"""
function summary_table(df, algs)
    a, b = algs
    table = combine(groupby(df, :budget),
        Symbol("$(a)_welfare") => roundmean => Symbol("$(a)_welfare"),
        Symbol("$(a)_error") => roundmean => Symbol("$(a)_guarantee"),
        Symbol("$(a)_time") => (t -> roundmean(parse_ms.(t))) => Symbol("$(a)_time_ms"),
        Symbol("$(b)_welfare") => roundmean => Symbol("$(b)_welfare"),
        Symbol("$(b)_time") => (t -> roundmean(parse_ms.(t))) => Symbol("$(b)_time_ms"),
    )
    sort!(table, :budget)
    table.apx_to_optimal = (table[:, Symbol("$(a)_welfare")] .+ table[:, Symbol("$(a)_guarantee")]) ./ table[:, Symbol("$(b)_welfare")]
    return table
end

"""
Compile a `table`-environment `.tex` file (as written by `write_summary`) to a
standalone PDF alongside it. The `standalone` document class can't capture a
`\\begin{table}[tb!]...` float directly (the optional placement argument
confuses its environment-capture macro), so this strips the float wrapper and
caption/label, keeping just the `tabular`, and prints the caption as plain
bold text above it instead. Requires a local LaTeX installation.
"""
function compile_table_pdf(path)
    dir = dirname(path)
    name = first(splitext(basename(path)))
    lines = readlines(path)
    caption_line = findfirst(l -> occursin("\\caption{", l), lines)
    caption = caption_line === nothing ? "" : match(r"\\caption\{(.*)\}", lines[caption_line]).captures[1]
    tabular_start = findfirst(l -> occursin("\\begin{tabular}", l), lines)
    tabular_end = findfirst(l -> occursin("\\end{tabular}", l), lines)
    body = lines[tabular_start:tabular_end]

    wrapper = joinpath(dir, "$(name)-standalone.tex")
    open(wrapper, "w") do io
        println(io, raw"\documentclass[tightpage,border=20pt]{standalone}")
        println(io, raw"\usepackage{booktabs}")
        println(io, raw"\usepackage{varwidth}")
        println(io, raw"\newcommand{\greedy}{\textsc{Greedy}}")
        println(io, raw"\begin{document}")
        println(io, raw"\begin{varwidth}{\maxdimen}")
        println(io, raw"\centering")
        println(io, "\\textbf{$(caption)}\\\\[4pt]")
        for l in body
            println(io, l)
        end
        println(io, raw"\end{varwidth}")
        println(io, raw"\end{document}")
    end
    run(pipeline(`pdflatex -interaction=nonstopmode -output-directory=$(dir) $(wrapper)`; stdout=devnull, stderr=devnull))
    mv(joinpath(dir, "$(name)-standalone.pdf"), joinpath(dir, "$(name).pdf"); force=true)
    for ext in ("tex", "aux", "log")
        rm(joinpath(dir, "$(name)-standalone.$(ext)"); force=true)
    end
end

"""
Write a `threeparttable`-style LaTeX table to `tables/expN-summary.tex`,
matching the paper's table convention: a two-level header grouping each
algorithm's Welfare/Guarantee/Time (first algorithm) or Welfare/Apx To
Optimal/Time (second algorithm) columns under its name, a caption/label, and
a `tablenotes` note — directly `\\input`-able into the paper. Also writes a
rendered `tables/expN-summary.pdf` alongside it, and prints a flat-header
version of the same data to stdout.
"""
function write_summary(num, table, algs, names, caption, note)
    a, b = algs
    na, nb = names
    na_tex, nb_tex = latex_algorithm_name(na), latex_algorithm_name(nb)
    path = joinpath(ROOT, "tables", "exp$(num)-summary.tex")
    open(path, "w") do io
        println(io, raw"\begin{table}[tb!]")
        println(io, raw"    \centering")
        println(io, raw"    \begin{threeparttable}")
        println(io, "    \\caption{$(caption)}")
        println(io, "    \\label{table:experiment$(num)}")
        println(io, raw"    {\small")
        println(io, raw"    \renewcommand{\arraystretch}{1.15}")
        println(io, raw"    \begin{tabular}{@{} crrrrrr @{}}")
        println(io, raw"        \toprule")
        println(io, "        & \\multicolumn{3}{c}{{$(na_tex)}} & \\multicolumn{3}{c}{{$(nb_tex)}} \\\\")
        println(io, raw"        \cmidrule(lr){2-4} \cmidrule(l){5-7}")
        println(io, "        {Budget} & Welfare & Guarantee & Time & Welfare & Apx To Optimal & Time \\\\")
        println(io, raw"        \midrule")
        for row in eachrow(table)
            println(io, "        $(row.budget) & $(fmt2(row[Symbol("$(a)_welfare")])) & " *
                "$(fmt2(row[Symbol("$(a)_guarantee")])) & $(human_time(row[Symbol("$(a)_time_ms")])) & " *
                "$(fmt2(row[Symbol("$(b)_welfare")])) & $(fmt4(row.apx_to_optimal)) & " *
                "$(human_time(row[Symbol("$(b)_time_ms")])) \\\\")
        end
        println(io, raw"        \bottomrule")
        println(io, raw"    \end{tabular}")
        println(io, raw"    }")
        println(io, raw"    \begin{tablenotes}[flushleft]")
        println(io, raw"    \footnotesize")
        println(io, "    \\item[] \\emph{Notes.} $(note) The column ``Guarantee'' reports the a priori additive approximation guarantee of $(na_tex) relative to optimal non-overlapping welfare. The column ``Apx To Optimal'' reports the upper bound on the ratio between optimal non-overlapping welfare and $(nb_tex) welfare, computed as $(na_tex) welfare plus the guarantee, divided by $(nb_tex) welfare.")
        println(io, raw"    \end{tablenotes}")
        println(io, raw"    \end{threeparttable}")
        println(io, raw"\end{table}")
    end
    compile_table_pdf(path)
    column_labels = ["Budget", "$(a) welfare", "$(a) guarantee", "$(a) time", "$(b) welfare", "Apx to optimal", "$(b) time"]
    data = Matrix{Any}(undef, nrow(table), 7)
    for (i, row) in enumerate(eachrow(table))
        data[i, :] = [
            row.budget,
            fmt2(row[Symbol("$(a)_welfare")]),
            fmt2(row[Symbol("$(a)_guarantee")]),
            human_time(row[Symbol("$(a)_time_ms")]),
            fmt2(row[Symbol("$(b)_welfare")]),
            fmt4(row.apx_to_optimal),
            human_time(row[Symbol("$(b)_time_ms")]),
        ]
    end
    println("\nExperiment $(num):")
    pretty_table(data; column_labels=column_labels, alignment=:c)
    return table
end


## FIGURES (PGFPlotsX / pgfplots)

"Save `fig` as both `basepath.tikz` (raw tikzpicture, for \\input{} into a paper) and `basepath.pdf` (rendered)."
function save_figure(basepath, fig)
    pgfsave("$(basepath).tikz", fig; include_preamble=false)
    pgfsave("$(basepath).pdf", fig)
end

"Per-budget dot plot of welfare ratio (as a percentage): one point per population, in place of a KDE violin."
function plot_ratio_dots(df, names, budgets, basepath)
    a, b = names
    a_tex, b_tex = figure_algorithm_name(a), figure_algorithm_name(b)
    spacing = length(budgets) > 1 ? minimum(diff(sort(budgets))) : 1
    jitter_width = 0.3 * spacing
    axis = @pgf Axis(
        {
            xlabel = "Test budget",
            ylabel = "$(a_tex)-to-$(b_tex) welfare ratio (\\%)",
            xtick = budgets,
            width = "10cm", height = "7cm",
            ymajorgrids,
            grid_style = "{gray!25}",
        },
    )
    rng = Random.MersenneTwister(0)
    for budget in budgets
        ratios = 100 .* collect(skipmissing(filter(:budget => ==(budget), df).ratio))
        isempty(ratios) && continue
        xs = budget .+ jitter_width .* (rand(rng, length(ratios)) .- 0.5)
        push!(axis, @pgf Plot({only_marks, mark = "*", mark_size = "1.6pt", mark_options = "{fill=black, draw=none}"}, Coordinates(xs, ratios)))
    end
    save_figure(basepath, axis)
end

## PAPER-STYLE (_paper) FIGURES
#
# The `_paper` variants are the ones the paper actually \input{}s. They differ
# from the plain figures above in styling only (same data): split grey axis
# lines, \columnwidth sizing so the figure fits the surrounding minipage, a
# dashed parity line at 100%, per-panel budget annotations, a shared rotated
# y-label, and an (a)/(b) subfigure label underneath. The subfigure label and
# the shared y-label sit outside the axis/groupplot environment, which
# PGFPlotsX won't emit, so these are written as LaTeX text directly rather
# than built as plot objects.

"Round `x` down/up to a multiple of `step`."
floorstep(x, step) = floor(x / step) * step
ceilstep(x, step) = ceil(x / step) * step

"""
Choose ~`target` evenly spaced tick values covering `[lo, hi]`, snapped to a
1/2/5-times-power-of-ten step so the labels stay short. Returns a string
suitable for a pgfplots `xtick`/`ytick` option.
"""
function nice_ticks(lo, hi; target=3)
    span = hi - lo
    span <= 0 && return @sprintf("%g", lo)
    raw = span / max(target, 1)
    mag = 10.0^floor(log10(raw))
    step = raw / mag <= 1.5 ? mag : raw / mag <= 3.5 ? 2mag : raw / mag <= 7.5 ? 5mag : 10mag
    ticks = collect(floorstep(lo, step):step:ceilstep(hi, step))
    filter!(t -> t >= lo - step / 2 && t <= hi + step / 2, ticks)
    isempty(ticks) && return @sprintf("%g", lo)
    return join((@sprintf("%g", t) for t in ticks), ",")
end

"Shared axis styling used by both `_paper` figures (one option per line, unindented)."
const PAPER_AXIS_STYLE = [
    raw"axis y line=left, axis x line=bottom, separate axis lines,",
    raw"axis line style={gray!55, line width={0.4pt}, -},",
    raw"ymajorgrids, grid style={gray!20, line width={0.4pt}},",
]

"Shared font styling used by both `_paper` figures (one option per line, unindented)."
const PAPER_FONT_STYLE = [
    raw"tick style={gray!55, line width={0.4pt}},",
    raw"tick label style={font={\footnotesize}},",
    raw"label style={font={\small}},",
]

"Print each line of a shared style block at the standard 4-space axis-option indent."
print_style(io, style) = for line in style
    println(io, "    ", line)
end

"""
Paper-style dot plot, written to `basepath.tikz`. Same jittered per-population
points as `plot_ratio_dots`, restyled for the manuscript.
"""
function plot_ratio_dots_paper(df, names, budgets, basepath)
    a, b = names
    a_tex, b_tex = figure_algorithm_name(a), figure_algorithm_name(b)
    all_ratios = 100 .* collect(skipmissing(df.ratio))
    isempty(all_ratios) && return
    lo, hi = extrema(all_ratios)
    spacing = length(budgets) > 1 ? minimum(diff(sort(budgets))) : 1
    jitter_width = 0.3 * spacing
    xlo, xhi = minimum(budgets) - 0.3 * spacing, maximum(budgets) + 0.3 * spacing

    rng = Random.MersenneTwister(0)
    open("$(basepath).tikz", "w") do io
        println(io, raw"% Auto-generated by analysis.jl -- do not edit by hand.")
        println(io, raw"\begin{tikzpicture}")
        println(io, raw"\begin{axis}[")
        println(io, raw"    width=\columnwidth, height={9.5cm},")
        print_style(io, PAPER_AXIS_STYLE)
        println(io, "    xmin={$(xlo)}, xmax={$(xhi)}, xtick={$(join(budgets, ','))},")
        println(io, "    ymin={$(lo)}, ymax={$(hi)},")
        println(io, "    enlarge y limits={0.02}, ytick={$(nice_ticks(lo, hi))},")
        println(io, "    xlabel={Test budget \$B\$},")
        println(io, "    ylabel={$(a_tex)-to-$(b_tex) welfare ratio (\\%)},")
        print_style(io, PAPER_FONT_STYLE)
        println(io, raw"]")
        println(io, raw"    \addplot[")
        println(io, raw"        densely dashed, gray!70, line width={0.5pt},")
        println(io, "        domain={$(xlo):$(xhi)}, samples={2}, forget plot")
        println(io, raw"    ] {100};")
        for budget in budgets
            ratios = 100 .* collect(skipmissing(filter(:budget => ==(budget), df).ratio))
            isempty(ratios) && continue
            xs = budget .+ jitter_width .* (rand(rng, length(ratios)) .- 0.5)
            println(io, raw"    \addplot[only marks, mark={*}, mark size={1.6pt}, mark options={fill={black!55}, draw={black!55}}]")
            println(io, raw"        coordinates {")
            for (x, y) in zip(xs, ratios)
                println(io, "            ($(x),$(y))")
            end
            println(io, raw"        }")
            println(io, raw"        ;")
        end
        println(io, raw"\end{axis}")
        println(io, raw"\end{tikzpicture}")
    end
end

"""
Paper-style stacked histograms, written to `basepath.tikz`. Same per-budget
binning as `plot_ratio_hist`, restyled for the manuscript, with a single
rotated "Populations" label spanning all panels.
"""
function plot_ratio_hist_paper(df, names, budgets, basepath)
    a, b = names
    a_tex, b_tex = figure_algorithm_name(a), figure_algorithm_name(b)
    all_ratios = 100 .* collect(skipmissing(df.ratio))
    isempty(all_ratios) && return
    lo, hi = extrema(all_ratios)
    edges = collect(range(lo, hi; length=21))

    per_budget_counts = map(budgets) do budget
        ratios = 100 .* collect(skipmissing(filter(:budget => ==(budget), df).ratio))
        counts = zeros(Int, length(edges) - 1)
        for r in ratios
            idx = clamp(searchsortedlast(edges, r), 1, length(counts))
            counts[idx] += 1
        end
        counts
    end
    peak = maximum(maximum, per_budget_counts; init=1)
    ymax = max(5 * ceil(Int, peak / 5), 5)
    n = length(budgets)

    open("$(basepath).tikz", "w") do io
        println(io, raw"% Auto-generated by analysis.jl -- do not edit by hand.")
        println(io, raw"% Requires \usepgfplotslibrary{groupplots} in the preamble.")
        println(io, raw"\begin{tikzpicture}")
        println(io, raw"\begin{groupplot}[")
        println(io, "    group style={group size={1 by $(n)}, vertical sep={9pt}, x descriptions at=edge bottom},")
        println(io, raw"    width=\columnwidth, height={2.9cm},")
        print_style(io, PAPER_AXIS_STYLE)
        println(io, "    xmin={$(lo)}, xmax={$(hi)}, enlarge x limits={0.02},")
        println(io, "    xtick={$(nice_ticks(lo, hi))},")
        println(io, "    ymin={0}, ymax={$(ymax)}, ytick={$(nice_ticks(0, ymax))},")
        println(io, "    xlabel={$(a_tex)-to-$(b_tex) welfare ratio (\\%)},")
        print_style(io, PAPER_FONT_STYLE)
        println(io, raw"]")
        for (i, budget) in enumerate(budgets)
            counts = per_budget_counts[i]
            ys = [counts; counts[end]]
            println(io, raw"    \nextgroupplot")
            println(io, "    \\node[anchor=north east, font={\\footnotesize}] at (rel axis cs:0.985,0.92) {\$B = $(budget)\$};")
            println(io, raw"    \addplot[ybar interval, fill={rgb,255:red,100;green,149;blue,237}, draw={gray!60}, line width={0.3pt}]")
            println(io, raw"        table[row sep={\\}]")
            println(io, raw"        {")
            # NB: a trailing "\\" collapses to one backslash even in a raw
            # string, so the pgfplots row separator is escaped explicitly.
            println(io, "            x  y  \\\\")
            for (x, y) in zip(edges, ys)
                println(io, "            $(x)  $(y)  \\\\")
            end
            println(io, raw"        }")
            println(io, raw"        ;")
            println(io, raw"    \draw[densely dashed, gray!70, line width={0.5pt}] ({axis cs:100.0,0}|-{rel axis cs:0,1}) -- ({axis cs:100.0,0}|-{rel axis cs:0,0});")
        end
        println(io, raw"\end{groupplot}")
        println(io, raw"% shared y-axis label spanning all panels (bar height = count of populations)")
        println(io, raw"\node[rotate=90, anchor=south, font={\small}]")
        println(io, "    at ([xshift={-24pt}]\$(group c1r1.north west)!0.5!(group c1r$(n).south west)\$)")
        println(io, raw"    {Populations};")
        println(io, raw"\end{tikzpicture}")
    end
end

"Per-budget histograms of welfare ratio (as a percentage), stacked in one figure."
function plot_ratio_hist(df, names, budgets, basepath)
    a, b = names
    a_tex, b_tex = figure_algorithm_name(a), figure_algorithm_name(b)
    all_ratios = 100 .* collect(skipmissing(df.ratio))
    isempty(all_ratios) && return
    lo, hi = extrema(all_ratios)
    edges = collect(range(lo, hi; length=21))

    per_budget_counts = map(budgets) do budget
        ratios = 100 .* collect(skipmissing(filter(:budget => ==(budget), df).ratio))
        counts = zeros(Int, length(edges) - 1)
        for r in ratios
            idx = clamp(searchsortedlast(edges, r), 1, length(counts))
            counts[idx] += 1
        end
        counts
    end
    ymax = maximum(maximum, per_budget_counts; init=1)

    gp = @pgf GroupPlot(
        {
            group_style = {group_size="1 by $(length(budgets))", vertical_sep="6pt"},
            width = "10cm", height = "3.2cm",
            ymajorgrids,
            grid_style = "{gray!25}",
        },
    )
    for (i, budget) in enumerate(budgets)
        counts = per_budget_counts[i]
        opts = PGFPlotsX.Options(
            "ylabel" => "B=$(budget)", "ytick" => raw"\empty",
            "ymin" => 0, "ymax" => ymax,
            "xmin" => lo, "xmax" => hi,
        )
        if i < length(budgets)
            opts["xtick"] = raw"\empty"
        else
            opts["xlabel"] = "$(a_tex)-to-$(b_tex) welfare ratio (\\%)"
        end
        push!(gp, @pgf Axis(opts,
            Plot({ybar_interval, fill="{rgb,255:red,31;green,119;blue,180}", draw="none"}, Table(["x" => edges, "y" => [counts; counts[end]]])),
            VLine({dashed, "gray!70"}, 100.0)))
    end
    save_figure(basepath, gp)
end


## DRIVING THE ANALYSIS PER EXPERIMENT

function analyse(num)
    datapath = joinpath(ROOT, "data", "exp$(num)-data.csv")
    if !isfile(datapath)
        @warn "No data file for experiment $(num) ($(datapath)); skipping."
        return
    end
    df = DataFrame(CSV.File(datapath))
    spec = ANALYSES[string(num)]
    mkpath(joinpath(ROOT, "tables"))
    mkpath(joinpath(ROOT, "figs"))
    table = summary_table(df, spec.algs)
    write_summary(num, table, spec.algs, spec.names, spec.caption, spec.note)
    if num in (3, 4)
        plot_ratio_dots(df, spec.names, spec.budgets, joinpath(ROOT, "figs", "exp$(num)-ratiodots"))
        plot_ratio_hist(df, spec.names, spec.budgets, joinpath(ROOT, "figs", "exp$(num)-ratiohist"))
        plot_ratio_dots_paper(df, spec.names, spec.budgets, joinpath(ROOT, "figs", "exp$(num)-ratiodots_paper"))
        plot_ratio_hist_paper(df, spec.names, spec.budgets, joinpath(ROOT, "figs", "exp$(num)-ratiohist_paper"))
    end
end


## COMMAND LINE INTERFACE

function available_experiments()
    return sort([parse(Int, k) for k in keys(ANALYSES) if isfile(joinpath(ROOT, "data", "exp$(k)-data.csv"))])
end

function parse_commandline(args)
    s = ArgParseSettings(description="Analyse the test-allocation experiment data.")
    @add_arg_table! s begin
        "--experiments"
            help = "Comma-separated experiments to analyse. Default: all experiments with data on disk."
            arg_type = String
            default = ""
    end
    return parse_args(args, s)
end

function main(args)
    parsed = parse_commandline(args)
    nums = isempty(strip(parsed["experiments"])) ?
        available_experiments() :
        [parse(Int, strip(s)) for s in split(parsed["experiments"], [',', ' ']; keepempty=false)]
    for num in nums
        if !haskey(ANALYSES, string(num))
            @warn "Unknown experiment: $(num)"
            continue
        end
        analyse(num)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main(ARGS)
end
