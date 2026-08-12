utility_density_coordinates <- function(data) {
  required <- c("baseline_utility", "treatment")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Utility figure input is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  plot_data <- data.frame(
    baseline_utility = as.numeric(data$baseline_utility),
    condition = factor(
      ifelse(as.logical(data$treatment), "Treatment", "Control"),
      levels = c("Control", "Treatment")
    )
  )
  plot_data <- plot_data[stats::complete.cases(plot_data), , drop = FALSE]

  density_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = baseline_utility, fill = condition)
  ) +
    ggplot2::geom_density(n = 512, na.rm = TRUE)

  density_data <- ggplot2::ggplot_build(density_plot)$data[[1L]]
  density_groups <- split(density_data, density_data$group)
  if (length(density_groups) != 2L ||
      any(vapply(density_groups, nrow, integer(1)) != 512L)) {
    stop("Expected two 512-point utility-density series.", call. = FALSE)
  }

  # Retain every eighth density point plus the endpoint.
  keep <- unique(c(seq.int(1L, 512L, by = 8L), 512L))
  lapply(density_groups, function(group) {
    group <- group[order(group$x), , drop = FALSE]
    data.frame(x = group$x[keep], y = group$y[keep])
  })
}

write_utility_condition_tikz <- function(data, path) {
  coordinates <- utility_density_coordinates(data)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  output <- file(path, open = "wb")
  on.exit(close(output), add = TRUE)

  writeLines(c(
    "\\begin{tikzpicture}",
    "\\begin{axis}[",
    "    width=\\columnwidth,",
    "    height={9.5cm},",
    "    axis y line=left,",
    "    axis x line=bottom,",
    "    separate axis lines,",
    "    axis line style={gray!55, line width={0.4pt}, -},",
    "    tick style={gray!55, line width={0.4pt}},",
    "    xmin={1.88}, xmax={3.60},",
    "    ymin={-0.06}, ymax={1.30},",
    "    xtick={2.0,2.5,3.0,3.5},",
    "    ytick={0.0,0.4,0.8,1.2},",
    "    xticklabel={\\pgfmathprintnumber[fixed,fixed zerofill,precision=1]{\\tick}},",
    "    yticklabel={\\pgfmathprintnumber[fixed,fixed zerofill,precision=1]{\\tick}},",
    "    xlabel={Baseline utility},",
    "    ylabel={Density},",
    "    label style={font={\\small}},",
    "    tick label style={font={\\footnotesize}},",
    "    xmajorgrids,",
    "    ymajorgrids,",
    "    grid style={gray!20, line width={0.4pt}},",
    "]"
  ), output, useBytes = TRUE)

  write_density <- function(label, values, colour) {
    writeLines(c(
      sprintf("    %% %s", label),
      "    \\addplot[",
      sprintf("        fill={rgb,255:red,%s},", colour),
      "        fill opacity={0.85},",
      "        draw={gray!60},",
      "        draw opacity={1},",
      "        line width={0.5pt},",
      "    ] coordinates {",
      sprintf("            (%.6f,0)", values$x[1L]),
      sprintf("            (%.6f,%.6f)", values$x, values$y),
      sprintf("            (%.6f,0)", values$x[nrow(values)]),
      "    } \\closedcycle;"
    ), output, useBytes = TRUE)
  }

  write_density("Control", coordinates[[1L]], "100;green,149;blue,237")
  write_density("Treatment", coordinates[[2L]], "213;green,94;blue,0")

  writeLines(c(
    "\\end{axis}",
    "% Manual legend: title and swatches share an exact left edge.",
    "\\coordinate (legend west) at ([xshift={15pt},yshift={25pt}]current axis.east);",
    "\\node[anchor=west, inner sep={0pt}, outer sep={0pt}, font={\\small}]",
    "    at (legend west) {Experimental Condition};",
    "\\node[",
    "    anchor=west, outer sep={0pt},",
    "    minimum width={0.40cm}, minimum height={0.40cm}, inner sep={0pt},",
    "    fill={rgb,255:red,100;green,149;blue,237}, fill opacity={0.85},",
    "    draw={gray!60}, line width={0.5pt}",
    "] (control swatch) at ([yshift={-18pt}]legend west) {};",
    "\\node[anchor=west, font={\\footnotesize}]",
    "    at ([xshift={5pt}]control swatch.east) {Control};",
    "\\node[",
    "    anchor=west, outer sep={0pt},",
    "    minimum width={0.40cm}, minimum height={0.40cm}, inner sep={0pt},",
    "    fill={rgb,255:red,213;green,94;blue,0}, fill opacity={0.85},",
    "    draw={gray!60}, line width={0.5pt}",
    "] (treatment swatch) at ([yshift={-33pt}]legend west) {};",
    "\\node[anchor=west, font={\\footnotesize}]",
    "    at ([xshift={5pt}]treatment swatch.east) {Treatment};",
    "\\end{tikzpicture}"
  ), output, useBytes = TRUE)

  invisible(path)
}

compile_utility_condition_pdf <- function(tikz_path, pdf_path) {
  pdflatex <- Sys.which("pdflatex")
  if (!nzchar(pdflatex)) {
    stop("pdflatex is required to render the utility figure PDF.", call. = FALSE)
  }

  build_dir <- tempfile(pattern = "utility-figure-", tmpdir = tempdir())
  dir.create(build_dir)
  build_dir <- normalizePath(build_dir, mustWork = TRUE)
  temp_root <- normalizePath(tempdir(), mustWork = TRUE)
  if (!startsWith(build_dir, paste0(temp_root, .Platform$file.sep))) {
    stop("Refusing to use a build directory outside the R temporary directory.",
         call. = FALSE)
  }
  on.exit(unlink(build_dir, recursive = TRUE, force = TRUE), add = TRUE)

  file.copy(tikz_path, file.path(build_dir, "utilities_by_condition.tikz"),
            overwrite = TRUE)
  writeLines(c(
    "\\documentclass[border=10pt]{standalone}",
    "\\pdfinfoomitdate=1",
    "\\pdftrailerid{}",
    "\\usepackage{pgfplots}",
    "\\pgfplotsset{compat=1.18}",
    "\\usetikzlibrary{calc}",
    "\\begin{document}",
    "% Match the journal-column context used to render the manuscript reference.",
    "\\setlength{\\columnwidth}{234.875pt}%",
    "\\input{utilities_by_condition.tikz}",
    "\\end{document}"
  ), file.path(build_dir, "utility-figure.tex"), useBytes = TRUE)

  previous_dir <- setwd(build_dir)
  status <- tryCatch(
    system2(pdflatex,
            c("-interaction=nonstopmode", "-halt-on-error", "utility-figure.tex"),
            stdout = FALSE, stderr = FALSE,
            env = c("SOURCE_DATE_EPOCH=946684800", "FORCE_SOURCE_DATE=1", "TZ=UTC")),
    finally = setwd(previous_dir)
  )
  if (!identical(status, 0L)) {
    stop("pdflatex failed while rendering the utility figure.", call. = FALSE)
  }

  dir.create(dirname(pdf_path), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(file.path(build_dir, "utility-figure.pdf"), pdf_path,
                 overwrite = TRUE)) {
    stop("Could not copy the rendered utility figure PDF.", call. = FALSE)
  }
  invisible(pdf_path)
}

render_utility_condition_figure <- function(data, tikz_path, pdf_path) {
  write_utility_condition_tikz(data, tikz_path)
  compile_utility_condition_pdf(tikz_path, pdf_path)
  invisible(c(tikz = tikz_path, pdf = pdf_path))
}
