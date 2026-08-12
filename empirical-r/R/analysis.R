# ===========================================================================================
# WELFARE MAXIMIZING POOLED TESTING
# ===========================================================================================
# Script: R/analysis.R
# Project: Empirical analysis for the C-SEF randomized trial
# Purpose: Prepare the trial data and reproduce the empirical results.
# Inputs: data/baseline.csv and data/endline.csv.
# Outputs: Tables in output/tables/ and the utility figure in output/figures/.
#
# Reproducibility notes:
#   - Run from the project root with Rscript run_analysis.R.
# ===========================================================================================

# ---- Data -----------------------------------------------------------------------

baseline <- data.table::fread(data_file("baseline.csv"), check.names = FALSE)
endline <- data.table::fread(data_file("endline.csv"), check.names = FALSE)

# ---- Outcomes and covariates ----------------------------------------------------

reverse_1_to_5 <- function(x) 6 - as.numeric(x)

stress_score <- function(stress1, stress2, stress3, stress4) {
  score <- rowMeans(
    cbind(stress1, reverse_1_to_5(stress2), reverse_1_to_5(stress3), stress4),
    na.rm = TRUE
  )
  score[!is.finite(score)] <- NA_real_
  score
}

baseline$stress_aggregate <- stress_score(
  baseline$stress1, baseline$stress2, baseline$stress3, baseline$stress4
)
endline$stress_aggregate <- stress_score(
  endline$stress1, endline$stress2, endline$stress3, endline$stress4
)

baseline$gender <- stats::relevel(
  factor(baseline$gender, levels = c(1, 2, 4),
         labels = c("Female", "Male", "Prefer not to say")),
  ref = "Male"
)
baseline$illness <- stats::relevel(
  factor(baseline$illness, levels = c(1, 2, 3),
         labels = c("Yes", "No", "Prefer not to say")),
  ref = "Prefer not to say"
)
baseline$academics <- factor(
  ifelse(baseline$role == 4, "Staff", "Academics"),
  levels = c("Staff", "Academics")
)

# ---- Table formatting -----------------------------------------------------------

write_booktabs <- function(data, out, caption, label, digits = 3, align = NULL, ...) {
  table <- kableExtra::kbl(
    data,
    format = "latex",
    booktabs = TRUE,
    caption = caption,
    label = sub("^tab:", "", label),
    digits = digits,
    align = align,
    linesep = "",
    row.names = FALSE,
    ...
  )
  writeLines(as.character(table), out)
  invisible(out)
}

covariate_label_map <- c(
  "treatmentTRUE" = "Treat vs. control",
  "baseline_outcome_time0" = "Baseline outcome",
  "genderFemale" = "Female",
  "genderPrefer not to say" = "Prefer not to say",
  "age" = "Age",
  "academicsAcademics" = "Academic vs. staff",
  "baseline_utility" = "Utility",
  "sociability_time0" = "Sociability",
  "fear_time0" = "Fear",
  "institute_satisfaction_time0" = "Institute satisfaction",
  "life_satisfaction_time0" = "Life satisfaction",
  "stress_aggregate_time0" = "Stress score",
  "illnessYes" = "C19 recovered",
  "illnessNo" = "Not recovered",
  "household_size" = "Household size",
  "digital_media_overall" = "Digital media score",
  "socioeconomic_overall" = "Socio-economic score",
  "(Intercept)" = "Constant"
)

gof_map_basic <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^{2}$", "fmt" = 3),
  list("raw" = "adj.r.squared", "clean" = "Adjusted $R^{2}$", "fmt" = 3)
)

write_reg_table <- function(models, vcov_list = NULL, out, caption, label,
                            notes = NULL,
                            stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
                            exponentiate = FALSE) {
  arguments <- list(
    models,
    output = "kableExtra",
    format = "latex",
    title = paste0(caption, "\\label{", label, "}"),
    fmt = function(value) sprintf("%.3f", ifelse(abs(value) < 0.0005, 0, value)),
    stars = stars,
    exponentiate = exponentiate,
    coef_map = covariate_label_map,
    gof_map = gof_map_basic,
    gof_omit = "AIC|BIC|Log\\.Lik\\.|F|RMSE",
    notes = notes,
    booktabs = TRUE,
    escape = FALSE
  )
  if (!is.null(vcov_list)) arguments$vcov <- vcov_list
  table <- do.call(modelsummary::modelsummary, arguments)
  writeLines(as.character(table), out)
  invisible(out)
}

# ---- Analysis samples -----------------------------------------------------------

robustness_baseline <- baseline[baseline$cluster_robustness_sample, ]
robustness_endline <- endline[endline$cluster_robustness_sample, ]

build_analysis_sample <- function(baseline_data, endline_data) {
  baseline_time0 <- data.frame(
    user_id = baseline_data$user_id,
    group_id = baseline_data$group_id,
    treatment = baseline_data$treatment,
    randomization_unit = baseline_data$randomization_unit,
    baseline_utility = baseline_data$baseline_utility,
    gender = baseline_data$gender,
    age = baseline_data$age,
    academics = baseline_data$academics,
    household_size = baseline_data$household_size,
    socioeconomic_overall = baseline_data$socioeconomic_overall,
    sociability_time0 = baseline_data$sociability,
    institute_satisfaction_time0 = baseline_data$institute_satisfaction,
    fear_time0 = baseline_data$fear,
    stress_aggregate_time0 = baseline_data$stress_aggregate,
    life_satisfaction_time0 = baseline_data$life_satisfaction,
    illness = baseline_data$illness,
    digital_media_overall = baseline_data$digital_media_overall,
    performance_time0 = baseline_data$performance,
    own_goals_time0 = baseline_data$own_goals,
    supervisor_goals_time0 = baseline_data$supervisor_goals,
    productivity_time0 = baseline_data$productivity,
    learning_time0 = baseline_data$learning,
    check.names = FALSE
  )
  endline_outcomes <- data.frame(
    user_id = endline_data$user_id,
    group_id = endline_data$group_id,
    sociability = endline_data$sociability,
    stress_aggregate = endline_data$stress_aggregate,
    life_satisfaction = endline_data$life_satisfaction,
    performance = endline_data$performance,
    own_goals = endline_data$own_goals,
    supervisor_goals = endline_data$supervisor_goals,
    productivity = endline_data$productivity,
    learning = endline_data$learning,
    check.names = FALSE
  )
  analysis <- dplyr::inner_join(
    baseline_time0, endline_outcomes, by = c("user_id", "group_id")
  )
  analysis$delta_stress <- analysis$stress_aggregate - analysis$stress_aggregate_time0
  analysis$delta_lifesat <- analysis$life_satisfaction - analysis$life_satisfaction_time0
  analysis$delta_performance <- analysis$performance - analysis$performance_time0
  analysis$delta_productivity <- analysis$productivity - analysis$productivity_time0
  analysis$delta_learning <- analysis$learning - analysis$learning_time0
  analysis
}

analysis_sample <- build_analysis_sample(baseline, endline)
robustness_sample <- build_analysis_sample(
  robustness_baseline, robustness_endline
)

message(sprintf(
  paste(
    "Analysis samples: full baseline=%d, full endline=%d;",
    "cluster robustness sample: baseline=%d, endline=%d."
  ),
  nrow(baseline), nrow(endline),
  nrow(robustness_baseline), nrow(robustness_endline)
))

# ---- Trial flow and randomization ----------------------------------------------

retention_data <- data.frame(
  treatment = baseline$treatment,
  observed = as.integer(baseline$user_id %in% endline$user_id)
)
retention_model <- stats::glm(
  observed ~ treatment,
  data = retention_data,
  family = stats::binomial()
)
retention_p <- summary(retention_model)$coefficients["treatmentTRUE", "Pr(>|z|)"]

control_n <- sum(!retention_data$treatment)
treatment_n <- sum(retention_data$treatment)
control_attrition <- sum(!retention_data$treatment & retention_data$observed == 0)
treatment_attrition <- sum(retention_data$treatment & retention_data$observed == 0)
# Allocation data require both fields used to construct the baseline utility measure.
allocation_complete <- sum(
  !is.na(baseline$baseline_utility) & !is.na(baseline$health_probability)
)

message(sprintf(
  paste(
    "Sample flow: %d at baseline, %d allocation-complete, %d at endline,",
    "%d attriters (%.1f%%)."
  ),
  nrow(baseline), allocation_complete, nrow(endline), nrow(baseline) - nrow(endline),
  100 * (nrow(baseline) - nrow(endline)) / nrow(baseline)
))
message(sprintf(
  "Attrition: control %d/%d; treatment %d/%d; binomial-model p=%.3f.",
  control_attrition, control_n, treatment_attrition, treatment_n, retention_p
))

randomization_counts <- table(baseline$randomization_stage)
message(
  "Randomization stages: ",
  paste(names(randomization_counts), as.integer(randomization_counts), collapse = "; "),
  "."
)

# ---- Measurement checks --------------------------------------------------------

cronbach_alpha <- function(items) {
  complete_items <- items[stats::complete.cases(items), , drop = FALSE]
  item_count <- ncol(complete_items)
  alpha <- item_count / (item_count - 1) *
    (1 - sum(apply(complete_items, 2, stats::var)) /
       stats::var(rowSums(complete_items)))
  c(alpha = alpha, n = nrow(complete_items))
}

baseline_stress_items <- cbind(
  baseline$stress1, reverse_1_to_5(baseline$stress2),
  reverse_1_to_5(baseline$stress3), baseline$stress4
)
endline_stress_items <- cbind(
  endline$stress1, reverse_1_to_5(endline$stress2),
  reverse_1_to_5(endline$stress3), endline$stress4
)
baseline_items_answered <- rowSums(!is.na(baseline_stress_items))
endline_items_answered <- rowSums(!is.na(endline_stress_items))
baseline_reliability <- cronbach_alpha(baseline_stress_items)
endline_reliability <- cronbach_alpha(endline_stress_items)

message(sprintf(
  paste(
    "Stress scoring: available-item mean; partial responses baseline",
    "(one item=%d, three items=%d), endline (three items=%d)."
  ),
  sum(baseline_items_answered == 1),
  sum(baseline_items_answered == 3),
  sum(endline_items_answered == 3)
))
message(sprintf(
  "Stress reliability: baseline alpha=%.3f (n=%d); endline alpha=%.3f (n=%d).",
  baseline_reliability["alpha"], as.integer(baseline_reliability["n"]),
  endline_reliability["alpha"], as.integer(endline_reliability["n"])
))

# ---- Design benchmark ----------------------------------------------------------

mde_d <- stats::power.t.test(
  n = 60,
  sd = 1,
  sig.level = 0.05,
  power = 0.80,
  type = "two.sample",
  alternative = "two.sided"
)$delta
message(sprintf(
  "MDE benchmark: d=%.3f (80%% power, two-sided alpha=0.05, 60 per arm).",
  mde_d
))

# ---- Descriptive outcome checks ------------------------------------------------

stress_treated <- endline$stress_aggregate[endline$treatment]
stress_control <- endline$stress_aggregate[!endline$treatment]
stress_treated <- stress_treated[!is.na(stress_treated)]
stress_control <- stress_control[!is.na(stress_control)]
stress_pooled_sd <- sqrt(
  ((length(stress_treated) - 1) * stats::var(stress_treated) +
   (length(stress_control) - 1) * stats::var(stress_control)) /
    (length(stress_treated) + length(stress_control) - 2)
)
observed_stress_d <-
  (mean(stress_treated) - mean(stress_control)) / stress_pooled_sd
message(sprintf("Observed endline stress difference: d=%.3f.", observed_stress_d))

sociability_complete <- stats::complete.cases(
  analysis_sample[, c("sociability_time0", "sociability")]
)
sociability_control <- analysis_sample[
  sociability_complete & !analysis_sample$treatment,
]
sociability_treated <- analysis_sample[
  sociability_complete & analysis_sample$treatment,
]
message(sprintf(
  paste(
    "Sociability (paired): control %.3f to %.3f (n=%d);",
    "treatment %.3f to %.3f (n=%d)."
  ),
  mean(sociability_control$sociability_time0), mean(sociability_control$sociability),
  nrow(sociability_control), mean(sociability_treated$sociability_time0),
  mean(sociability_treated$sociability), nrow(sociability_treated)
))
sociability_control_change <-
  sociability_control$sociability - sociability_control$sociability_time0
sociability_treated_change <-
  sociability_treated$sociability - sociability_treated$sociability_time0
message(sprintf(
  paste(
    "Sociability change (decrease/unchanged/increase): control %d/%d/%d;",
    "treatment %d/%d/%d; unchanged or increased %d/%d."
  ),
  sum(sociability_control_change < 0), sum(sociability_control_change == 0),
  sum(sociability_control_change > 0), sum(sociability_treated_change < 0),
  sum(sociability_treated_change == 0), sum(sociability_treated_change > 0),
  sum(sociability_control_change >= 0), sum(sociability_treated_change >= 0)
))

baseline_productivity <- tapply(baseline$productivity, baseline$treatment, mean, na.rm = TRUE)
endline_productivity <- tapply(endline$productivity, endline$treatment, mean, na.rm = TRUE)
message(sprintf(
  paste(
    "Productivity means (control/treatment): baseline %.3f/%.3f;",
    "endline %.3f/%.3f."
  ),
  baseline_productivity["FALSE"], baseline_productivity["TRUE"],
  endline_productivity["FALSE"], endline_productivity["TRUE"]
))

# ---- Descriptive statistics and balance ---------------------------------------

source(file.path(project_path, "R", "utility_figure.R"), local = TRUE)

render_utility_condition_figure(
  baseline,
  tikz_path = figure_file("utilities_by_condition.tikz"),
  pdf_path = figure_file("utilities_by_condition.pdf")
)

summary_stats <- function(data, variables) {
  data.frame(
    Variable = variables,
    N = unname(vapply(
      variables, function(variable) sum(!is.na(data[[variable]])), integer(1)
    )),
    Mean = unname(vapply(
      variables,
      function(variable) mean(as.numeric(data[[variable]]), na.rm = TRUE),
      numeric(1)
    )),
    SD = unname(vapply(
      variables,
      function(variable) stats::sd(as.numeric(data[[variable]]), na.rm = TRUE),
      numeric(1)
    )),
    Min = unname(vapply(
      variables,
      function(variable) min(as.numeric(data[[variable]]), na.rm = TRUE),
      numeric(1)
    )),
    P25 = unname(vapply(
      variables,
      function(variable) stats::quantile(
        as.numeric(data[[variable]]), 0.25, na.rm = TRUE, names = FALSE
      ),
      numeric(1)
    )),
    P75 = unname(vapply(
      variables,
      function(variable) stats::quantile(
        as.numeric(data[[variable]]), 0.75, na.rm = TRUE, names = FALSE
      ),
      numeric(1)
    )),
    Max = unname(vapply(
      variables,
      function(variable) max(as.numeric(data[[variable]]), na.rm = TRUE),
      numeric(1)
    )),
    check.names = FALSE,
    row.names = NULL
  )
}

baseline_summary_vars <- c(
  "treatment", "baseline_utility", "health_probability", "age", "dependants",
  "household_size", "socioeconomic_class", "socioeconomic_overall",
  "digital_media_overall", "sociability", "fear", "stress_aggregate",
  "life_satisfaction", "institute_satisfaction", "psychosocial_overall",
  "learning", "productivity", "performance"
)
endline_summary_vars <- c(
  "treatment", "baseline_utility", "health_probability", "sociability", "fear",
  "life_satisfaction", "institute_satisfaction", "learning", "productivity",
  "stress_aggregate"
)

baseline_stats <- summary_stats(baseline, baseline_summary_vars)
endline_stats <- summary_stats(endline, endline_summary_vars)
combined_stats <- rbind(baseline_stats, endline_stats)
combined_table <- kableExtra::kbl(
  combined_stats,
  format = "latex",
  booktabs = TRUE,
  caption = "Summary statistics at baseline and endline.",
  label = "summarystatsbaseline",
  digits = 3,
  linesep = ""
)
combined_table <- kableExtra::pack_rows(
  combined_table, "Baseline", 1, nrow(baseline_stats)
)
combined_table <- kableExtra::pack_rows(
  combined_table, "Endline", nrow(baseline_stats) + 1,
  nrow(baseline_stats) + nrow(endline_stats)
)
writeLines(as.character(combined_table), table_file("summary_stats.tex"))

treatment_mean_diff <- function(data, variable) {
  means <- tapply(data[[variable]], data$treatment, mean, na.rm = TRUE)
  unname(means["TRUE"] - means["FALSE"])
}

balance_row <- function(name, difference, method, p_value) {
  data.frame(
    Covariate = name,
    `Difference (mean)` = if (is.na(difference)) "N/A" else sprintf("%.4f", difference),
    Method = method,
    `p-value` = sprintf("%.4f", p_value),
    Status = if (p_value < 0.05) "unbalanced" else "balanced",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

balance_table <- do.call(rbind, list(
  balance_row("Utility", treatment_mean_diff(baseline, "baseline_utility"),
              "Welch t-test", stats::t.test(baseline_utility ~ treatment, data = baseline)$p.value),
  balance_row("Gender", NA, "Fisher exact",
              stats::fisher.test(table(baseline$gender, baseline$treatment))$p.value),
  balance_row("Age", treatment_mean_diff(baseline, "age"),
              "Welch t-test", stats::t.test(age ~ treatment, data = baseline)$p.value),
  balance_row("Role (Staff vs. Academics)", NA, "Fisher exact",
              stats::fisher.test(table(baseline$academics, baseline$treatment))$p.value),
  balance_row("C19 recovered", NA, "Fisher exact",
              stats::fisher.test(table(baseline$illness, baseline$treatment))$p.value),
  balance_row("Household size", treatment_mean_diff(baseline, "household_size"),
              "Welch t-test", stats::t.test(household_size ~ treatment, data = baseline)$p.value),
  balance_row("Socioeconomic status", treatment_mean_diff(baseline, "socioeconomic_overall"),
              "Welch t-test", stats::t.test(socioeconomic_overall ~ treatment, data = baseline)$p.value),
  balance_row("Sociability", treatment_mean_diff(baseline, "sociability"),
              "Welch t-test", stats::t.test(sociability ~ treatment, data = baseline)$p.value),
  balance_row("Stress (baseline)", treatment_mean_diff(baseline, "stress_aggregate"),
              "Welch t-test", stats::t.test(stress_aggregate ~ treatment, data = baseline)$p.value),
  balance_row("Learning", treatment_mean_diff(baseline, "learning"),
              "Welch t-test", stats::t.test(learning ~ treatment, data = baseline)$p.value),
  balance_row("Life satisfaction", treatment_mean_diff(baseline, "life_satisfaction"),
              "Welch t-test", stats::t.test(life_satisfaction ~ treatment, data = baseline)$p.value),
  balance_row("Productivity", treatment_mean_diff(baseline, "productivity"),
              "Welch t-test", stats::t.test(productivity ~ treatment, data = baseline)$p.value),
  balance_row("Institutional satisfaction", treatment_mean_diff(baseline, "institute_satisfaction"),
              "Welch t-test", stats::t.test(institute_satisfaction ~ treatment, data = baseline)$p.value),
  balance_row("Digital resources score", treatment_mean_diff(baseline, "digital_media_overall"),
              "Welch t-test", stats::t.test(digital_media_overall ~ treatment, data = baseline)$p.value),
  balance_row("Own goals (achieving)", treatment_mean_diff(baseline, "own_goals"),
              "Welch t-test", stats::t.test(own_goals ~ treatment, data = baseline)$p.value),
  balance_row("Supervisor goals (achieving)", treatment_mean_diff(baseline, "supervisor_goals"),
              "Welch t-test", stats::t.test(supervisor_goals ~ treatment, data = baseline)$p.value)
))

write_booktabs(
  balance_table,
  table_file("cov_balance.tex"),
  caption = paste(
    "Baseline covariate balance in the randomized sample ($N=131$).",
    "Continuous covariates use Welch's two-sample t-test;",
    "categorical covariates use Fisher's exact test.",
    "Status `unbalanced' indicates $p < 0.05$."
  ),
  label = "tab:cov-balance",
  digits = 4,
  align = "lcclc"
)

# ---- Primary treatment effects -------------------------------------------------

ancova_covariates <- c(
  "gender", "age", "academics", "baseline_utility", "sociability_time0",
  "fear_time0", "institute_satisfaction_time0", "life_satisfaction_time0",
  "stress_aggregate_time0", "illness", "household_size",
  "digital_media_overall", "socioeconomic_overall"
)
change_score_covariates <- c(
  "baseline_utility", "sociability_time0", "fear_time0",
  "institute_satisfaction_time0", "life_satisfaction_time0",
  "stress_aggregate_time0"
)

fit_adjusted_ols <- function(outcome, covariates, data) {
  stats::lm(
    stats::reformulate(c("treatment", covariates), response = outcome),
    data = data
  )
}

fit_work_ancova <- function(outcome, baseline_outcome, data) {
  model_data <- data
  model_data$baseline_outcome_time0 <- model_data[[baseline_outcome]]
  fit_adjusted_ols(
    outcome,
    c("baseline_outcome_time0", ancova_covariates),
    model_data
  )
}

fit_change_score <- function(outcome, data, omit = NULL) {
  fit_adjusted_ols(outcome, setdiff(change_score_covariates, omit), data)
}

stress_ancova <- fit_adjusted_ols(
  "stress_aggregate", ancova_covariates, analysis_sample
)
life_satisfaction_ancova <- fit_adjusted_ols(
  "life_satisfaction", ancova_covariates, analysis_sample
)
stress_change <- fit_change_score(
  "delta_stress", analysis_sample, omit = "stress_aggregate_time0"
)
life_satisfaction_change <- fit_change_score(
  "delta_lifesat", analysis_sample, omit = "life_satisfaction_time0"
)
performance_ancova <- fit_work_ancova(
  "performance", "performance_time0", analysis_sample
)
productivity_ancova <- fit_work_ancova(
  "productivity", "productivity_time0", analysis_sample
)
learning_ancova <- fit_work_ancova(
  "learning", "learning_time0", analysis_sample
)
performance_change <- fit_change_score("delta_performance", analysis_sample)
productivity_change <- fit_change_score("delta_productivity", analysis_sample)
learning_change <- fit_change_score("delta_learning", analysis_sample)

strict_stars <- c("*" = 0.05, "**" = 0.01, "***" = 0.001)

# A singleton gender category gives one ANCOVA row leverage 1. Suppress only
# sandwich's corresponding HC1 warning; all other warnings remain visible.
hc1_vcov <- function(model) {
  withCallingHandlers(
    sandwich::vcovHC(model, type = "HC1"),
    warning = function(warning) {
      if (startsWith(
        conditionMessage(warning),
        "HC1 covariances become (close to) singular if hat values"
      )) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

hc1_vcov_list <- function(models) {
  lapply(models, hc1_vcov)
}
primary_notes <- c(
  "Heteroskedasticity-robust (HC1) standard errors in parentheses.",
  paste(
    "Models use the full endline analysis sample; model-specific observations are",
    "reported in the table. Level columns are ANCOVA; delta columns regress the",
    "change in outcome on treatment plus baseline covariates."
  ),
  paste(
    "The multiple-testing table reports exact treatment-effect estimates, robust",
    "standard errors, raw $p$-values, and multiplicity-adjusted $p$-values."
  )
)

mental_health_models <- list(
  "Stress" = stress_ancova,
  "$\\Delta$ Stress" = stress_change,
  "Life satisfaction" = life_satisfaction_ancova,
  "$\\Delta$ Life satisfaction" = life_satisfaction_change
)
write_reg_table(
  mental_health_models,
  hc1_vcov_list(mental_health_models),
  out = table_file("results_mentalhealth.tex"),
  caption = paste(
    "Treatment effect on the mental-health outcomes (stress and life satisfaction).",
    "For each outcome we report the covariate-adjusted ANCOVA level model and the",
    "covariate-adjusted change-score (delta) model side by side. The ANCOVA model",
    "regresses the endline outcome on the treatment indicator and 13 pre-treatment",
    "covariates, including its baseline value. The delta model regresses",
    "$\\Delta Y = Y_{\\text{endline}} - Y_{\\text{baseline}}$ on the treatment",
    "indicator and five baseline covariates."
  ),
  label = "tab:results-mentalhealth",
  stars = strict_stars,
  notes = primary_notes
)

work_outcome_models <- list(
  "Performance" = performance_ancova,
  "$\\Delta$ Performance" = performance_change,
  "Productivity" = productivity_ancova,
  "$\\Delta$ Productivity" = productivity_change,
  "Learning" = learning_ancova,
  "$\\Delta$ Learning" = learning_change
)
write_reg_table(
  work_outcome_models,
  hc1_vcov_list(work_outcome_models),
  out = table_file("results_productivity.tex"),
  caption = paste(
    "Treatment effect on the work and study outcomes (performance, productivity,",
    "and learning). For each outcome we report the covariate-adjusted ANCOVA",
    "level model and the covariate-adjusted change-score (delta) model side by side.",
    "ANCOVA columns include the baseline value of the outcome plus 13 pre-treatment",
    "covariates; delta columns include six baseline covariates."
  ),
  label = "tab:results-productivity",
  stars = strict_stars,
  notes = primary_notes
)

# ---- Multiple testing -----------------------------------------------------------

primary_level_models <- list(
  "Stress" = stress_ancova,
  "Life satisfaction" = life_satisfaction_ancova,
  "Performance" = performance_ancova,
  "Productivity" = productivity_ancova,
  "Learning" = learning_ancova
)
primary_change_models <- list(
  "Delta Stress" = stress_change,
  "Delta Life satisfaction" = life_satisfaction_change,
  "Delta Performance" = performance_change,
  "Delta Productivity" = productivity_change,
  "Delta Learning" = learning_change
)

hc1_treatment <- function(model) {
  test <- lmtest::coeftest(model, vcov. = hc1_vcov(model))
  c(
    estimate = unname(stats::coef(model)["treatmentTRUE"]),
    se = unname(test["treatmentTRUE", "Std. Error"]),
    p = unname(test["treatmentTRUE", "Pr(>|t|)"])
  )
}

level_tests <- t(vapply(primary_level_models, hc1_treatment, numeric(3)))
delta_tests <- t(vapply(primary_change_models, hc1_treatment, numeric(3)))
multiple_testing_table <- data.frame(
  outcome = c(names(primary_level_models), names(primary_change_models)),
  family = c(rep("level", 5), rep("delta", 5)),
  estimate = c(level_tests[, "estimate"], delta_tests[, "estimate"]),
  se = c(level_tests[, "se"], delta_tests[, "se"]),
  raw_p = c(level_tests[, "p"], delta_tests[, "p"]),
  stringsAsFactors = FALSE
)
multiple_testing_table$holm_all <- stats::p.adjust(
  multiple_testing_table$raw_p, method = "holm"
)
multiple_testing_table$bh_all <- stats::p.adjust(
  multiple_testing_table$raw_p, method = "BH"
)
multiple_testing_table$holm_within_family <- ave(
  multiple_testing_table$raw_p, multiple_testing_table$family,
  FUN = function(p_value) stats::p.adjust(p_value, method = "holm")
)
multiple_testing_table$bh_within_family <- ave(
  multiple_testing_table$raw_p, multiple_testing_table$family,
  FUN = function(p_value) stats::p.adjust(p_value, method = "BH")
)
names(multiple_testing_table) <- c(
  "Outcome", "Specification", "Estimate", "HC1 SE", "Raw p-value",
  "Holm (all tests)", "BH (all tests)", "Holm (within family)",
  "BH (within family)"
)
write_booktabs(
  multiple_testing_table,
  out = table_file("multiple_testing_correction.tex"),
  caption = paste(
    "Treatment-effect summary for the five primary outcomes under the covariate-adjusted",
    "specifications (ANCOVA for levels; delta + baseline covariates for changes):",
    "HC1 treatment-effect estimate, robust standard error, raw $p$-value, and",
    "Holm and Benjamini-Hochberg adjusted $p$-values across all ten tests and",
    "within the level-only and delta-only families of five."
  ),
  label = "tab:mt-correction"
)

# ---- Equivalence tests ----------------------------------------------------------

tost_outcomes <- list(
  stress = list(variable = "stress_aggregate", label = "Stress"),
  life_satisfaction = list(variable = "life_satisfaction", label = "Life satisfaction"),
  performance = list(variable = "performance", label = "Performance"),
  productivity = list(variable = "productivity", label = "Productivity"),
  learning = list(variable = "learning", label = "Learning")
)
sesoi_d_headline <- 0.5
sesoi_d_grid <- c(0.3, 0.4, 0.5, 0.6)

tost_inputs <- function(data, variable) {
  treated <- data[[variable]][data$treatment]
  control <- data[[variable]][!data$treatment]
  treated <- treated[!is.na(treated)]
  control <- control[!is.na(control)]
  n_treated <- length(treated)
  n_control <- length(control)
  sd_treated <- stats::sd(treated)
  sd_control <- stats::sd(control)
  pooled_sd <- sqrt(
    ((n_treated - 1) * sd_treated^2 + (n_control - 1) * sd_control^2) /
      (n_treated + n_control - 2)
  )
  list(
    mean_treated = mean(treated), mean_control = mean(control),
    sd_treated = sd_treated, sd_control = sd_control,
    n_treated = n_treated, n_control = n_control, pooled_sd = pooled_sd
  )
}

tost_equivalence <- function(input, margin) {
  result <- TOSTER::tsum_TOST(
    m1 = input$mean_treated,
    m2 = input$mean_control,
    sd1 = input$sd_treated,
    sd2 = input$sd_control,
    n1 = input$n_treated,
    n2 = input$n_control,
    low_eqbound = -margin,
    high_eqbound = margin,
    alpha = 0.05,
    var.equal = TRUE
  )
  list(
    tost_p = max(result$TOST$p.value[2:3]),
    nhst_p = result$TOST$p.value[1]
  )
}

tost_input_table <- data.frame(
  outcome = character(), mean_treat = numeric(), sd_treat = numeric(),
  n_treat = integer(), mean_ctrl = numeric(), sd_ctrl = numeric(),
  n_ctrl = integer(), sd_pooled = numeric(), stringsAsFactors = FALSE
)
tost_result_table <- data.frame(
  outcome = character(), sesoi_d = numeric(), low_bound = numeric(),
  high_bound = numeric(), tost_p = numeric(), nhst_p = numeric(),
  conclusion = character(), stringsAsFactors = FALSE
)
tost_sensitivity <- data.frame(
  outcome = character(), p_d03 = numeric(), p_d04 = numeric(),
  p_d05 = numeric(), p_d06 = numeric(), stringsAsFactors = FALSE
)

for (name in names(tost_outcomes)) {
  specification <- tost_outcomes[[name]]
  input <- tost_inputs(analysis_sample, specification$variable)
  tost_input_table[nrow(tost_input_table) + 1, ] <- list(
    specification$label, input$mean_treated, input$sd_treated, input$n_treated,
    input$mean_control, input$sd_control, input$n_control, input$pooled_sd
  )
  headline_margin <- sesoi_d_headline * input$pooled_sd
  headline <- tost_equivalence(input, headline_margin)
  tost_result_table[nrow(tost_result_table) + 1, ] <- list(
    specification$label, sesoi_d_headline, -headline_margin, headline_margin,
    headline$tost_p, headline$nhst_p,
    if (headline$tost_p < 0.05) "equivalence established" else "not established"
  )
  sensitivity <- vapply(
    sesoi_d_grid,
    function(d) tost_equivalence(input, d * input$pooled_sd)$tost_p,
    numeric(1)
  )
  tost_sensitivity[nrow(tost_sensitivity) + 1, ] <- list(
    specification$label,
    sensitivity[1], sensitivity[2], sensitivity[3], sensitivity[4]
  )
}

names(tost_input_table) <- c(
  "Outcome", "Treatment mean", "Treatment SD", "Treatment N",
  "Control mean", "Control SD", "Control N", "Pooled SD"
)
names(tost_result_table) <- c(
  "Outcome", "SESOI (d)", "Lower bound", "Upper bound",
  "TOST p-value", "NHST p-value", "Conclusion"
)
names(tost_sensitivity) <- c(
  "Outcome", "d = 0.3", "d = 0.4", "d = 0.5", "d = 0.6"
)

write_booktabs(
  tost_input_table,
  out = table_file("tab_TOST_input.tex"),
  caption = "TOST equivalence test: input means, standard deviations, and pooled SD by treatment arm.",
  label = "tab:TOST-input"
)
write_booktabs(
  tost_result_table,
  out = table_file("tab_power_analysisTOST.tex"),
  caption = paste(
    "TOST equivalence tests for the five headline outcomes (full sample),",
    "using the preregistration-anchored benchmark Cohen's $d = 0.5$",
    "(margin $=d\\times$ pooled SD); the equivalence analysis was not preregistered."
  ),
  label = "tab:power-analysisTOST"
)
write_booktabs(
  tost_sensitivity,
  out = table_file("tab_TOST_sensitivity.tex"),
  caption = paste(
    "Sensitivity of the TOST equivalence conclusion to the standardized margin.",
    "Entries are equivalence $p$-values; $p<0.05$ establishes equivalence at that margin."
  ),
  label = "tab:TOST-sensitivity"
)

# ---- Ordered-outcome robustness ------------------------------------------------

analysis_sample$performance_ordered <- ordered(
  as.factor(analysis_sample$performance), c("1", "2", "3", "4")
)
analysis_sample$productivity_ordered <- ordered(
  as.factor(analysis_sample$productivity), c("1", "2", "3", "4")
)
analysis_sample$own_goals_ordered <- ordered(as.factor(analysis_sample$own_goals))
analysis_sample$supervisor_goals_ordered <- ordered(
  as.factor(analysis_sample$supervisor_goals)
)

fit_ordered_logit <- function(outcome, baseline_outcome, data) {
  model_data <- data
  model_data$baseline_outcome_time0 <- model_data[[baseline_outcome]]
  MASS::polr(
    stats::reformulate(
      c("treatment", "baseline_outcome_time0", ancova_covariates),
      response = outcome
    ),
    data = model_data,
    Hess = TRUE
  )
}

ordered_logit_models <- list(
  "Performance" = fit_ordered_logit(
    "performance_ordered", "performance_time0", analysis_sample
  ),
  "Productivity" = fit_ordered_logit(
    "productivity_ordered", "productivity_time0", analysis_sample
  ),
  "Own goals" = fit_ordered_logit(
    "own_goals_ordered", "own_goals_time0", analysis_sample
  ),
  "Supervisor goals" = fit_ordered_logit(
    "supervisor_goals_ordered", "supervisor_goals_time0", analysis_sample
  )
)
write_reg_table(
  ordered_logit_models,
  out = table_file("ordered_logit_cov.tex"),
  caption = paste(
    "Covariate-adjusted ordered-logit (polr) robustness for the four",
    "categorical Likert outcomes. Each model regresses the ordered",
    "endline outcome on the treatment indicator, its baseline value,",
    "and the same set of pre-treatment covariates as the primary ANCOVA",
    "specifications. Entries are reported as odds ratios."
  ),
  label = "tab:orderedlogit-cov",
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  exponentiate = TRUE,
  notes = c(
    "Odds ratios greater than 1 indicate higher odds of a higher outcome category.",
    "Default Hessian-based standard errors, transformed to the odds-ratio scale, are in parentheses.",
    "Cutpoint coefficients are omitted from the display."
  )
)

# ---- Cluster-robustness checks -------------------------------------------------

icc_metrics <- function(outcome, cluster) {
  complete <- !is.na(outcome) & !is.na(cluster)
  result <- tryCatch(
    fishmethods::clus.rho(outcome[complete], cluster[complete], est = 0)$icc,
    error = function(error) NULL
  )
  if (is.null(result)) return(c(anova = NA_real_, spread = NA_real_))
  values <- as.numeric(result)
  c(anova = values[length(values)], spread = diff(range(values, na.rm = TRUE)))
}

icc_outcomes <- list(
  "Stress" = c("stress_aggregate", "stress_aggregate_time0"),
  "Life satisfaction" = c("life_satisfaction", "life_satisfaction_time0"),
  "Performance" = c("performance", "performance_time0"),
  "Productivity" = c("productivity", "productivity_time0"),
  "Learning" = c("learning", "learning_time0")
)
icc_table <- data.frame(
  Outcome = character(), full_baseline = numeric(), full_endline = numeric(),
  robustness_baseline = numeric(), robustness_endline = numeric(),
  stringsAsFactors = FALSE
)
icc_spreads <- numeric(0)
for (name in names(icc_outcomes)) {
  endline_variable <- icc_outcomes[[name]][1]
  baseline_variable <- icc_outcomes[[name]][2]
  full_baseline_icc <- icc_metrics(
    analysis_sample[[baseline_variable]], analysis_sample$randomization_unit
  )
  full_endline_icc <- icc_metrics(
    analysis_sample[[endline_variable]], analysis_sample$randomization_unit
  )
  robustness_baseline_icc <- icc_metrics(
    robustness_sample[[baseline_variable]],
    robustness_sample$randomization_unit
  )
  robustness_endline_icc <- icc_metrics(
    robustness_sample[[endline_variable]],
    robustness_sample$randomization_unit
  )
  icc_table[nrow(icc_table) + 1, ] <- list(
    name, full_baseline_icc["anova"], full_endline_icc["anova"],
    robustness_baseline_icc["anova"], robustness_endline_icc["anova"]
  )
  icc_spreads <- c(
    icc_spreads, full_baseline_icc["spread"], full_endline_icc["spread"],
    robustness_baseline_icc["spread"], robustness_endline_icc["spread"]
  )
}
message(sprintf(
  "Maximum spread among the three ICC estimators: %.3f.",
  max(icc_spreads, na.rm = TRUE)
))

icc_tex <- kableExtra::kbl(
  icc_table,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  col.names = c("Outcome", "Baseline", "Endline", "Baseline", "Endline"),
  caption = paste(
    "Randomization-unit intracluster correlation (ANOVA one-way random-effects",
    "$\\rho$, R \\texttt{fishmethods}) for the five primary outcomes in the full",
    "analysis sample and the initial-stage robustness sample at both timepoints.",
    "These are robustness diagnostics; the primary models use HC1 standard errors.",
    "ICCs are near zero for mental health but moderate-to-substantial for performance and",
    "learning at both timepoints, indicating within-unit correlation in the work and",
    "study outcomes."
  ),
  label = "ICC",
  linesep = "",
  escape = FALSE
)
icc_tex <- kableExtra::add_header_above(
  icc_tex,
  c(" " = 1, "Full analysis sample" = 2,
    "Initial-stage robustness sample" = 2)
)
writeLines(as.character(icc_tex), table_file("ICC_updated.tex"))

cluster_vcov <- function(model, cluster) {
  as.matrix(clubSandwich::vcovCR(model, cluster = cluster, type = "CR2"))
}

vcov_list_at <- function(models, cluster) {
  lapply(models, cluster_vcov, cluster = cluster)
}

cr2_satt <- function(model, cluster, coefficient = "treatmentTRUE") {
  test <- clubSandwich::coef_test(
    model, vcov = "CR2", cluster = cluster, test = "Satterthwaite"
  )
  row <- which(rownames(test) == coefficient)
  if (length(row) != 1L) {
    stop("Treatment coefficient not found in CR2 test.", call. = FALSE)
  }
  list(
    estimate = test$beta[row], se = test$SE[row], df = test$df_Satt[row],
    statistic = test$tstat[row], p = test$p_Satt[row]
  )
}

stress_ancova_robustness <- fit_adjusted_ols(
  "stress_aggregate", ancova_covariates, robustness_sample
)
life_satisfaction_ancova_robustness <- fit_adjusted_ols(
  "life_satisfaction", ancova_covariates, robustness_sample
)
stress_change_robustness <- fit_change_score(
  "delta_stress", robustness_sample, omit = "stress_aggregate_time0"
)
life_satisfaction_change_robustness <- fit_change_score(
  "delta_lifesat", robustness_sample, omit = "life_satisfaction_time0"
)
performance_ancova_robustness <- fit_work_ancova(
  "performance", "performance_time0", robustness_sample
)
productivity_ancova_robustness <- fit_work_ancova(
  "productivity", "productivity_time0", robustness_sample
)
learning_ancova_robustness <- fit_work_ancova(
  "learning", "learning_time0", robustness_sample
)
performance_change_robustness <- fit_change_score(
  "delta_performance", robustness_sample
)
productivity_change_robustness <- fit_change_score(
  "delta_productivity", robustness_sample
)
learning_change_robustness <- fit_change_score(
  "delta_learning", robustness_sample
)

mental_health_robustness_models <- list(
  "Stress" = stress_ancova_robustness,
  "Life satisfaction" = life_satisfaction_ancova_robustness,
  "$\\Delta$ Stress" = stress_change_robustness,
  "$\\Delta$ Life satisfaction" = life_satisfaction_change_robustness
)
work_robustness_models <- list(
  "Performance" = performance_ancova_robustness,
  "Productivity" = productivity_ancova_robustness,
  "Learning" = learning_ancova_robustness,
  "$\\Delta$ Performance" = performance_change_robustness,
  "$\\Delta$ Productivity" = productivity_change_robustness,
  "$\\Delta$ Learning" = learning_change_robustness
)

write_reg_table(
  mental_health_robustness_models,
  vcov_list_at(
    mental_health_robustness_models,
    robustness_sample$randomization_unit
  ),
  out = table_file("lm_mentalhealth_cov_cluster.tex"),
  caption = paste(
    "Mental-health outcomes (stress and life satisfaction), covariate-adjusted",
    "level and delta models, refit on the cluster robustness sample with CR2",
    "cluster-robust standard errors at the randomization unit."
  ),
  label = "tab:lm-mentalhealth-cov-cluster",
  notes = "CR2 SEs at the randomization unit; cluster robustness sample."
)
write_reg_table(
  work_robustness_models,
  vcov_list_at(work_robustness_models, robustness_sample$randomization_unit),
  out = table_file("lm_performance_cov_cluster.tex"),
  caption = paste(
    "Performance, productivity, and learning, covariate-adjusted level and delta",
    "models, refit on the cluster robustness sample with CR2 cluster-robust standard",
    "errors at the randomization unit."
  ),
  label = "tab:lm-performance-cov-cluster",
  notes = "CR2 SEs at the randomization unit; cluster robustness sample."
)

mental_health_full_sample_models <- list(
  "Stress" = stress_ancova,
  "Life satisfaction" = life_satisfaction_ancova,
  "$\\Delta$ Stress" = stress_change,
  "$\\Delta$ Life satisfaction" = life_satisfaction_change
)
work_full_sample_models <- list(
  "Performance" = performance_ancova,
  "Productivity" = productivity_ancova,
  "Learning" = learning_ancova,
  "$\\Delta$ Performance" = performance_change,
  "$\\Delta$ Productivity" = productivity_change,
  "$\\Delta$ Learning" = learning_change
)

write_reg_table(
  mental_health_full_sample_models,
  vcov_list_at(mental_health_full_sample_models, analysis_sample$randomization_unit),
  out = table_file("lm_mentalhealth_cov_effective.tex"),
  caption = paste(
    "Mental-health outcomes (stress and life satisfaction), covariate-adjusted",
    "level and delta models on the full analysis sample with CR2 cluster-robust standard",
    "errors at the randomization unit."
  ),
  label = "tab:lm-mentalhealth-cov-effective",
  notes = "CR2 SEs at the randomization unit; full analysis sample."
)
write_reg_table(
  work_full_sample_models,
  vcov_list_at(work_full_sample_models, analysis_sample$randomization_unit),
  out = table_file("lm_performance_cov_effective.tex"),
  caption = paste(
    "Performance, productivity, and learning, covariate-adjusted level and delta",
    "models on the full analysis sample with CR2 cluster-robust standard errors at the",
    "randomization unit."
  ),
  label = "tab:lm-performance-cov-effective",
  notes = "CR2 SEs at the randomization unit; full analysis sample."
)

satterthwaite_specs <- list(
  list(
    outcome = "Stress",
    robustness_level = stress_ancova_robustness,
    robustness_change = stress_change_robustness,
    full_level = stress_ancova,
    full_change = stress_change
  ),
  list(
    outcome = "Life satisfaction",
    robustness_level = life_satisfaction_ancova_robustness,
    robustness_change = life_satisfaction_change_robustness,
    full_level = life_satisfaction_ancova,
    full_change = life_satisfaction_change
  ),
  list(
    outcome = "Performance",
    robustness_level = performance_ancova_robustness,
    robustness_change = performance_change_robustness,
    full_level = performance_ancova,
    full_change = performance_change
  ),
  list(
    outcome = "Productivity",
    robustness_level = productivity_ancova_robustness,
    robustness_change = productivity_change_robustness,
    full_level = productivity_ancova,
    full_change = productivity_change
  ),
  list(
    outcome = "Learning",
    robustness_level = learning_ancova_robustness,
    robustness_change = learning_change_robustness,
    full_level = learning_ancova,
    full_change = learning_change
  )
)
satterthwaite_table <- data.frame(
  outcome = character(), spec = character(), sample = character(),
  estimate = numeric(), cr2_se = numeric(), df_satt = numeric(),
  p_satt = numeric(), stringsAsFactors = FALSE
)

append_satterthwaite_row <- function(
    table, outcome, specification, sample_label, model, cluster) {
  result <- cr2_satt(model, cluster)
  table[nrow(table) + 1, ] <- list(
    outcome, specification, sample_label,
    result$estimate, result$se, result$df, result$p
  )
  table
}

for (specification in satterthwaite_specs) {
  satterthwaite_table <- append_satterthwaite_row(
    satterthwaite_table, specification$outcome, "level",
    "cluster robustness sample",
    specification$robustness_level, robustness_sample$randomization_unit
  )
  satterthwaite_table <- append_satterthwaite_row(
    satterthwaite_table, specification$outcome, "delta",
    "cluster robustness sample",
    specification$robustness_change, robustness_sample$randomization_unit
  )
  satterthwaite_table <- append_satterthwaite_row(
    satterthwaite_table, specification$outcome, "level", "full analysis sample",
    specification$full_level, analysis_sample$randomization_unit
  )
  satterthwaite_table <- append_satterthwaite_row(
    satterthwaite_table, specification$outcome, "delta", "full analysis sample",
    specification$full_change, analysis_sample$randomization_unit
  )
}
names(satterthwaite_table) <- c(
  "Outcome", "Specification", "Sample", "Estimate", "CR2 SE",
  "Satterthwaite df", "p-value"
)
write_booktabs(
  satterthwaite_table,
  out = table_file("cr2_satterthwaite.tex"),
  caption = paste(
    "Small-sample CR2 robustness checks for the treatment effect",
    "for the five primary outcomes, using clubSandwich Satterthwaite degrees of",
    "freedom in the full analysis sample and the cluster robustness sample."
  ),
  label = "tab:cr2-satterthwaite"
)

# ---- Attrition bounds -----------------------------------------------------------

attrition_sample <- data.frame(
  user_id = baseline$user_id,
  treatment = baseline$treatment,
  observed = as.integer(baseline$user_id %in% endline$user_id)
)

match_endline_outcome <- function(attrition, endline_data, outcome) {
  values <- rep(NA_real_, nrow(attrition))
  rows <- match(attrition$user_id, endline_data$user_id)
  present <- !is.na(rows)
  values[present] <- endline_data[[outcome]][rows[present]]
  values
}

lee_bounds <- function(outcome, treatment, observed) {
  treated <- as.logical(treatment)
  treatment_values <- outcome[treated & observed == 1]
  control_values <- outcome[!treated & observed == 1]
  treatment_values <- treatment_values[!is.na(treatment_values)]
  control_values <- control_values[!is.na(control_values)]
  p_treatment <- mean(observed[treated])
  p_control <- mean(observed[!treated])
  naive <- mean(treatment_values) - mean(control_values)

  if (p_treatment == p_control) {
    return(list(
      naive = naive, lower = naive, upper = naive,
      trim_fraction = 0, arm_trimmed = "none"
    ))
  }

  if (p_treatment > p_control) {
    trim_fraction <- (p_treatment - p_control) / p_treatment
    keep <- max(
      1L,
      length(treatment_values) - round(trim_fraction * length(treatment_values))
    )
    sorted <- sort(treatment_values)
    lower <- mean(sorted[seq_len(keep)]) - mean(control_values)
    upper <- mean(tail(sorted, keep)) - mean(control_values)
    return(list(
      naive = naive, lower = lower, upper = upper,
      trim_fraction = trim_fraction, arm_trimmed = "treatment"
    ))
  }

  trim_fraction <- (p_control - p_treatment) / p_control
  keep <- max(
    1L,
    length(control_values) - round(trim_fraction * length(control_values))
  )
  sorted <- sort(control_values)
  upper_control <- mean(sorted[seq_len(keep)])
  lower_control <- mean(tail(sorted, keep))
  list(
    naive = naive,
    lower = mean(treatment_values) - lower_control,
    upper = mean(treatment_values) - upper_control,
    trim_fraction = trim_fraction,
    arm_trimmed = "control"
  )
}

imbens_manski_critical <- function(gap, largest_se, alpha = 0.05) {
  if (!is.finite(gap) || !is.finite(largest_se) || largest_se <= 0) {
    return(stats::qnorm(1 - alpha / 2))
  }
  equation <- function(critical) {
    stats::pnorm(critical + gap / largest_se) - stats::pnorm(-critical) -
      (1 - alpha)
  }
  stats::uniroot(equation, interval = c(0, 10))$root
}

lee_bounds_ci <- function(
    attrition, endline_data, outcome, replications, alpha = 0.05) {
  values <- match_endline_outcome(attrition, endline_data, outcome)
  point <- lee_bounds(values, attrition$treatment, attrition$observed)
  treated_rows <- which(attrition$treatment)
  control_rows <- which(!attrition$treatment)
  lower <- upper <- numeric(replications)

  for (replication in seq_len(replications)) {
    sample_rows <- c(
      sample(treated_rows, length(treated_rows), replace = TRUE),
      sample(control_rows, length(control_rows), replace = TRUE)
    )
    bootstrap <- lee_bounds(
      values[sample_rows],
      attrition$treatment[sample_rows],
      attrition$observed[sample_rows]
    )
    lower[replication] <- bootstrap$lower
    upper[replication] <- bootstrap$upper
  }

  lower_se <- stats::sd(lower, na.rm = TRUE)
  upper_se <- stats::sd(upper, na.rm = TRUE)
  critical <- imbens_manski_critical(
    point$upper - point$lower, max(lower_se, upper_se), alpha
  )
  list(
    naive = point$naive,
    lower = point$lower,
    upper = point$upper,
    ci_lower = point$lower - critical * lower_se,
    ci_upper = point$upper + critical * upper_se,
    trim_fraction = point$trim_fraction,
    arm_trimmed = point$arm_trimmed
  )
}

set.seed(2009)
lee_replications <- 2000L
lee_outcomes <- c(
  "Stress" = "stress_aggregate",
  "Life satisfaction" = "life_satisfaction",
  "Performance" = "performance",
  "Productivity" = "productivity",
  "Learning" = "learning"
)
lee_table <- data.frame(
  outcome = character(), naive_difference = numeric(), lower_bound = numeric(),
  upper_bound = numeric(), ci_lower = numeric(), ci_upper = numeric(),
  trim_frac = numeric(), arm_trimmed = character(), stringsAsFactors = FALSE
)
for (outcome_label in names(lee_outcomes)) {
  outcome_variable <- lee_outcomes[[outcome_label]]
  result <- lee_bounds_ci(
    attrition_sample, endline, outcome_variable, lee_replications
  )
  lee_table[nrow(lee_table) + 1, ] <- list(
    outcome_label, result$naive, result$lower, result$upper,
    result$ci_lower, result$ci_upper, result$trim_fraction, result$arm_trimmed
  )
}
names(lee_table) <- c(
  "Outcome", "Naive difference", "Lower bound", "Upper bound",
  "CI lower", "CI upper", "Trim fraction", "Arm trimmed"
)
write_booktabs(
  lee_table,
  out = table_file("lee_bounds.tex"),
  caption = paste(
    "Lee (2009) bounds for each headline outcome among participants who would be",
    "observed under either assignment, accounting for differential attrition.",
    "The maintained monotonicity assumption is that treatment weakly reduces",
    "retention. The CI lower and CI upper columns",
    sprintf(
      paste(
        "give the 95\\%% Imbens-Manski confidence interval for this always-observed",
        "stratum effect, with bound standard errors from %d arm-stratified bootstrap",
        "replications."
      ),
      lee_replications
    )
  ),
  label = "tab:lee-bounds"
)
