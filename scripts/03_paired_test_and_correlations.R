# This script runs the main statistical analyses for the demo project.
#
# Statistical workflow:
# - pairwise filtering for valid observations
# - Shapiro-Wilk normality test on paired differences
# - paired t-test or Wilcoxon signed-rank test
# - correlation analysis using Spearman or adaptive Pearson/Spearman
# - multiple testing correction (Benjamini-Hochberg)

# -----------------------------
# 0) Libraries
# -----------------------------
library(readxl)
library(writexl)
library(dplyr)

# -----------------------------
# 1) Create output folders if they don't exist
# -----------------------------
if (!dir.exists("outputs")) {
  dir.create("outputs", recursive = TRUE)
}

if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables", recursive = TRUE)
}

# -----------------------------
# 2) Read cleaned datasets
# -----------------------------
small_data <- read_excel("data_demo/cleaned/demo_small_data_clean.xlsx")
incomplete_data <- read_excel("data_demo/cleaned/demo_incomplete_data_clean.xlsx")

# -----------------------------
# 3) Define variable groups
# -----------------------------
before_treatment_vars <- c("before_treatment_param_1", "before_treatment_param_2", "before_treatment_param_3")
after_treatment_vars <- c("after_treatment_param_1", "after_treatment_param_2", "after_treatment_param_3")
molecule_vars <- c("molecule_1", "molecule_2", "molecule_3")

paired_var_map <- data.frame(
  before_treatment_var = before_treatment_vars,
  after_treatment_var = after_treatment_vars,
  stringsAsFactors = FALSE
)

# -----------------------------
# 4) Helper functions
# -----------------------------

# Pairwise valid rows: both values exist and are not zero
get_valid_pairs <- function(data, var1, var2) {
  valid_idx <- !is.na(data[[var1]]) &
    !is.na(data[[var2]]) &
    data[[var1]] != 0 &
    data[[var2]] != 0
  
  data[valid_idx, c(var1, var2), drop = FALSE]
}

# Choose paired t-test or Wilcoxon based on the Shapiro test
run_paired_test <- function(data, before_treatment_var, after_treatment_var) {
  pair_data <- get_valid_pairs(data, before_treatment_var, after_treatment_var)
  
  n_valid <- nrow(pair_data)
  
  if (n_valid < 3) {
    return(data.frame(
      before_treatment_var = before_treatment_var,
      after_treatment_var = after_treatment_var,
      n_valid = n_valid,
      mean_before_treatment = NA_real_,
      mean_after_treatment = NA_real_,
      median_before_treatment = NA_real_,
      median_after_treatment = NA_real_,
      shapiro_p = NA_real_,
      test_used = "not_enough_data",
      statistic = NA_real_,
      p_value = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  
  diff_values <- pair_data[[after_treatment_var]] - pair_data[[before_treatment_var]]
  
  shapiro_p <- NA_real_
  
  # Shapiro only works properly when there is enough variation
  if (length(unique(diff_values)) >= 3 && n_valid >= 3) {
    shapiro_p <- tryCatch(
      shapiro.test(diff_values)$p.value,
      error = function(e) NA_real_
    )
  }
  
  if (!is.na(shapiro_p) && shapiro_p > 0.05) {
    test_res <- t.test(pair_data[[before_treatment_var]], pair_data[[after_treatment_var]], paired = TRUE)
    test_used <- "paired_t_test"
    statistic <- unname(test_res$statistic)
    p_value <- test_res$p.value
  } else {
    test_res <- wilcox.test(pair_data[[before_treatment_var]], pair_data[[after_treatment_var]], paired = TRUE, exact = FALSE)
    test_used <- "wilcoxon_signed_rank"
    statistic <- unname(test_res$statistic)
    p_value <- test_res$p.value
  }
  
  data.frame(
    before_treatment_var = before_treatment_var,
    after_treatment_var = after_treatment_var,
    n_valid = n_valid,
    mean_before_treatment = mean(pair_data[[before_treatment_var]], na.rm = TRUE),
    mean_after_treatment = mean(pair_data[[after_treatment_var]], na.rm = TRUE),
    median_before_treatment = median(pair_data[[before_treatment_var]], na.rm = TRUE),
    median_after_treatment = median(pair_data[[after_treatment_var]], na.rm = TRUE),
    shapiro_p = shapiro_p,
    test_used = test_used,
    statistic = statistic,
    p_value = p_value,
    stringsAsFactors = FALSE
  )
}

# Helper function for correlations
run_correlation <- function(data, molecule_var, target_var, adaptive_method = FALSE) {
  valid_idx <- !is.na(data[[molecule_var]]) &
    !is.na(data[[target_var]]) &
    data[[molecule_var]] != 0 &
    data[[target_var]] != 0
  
  pair_data <- data[valid_idx, c(molecule_var, target_var), drop = FALSE]
  n_valid <- nrow(pair_data)
  
  if (n_valid < 3) {
    return(data.frame(
      molecule = molecule_var,
      target = target_var,
      n_valid = n_valid,
      method = "not_enough_data",
      estimate = NA_real_,
      p_value = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  
  method_to_use <- "spearman"
  
  if (adaptive_method) {
    x <- pair_data[[molecule_var]]
    y <- pair_data[[target_var]]
    
    shapiro_x <- tryCatch(
      if (length(unique(x)) >= 3) shapiro.test(x)$p.value else NA_real_,
      error = function(e) NA_real_
    )
    
    shapiro_y <- tryCatch(
      if (length(unique(y)) >= 3) shapiro.test(y)$p.value else NA_real_,
      error = function(e) NA_real_
    )
    
    if (!is.na(shapiro_x) && !is.na(shapiro_y) &&
        shapiro_x > 0.05 && shapiro_y > 0.05) {
      method_to_use <- "pearson"
    }
  }
  
  cor_res <- suppressWarnings(
    cor.test(pair_data[[molecule_var]], pair_data[[target_var]], method = method_to_use)
  )
  
  data.frame(
    molecule = molecule_var,
    target = target_var,
    n_valid = n_valid,
    method = method_to_use,
    estimate = unname(cor_res$estimate),
    p_value = cor_res$p.value,
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# 5) Run paired tests
# -----------------------------
run_paired_block <- function(data, dataset_name) {
  results <- do.call(
    rbind,
    lapply(seq_len(nrow(paired_var_map)), function(i) {
      run_paired_test(data, paired_var_map$before_treatment_var[i], paired_var_map$after_treatment_var[i])
    })
  )
  
  results$dataset <- dataset_name
  results$p_adjust_bh <- p.adjust(results$p_value, method = "BH")
  results <- results %>%
    select(dataset, everything())
  
  results
}

paired_results_small <- run_paired_block(small_data, "demo_small_data")
paired_results_incomplete <- run_paired_block(incomplete_data, "demo_incomplete_data")

paired_results_all <- bind_rows(paired_results_small, paired_results_incomplete)

# -----------------------------
# 6) Run molecule correlations
# -----------------------------
run_correlation_block <- function(data, dataset_name, adaptive_method = FALSE) {
  target_vars <- c(before_treatment_vars, after_treatment_vars)
  
  results_list <- list()
  counter <- 1
  
  for (mol in molecule_vars) {
    for (target in target_vars) {
      results_list[[counter]] <- run_correlation(
        data = data,
        molecule_var = mol,
        target_var = target,
        adaptive_method = adaptive_method
      )
      counter <- counter + 1
    }
  }
  
  results <- bind_rows(results_list)
  results$dataset <- dataset_name
  results$p_adjust_bh <- p.adjust(results$p_value, method = "BH")
  results <- results %>%
    select(dataset, everything())
  
  results
}

# Main correlation output: Spearman only
cor_results_small_spearman <- run_correlation_block(
  small_data,
  "demo_small_data",
  adaptive_method = FALSE
)

cor_results_incomplete_spearman <- run_correlation_block(
  incomplete_data,
  "demo_incomplete_data",
  adaptive_method = FALSE
)

cor_results_all_spearman <- bind_rows(
  cor_results_small_spearman,
  cor_results_incomplete_spearman
)

# Optional version with automatic Pearson/Spearman selection
cor_results_small_adaptive <- run_correlation_block(
  small_data,
  "demo_small_data",
  adaptive_method = TRUE
)

cor_results_incomplete_adaptive <- run_correlation_block(
  incomplete_data,
  "demo_incomplete_data",
  adaptive_method = TRUE
)

cor_results_all_adaptive <- bind_rows(
  cor_results_small_adaptive,
  cor_results_incomplete_adaptive
)

# -----------------------------
# 7) Save outputs
# -----------------------------
write_xlsx(
  paired_results_all,
  "outputs/tables/paired_before_treatment_vs_after_treatment_results.xlsx"
)

write_xlsx(
  cor_results_all_spearman,
  "outputs/tables/molecule_correlations_spearman.xlsx"
)

write_xlsx(
  cor_results_all_adaptive,
  "outputs/tables/molecule_correlations_adaptive.xlsx"
)

# -----------------------------
# 8) Console summary
# -----------------------------
cat("Statistical analysis completed successfully.\n\n")

cat("Saved files:\n")
cat("- outputs/tables/paired_before_treatment_vs_after_treatment_results.xlsx\n")
cat("- outputs/tables/molecule_correlations_spearman.xlsx\n")
cat("- outputs/tables/molecule_correlations_adaptive.xlsx\n\n")

cat("Paired comparison results:\n")
print(paired_results_all)

cat("\nSpearman correlation results (first rows):\n")
print(head(cor_results_all_spearman))

cat("\nAdaptive correlation results (first rows):\n")
print(head(cor_results_all_adaptive))