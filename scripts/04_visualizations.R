# This script creates the main plots for the demo project:
# barplots, selected Q-Q plots, correlation heatmaps,
# and a few scatter plots for significant correlations.

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(stringr)

# Create output folders if they don't exist
dir.create("outputs/plots", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/plots/barplots", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/plots/qq_plots", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/plots/heatmaps", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/plots/correlation_scatter", showWarnings = FALSE, recursive = TRUE)

# Read cleaned demo data
small_data <- read_excel("data_demo/cleaned/demo_small_data_clean.xlsx")
incomplete_data <- read_excel("data_demo/cleaned/demo_incomplete_data_clean.xlsx")

# Variable groups
before_treatment_vars <- c(
  "before_treatment_param_1",
  "before_treatment_param_2",
  "before_treatment_param_3"
)

after_treatment_vars <- c(
  "after_treatment_param_1",
  "after_treatment_param_2",
  "after_treatment_param_3"
)

molecule_vars <- c(
  "molecule_1",
  "molecule_2",
  "molecule_3"
)

paired_var_map <- data.frame(
  before_treatment = before_treatment_vars,
  after_treatment = after_treatment_vars,
  stringsAsFactors = FALSE
)

# Keep only rows where both variables are present and non-zero
get_valid_pairs <- function(data, var1, var2) {
  valid_idx <- !is.na(data[[var1]]) &
    !is.na(data[[var2]]) &
    data[[var1]] != 0 &
    data[[var2]] != 0
  
  data[valid_idx, c(var1, var2), drop = FALSE]
}

# Safe Shapiro test helper
shapiro_p_safe <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n >= 3 && n <= 5000 && length(unique(x)) >= 3) {
    return(tryCatch(shapiro.test(x)$p.value, error = function(e) NA_real_))
  }
  
  NA_real_
}

# Turn p-values into simple significance labels
get_signif_label <- function(p) {
  dplyr::case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

# Build a simple Spearman correlation table
run_spearman_table <- function(data, dataset_name) {
  results <- data.frame(
    molecule = character(),
    target = character(),
    rho = numeric(),
    p_value = numeric(),
    n_valid = integer(),
    stringsAsFactors = FALSE
  )
  
  target_vars <- c(before_treatment_vars, after_treatment_vars)
  
  for (mol in molecule_vars) {
    for (target in target_vars) {
      x <- data[[mol]]
      y <- data[[target]]
      
      ok <- !is.na(x) & !is.na(y) & x != 0 & y != 0
      x2 <- x[ok]
      y2 <- y[ok]
      n_valid <- length(x2)
      
      if (n_valid >= 3) {
        test <- suppressWarnings(cor.test(x2, y2, method = "spearman", exact = FALSE))
        
        results <- rbind(
          results,
          data.frame(
            molecule = mol,
            target = target,
            rho = as.numeric(test$estimate),
            p_value = test$p.value,
            n_valid = n_valid,
            stringsAsFactors = FALSE
          )
        )
      } else {
        results <- rbind(
          results,
          data.frame(
            molecule = mol,
            target = target,
            rho = NA_real_,
            p_value = NA_real_,
            n_valid = n_valid,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  
  results$dataset <- dataset_name
  results$p_adjust_bh <- p.adjust(results$p_value, method = "BH")
  results
}

# Barplots for paired comparisons
make_barplot <- function(data, before_var, after_var, dataset_name, out_dir) {
  pair_data <- get_valid_pairs(data, before_var, after_var)
  n_valid <- nrow(pair_data)
  
  if (n_valid < 3) return(NULL)
  
  diff_values <- pair_data[[after_var]] - pair_data[[before_var]]
  shapiro_p <- shapiro_p_safe(diff_values)
  
  if (!is.na(shapiro_p) && shapiro_p > 0.05) {
    test_method <- "t.test"
    test_label <- "paired t-test"
  } else {
    test_method <- "wilcox.test"
    test_label <- "Wilcoxon"
  }
  
  plot_data <- data.frame(
    value = c(pair_data[[before_var]], pair_data[[after_var]]),
    condition = rep(c("Before treatment", "After treatment"), each = n_valid)
  )
  
  p <- ggplot(plot_data, aes(x = condition, y = value, fill = condition)) +
    stat_summary(fun = mean, geom = "col", width = 0.6) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
    geom_jitter(width = 0.08, size = 2, alpha = 0.9) +
    scale_fill_manual(values = c(
      "Before treatment" = "#1f78b4",
      "After treatment" = "#e31a1c"
    )) +
    stat_compare_means(
      comparisons = list(c("Before treatment", "After treatment")),
      method = test_method,
      paired = TRUE,
      label = "p.format"
    ) +
    labs(
      title = paste0(before_var, " vs ", after_var, " (", dataset_name, ")"),
      subtitle = paste0("N = ", n_valid, " | Test: ", test_label),
      x = NULL,
      y = "Value"
    ) +
    theme_classic(base_size = 12) +
    theme(legend.position = "none")
  
  ggsave(
    filename = file.path(
      out_dir,
      paste0(dataset_name, "_", before_var, "_vs_", after_var, "_bar.png")
    ),
    plot = p,
    width = 7,
    height = 4,
    dpi = 300
  )
  
  invisible(p)
}

for (i in seq_len(nrow(paired_var_map))) {
  make_barplot(
    data = small_data,
    before_var = paired_var_map$before_treatment[i],
    after_var = paired_var_map$after_treatment[i],
    dataset_name = "small",
    out_dir = "outputs/plots/barplots"
  )
  
  make_barplot(
    data = incomplete_data,
    before_var = paired_var_map$before_treatment[i],
    after_var = paired_var_map$after_treatment[i],
    dataset_name = "incomplete",
    out_dir = "outputs/plots/barplots"
  )
}

# Only create a few selected Q-Q plots
qq_vars <- c(
  "before_treatment_param_1",
  "after_treatment_param_1",
  "molecule_1"
)

make_selected_qq <- function(data, dataset_name, selected_vars, out_dir) {
  for (var_name in selected_vars) {
    x <- data[[var_name]]
    x <- x[!is.na(x) & x != 0]
    
    if (length(x) < 3) next
    
    qq_plot <- ggplot(data.frame(value = x), aes(sample = value)) +
      stat_qq(color = "steelblue", size = 2) +
      stat_qq_line(color = "red", linewidth = 1) +
      labs(
        title = paste("Q-Q plot:", var_name, "(", dataset_name, ")"),
        subtitle = paste("N =", length(x)),
        x = "Theoretical quantiles",
        y = "Observed quantiles"
      ) +
      theme_classic(base_size = 12)
    
    safe_name <- str_replace_all(var_name, "[^A-Za-z0-9_]", "_")
    
    ggsave(
      filename = file.path(out_dir, paste0(dataset_name, "_", safe_name, "_QQ.png")),
      plot = qq_plot,
      width = 6,
      height = 5,
      dpi = 300
    )
  }
}

make_selected_qq(small_data, "small", qq_vars, "outputs/plots/qq_plots")
make_selected_qq(incomplete_data, "incomplete", qq_vars, "outputs/plots/qq_plots")

# Create one simple heatmap per dataset
make_heatmap <- function(data, dataset_name, out_dir) {
  cor_results <- run_spearman_table(data, dataset_name)
  
  heatmap_data <- cor_results %>%
    mutate(
      signif = sapply(p_value, get_signif_label),
      target = factor(target, levels = c(before_treatment_vars, after_treatment_vars)),
      molecule = factor(molecule, levels = rev(molecule_vars))
    )
  
  p <- ggplot(heatmap_data, aes(x = target, y = molecule, fill = rho)) +
    geom_tile(color = "white") +
    geom_text(aes(label = signif), size = 5) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Spearman rho"
    ) +
    labs(
      title = paste("Correlation heatmap -", dataset_name),
      x = "Parameters",
      y = "Molecules"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  ggsave(
    filename = file.path(out_dir, paste0("heatmap_", dataset_name, ".png")),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  invisible(cor_results)
}

cor_small <- make_heatmap(small_data, "small", "outputs/plots/heatmaps")
cor_incomplete <- make_heatmap(incomplete_data, "incomplete", "outputs/plots/heatmaps")

# Create scatter plots for the top significant correlations
make_top_scatter <- function(data, cor_results, dataset_name, out_dir, top_n = 4) {
  sig_pairs <- cor_results %>%
    filter(!is.na(p_value), p_value < 0.05) %>%
    arrange(p_value) %>%
    slice_head(n = top_n)
  
  if (nrow(sig_pairs) == 0) return(NULL)
  
  for (i in seq_len(nrow(sig_pairs))) {
    mol <- sig_pairs$molecule[i]
    target <- sig_pairs$target[i]
    
    plot_data <- data %>%
      transmute(x = .data[[target]], y = .data[[mol]]) %>%
      filter(!is.na(x), !is.na(y), x != 0, y != 0)
    
    if (nrow(plot_data) < 3) next
    
    p <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", se = TRUE) +
      stat_cor(method = "spearman", label.x.npc = "left", label.y.npc = "top") +
      labs(
        title = paste0(mol, " vs ", target, " (", dataset_name, ")"),
        x = target,
        y = mol
      ) +
      theme_classic(base_size = 12) +
      annotate(
        "text",
        x = Inf, y = -Inf,
        hjust = 1.05, vjust = -0.6,
        label = paste0("n = ", nrow(plot_data)),
        size = 3.5
      )
    
    ggsave(
      filename = file.path(out_dir, paste0(dataset_name, "_", mol, "_", target, "_scatter.png")),
      plot = p,
      width = 5.5,
      height = 4.2,
      dpi = 300
    )
  }
}

make_top_scatter(small_data, cor_small, "small", "outputs/plots/correlation_scatter", top_n = 4)
make_top_scatter(incomplete_data, cor_incomplete, "incomplete", "outputs/plots/correlation_scatter", top_n = 4)

cat("Visualization script completed successfully.\n")
cat("Saved plots in:\n")
cat("- outputs/plots/barplots/\n")
cat("- outputs/plots/qq_plots/\n")
cat("- outputs/plots/heatmaps/\n")
cat("- outputs/plots/correlation_scatter/\n")