# Purpose:
# Generate fully simulated demo data for a public portfolio version.
# The demo data mimics the structure and common data quality issues
# of the original project without containing any real patient information.

# -----------------------------
# 0) Setup
# -----------------------------
set.seed(123)

library(writexl)

# Create output folder if it does not exist
if (!dir.exists("data_demo")) {
  dir.create("data_demo", recursive = TRUE)
}

# -----------------------------
# 1) Create base dataset
# -----------------------------
n_total <- 60

base_data <- data.frame(
  subject_id = 1:n_total,
  
  # Before treatment parameters
  before_treatment_param_1 = round(rnorm(n_total, mean = 68, sd = 8), 1),
  before_treatment_param_2 = round(rnorm(n_total, mean = 118, sd = 12), 1),
  before_treatment_param_3 = round(rnorm(n_total, mean = 76, sd = 8), 1),
  
  # After treatment parameters
  after_treatment_param_1 = round(rnorm(n_total, mean = 84, sd = 10), 1),
  after_treatment_param_2 = round(rnorm(n_total, mean = 138, sd = 15), 1),
  after_treatment_param_3 = round(rnorm(n_total, mean = 84, sd = 10), 1),
  
  # Molecules
  molecule_1 = round(rnorm(n_total, mean = 5.2, sd = 1.1), 2),
  molecule_2 = round(rnorm(n_total, mean = 92, sd = 18), 2),
  molecule_3 = round(rnorm(n_total, mean = 1.8, sd = 0.4), 2),
  
  # Grouping variable only for the larger incomplete dataset
  group_flag = sample(c("Group_A", "Group_B", NA), size = n_total, replace = TRUE,
                      prob = c(0.4, 0.4, 0.2)),
  
  stringsAsFactors = FALSE
)

# Make sure numeric values stay positive
numeric_cols <- names(base_data)[sapply(base_data, is.numeric)]
base_data[numeric_cols] <- lapply(base_data[numeric_cols], function(x) {
  ifelse(x < 0, abs(x), x)
})

# -----------------------------
# 2) Create the small, cleaner dataset
# -----------------------------
demo_small_data <- base_data[1:12, ]

# Small dataset should not contain grouping
demo_small_data$group_flag <- NULL

# Add only minimal missingness
demo_small_data$molecule_2[3] <- NA
demo_small_data$after_treatment_param_3[8] <- NA

# -----------------------------
# 3) Create larger dataset with missing and messy values
# -----------------------------
demo_incomplete_data <- base_data

set_na_random <- function(x, n_missing) {
  idx <- sample(seq_along(x), size = n_missing, replace = FALSE)
  x[idx] <- NA
  x
}

demo_incomplete_data$before_treatment_param_1   <- set_na_random(demo_incomplete_data$before_treatment_param_1, 12)
demo_incomplete_data$before_treatment_param_2   <- set_na_random(demo_incomplete_data$before_treatment_param_2, 16)
demo_incomplete_data$before_treatment_param_3   <- set_na_random(demo_incomplete_data$before_treatment_param_3, 10)

demo_incomplete_data$after_treatment_param_1 <- set_na_random(demo_incomplete_data$after_treatment_param_1, 14)
demo_incomplete_data$after_treatment_param_2 <- set_na_random(demo_incomplete_data$after_treatment_param_2, 18)
demo_incomplete_data$after_treatment_param_3 <- set_na_random(demo_incomplete_data$after_treatment_param_3, 15)

demo_incomplete_data$molecule_1    <- set_na_random(demo_incomplete_data$molecule_1, 20)
demo_incomplete_data$molecule_2    <- set_na_random(demo_incomplete_data$molecule_2, 17)
demo_incomplete_data$molecule_3    <- set_na_random(demo_incomplete_data$molecule_3, 19)

# Convert some columns to character (to simulate Excel import issues)
demo_incomplete_data$molecule_2   <- as.character(demo_incomplete_data$molecule_2)
demo_incomplete_data$molecule_3   <- as.character(demo_incomplete_data$molecule_3)
demo_incomplete_data$after_treatment_param_2 <- as.character(demo_incomplete_data$after_treatment_param_2)

# Insert problematic values
problem_rows_mol2 <- sample(1:n_total, 4)
demo_incomplete_data$molecule_2[problem_rows_mol2] <- c("?", "<80", "?", "<95")

problem_rows_mol3 <- sample(setdiff(1:n_total, problem_rows_mol2), 3)
demo_incomplete_data$molecule_3[problem_rows_mol3] <- c("<1.2", "?", "<2.0")

problem_rows_after_treatment2 <- sample(setdiff(1:n_total, c(problem_rows_mol2, problem_rows_mol3)), 3)
demo_incomplete_data$after_treatment_param_2[problem_rows_after_treatment2] <- c("?", "<130", "?")

# Add some zero values for later exclusion logic
zero_rows_1 <- sample(1:n_total, 3)
zero_rows_2 <- sample(1:n_total, 3)

demo_incomplete_data$before_treatment_param_1[zero_rows_1] <- 0
demo_incomplete_data$molecule_1[zero_rows_2] <- 0

# -----------------------------
# 4) Make most rows incomplete (only a few remain usable)
# -----------------------------
analysis_cols <- c(
  "before_treatment_param_1", "before_treatment_param_2", "before_treatment_param_3",
  "after_treatment_param_1", "after_treatment_param_2", "after_treatment_param_3",
  "molecule_1", "molecule_2", "molecule_3"
)

for (i in seq_len(nrow(demo_incomplete_data))) {
  if (i > 4) {
    col_to_damage <- sample(analysis_cols, 1)
    
    if (col_to_damage %in% c("molecule_2", "molecule_3", "after_treatment_param_2")) {
      demo_incomplete_data[i, col_to_damage] <- sample(c(NA, "?", "<1.0", "<50"), 1)
    } else {
      demo_incomplete_data[i, col_to_damage] <- NA
    }
  }
}

# Keep first 4 rows relatively complete
for (col in analysis_cols) {
  if (col %in% c("molecule_2", "molecule_3", "after_treatment_param_2")) {
    demo_incomplete_data[1:4, col] <- as.character(base_data[1:4, col])
  } else {
    demo_incomplete_data[1:4, col] <- base_data[1:4, col]
  }
}

# -----------------------------
# 5) Export demo datasets to Excel
# -----------------------------
write_xlsx(demo_small_data, "data_demo/demo_small_data.xlsx")
write_xlsx(demo_incomplete_data, "data_demo/demo_incomplete_data.xlsx")

# -----------------------------
# 6) Console summary
# -----------------------------
cat("Demo datasets generated successfully.\n")
cat("- Small dataset: data_demo/demo_small_data.xlsx\n")
cat("- Incomplete dataset: data_demo/demo_incomplete_data.xlsx\n\n")

cat("Dimensions:\n")
cat("demo_small_data:", nrow(demo_small_data), "rows x", ncol(demo_small_data), "columns\n")
cat("demo_incomplete_data:", nrow(demo_incomplete_data), "rows x", ncol(demo_incomplete_data), "columns\n")