# Purpose:
# Clean and prepare the simulated demo datasets.
# Tasks:
# - read Excel files
# - standardize column names
# - convert problematic character values to numeric where needed
# - handle special symbols such as "<" and "?"
# - clean and standardize datasets for analysis
# - save cleaned outputs

# -----------------------------
# 0) Libraries
# -----------------------------
library(readxl)
library(writexl)
library(dplyr)
library(janitor)

# -----------------------------
# 1) Create output folder if needed
# -----------------------------
if (!dir.exists("data_demo/cleaned")) {
  dir.create("data_demo/cleaned", recursive = TRUE)
}

# -----------------------------
# 2) Read demo Excel files
# -----------------------------
small_raw <- read_excel("data_demo/demo_small_data.xlsx") %>%
  clean_names()

incomplete_raw <- read_excel("data_demo/demo_incomplete_data.xlsx") %>%
  clean_names()

# -----------------------------
# 3) Helper function to clean numeric values imported from Excel
# -----------------------------
clean_numeric_like <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  
  # Treat empty strings and obvious placeholders as missing
  x[x %in% c("", "NA", "N/A", "?", "na", "n/a")] <- NA
  
  # Values with "<" are treated as missing
  x[grepl("<", x, fixed = TRUE)] <- NA
  
  # Replace comma decimal separator if present
  x <- gsub(",", ".", x, fixed = TRUE)
  
  # Convert to numeric
  suppressWarnings(as.numeric(x))
}

# -----------------------------
# 4) Define variable groups
# -----------------------------
before_treatment_vars <- c("before_treatment_param_1", "before_treatment_param_2", "before_treatment_param_3")
after_treatment_vars <- c("after_treatment_param_1", "after_treatment_param_2", "after_treatment_param_3")
molecule_vars <- c("molecule_1", "molecule_2", "molecule_3")

analysis_vars <- c(before_treatment_vars, after_treatment_vars, molecule_vars)

# -----------------------------
# 5) Clean small dataset
# -----------------------------
small_clean <- small_raw

for (col in analysis_vars) {
  if (col %in% names(small_clean)) {
    small_clean[[col]] <- clean_numeric_like(small_clean[[col]])
  }
}

# -----------------------------
# 6) Clean incomplete dataset
# -----------------------------
incomplete_clean <- incomplete_raw

for (col in analysis_vars) {
  if (col %in% names(incomplete_clean)) {
    incomplete_clean[[col]] <- clean_numeric_like(incomplete_clean[[col]])
  }
}

# Standardize grouping variable if present
if ("group_flag" %in% names(incomplete_clean)) {
  incomplete_clean$group_flag <- as.character(incomplete_clean$group_flag)
  incomplete_clean$group_flag <- trimws(incomplete_clean$group_flag)
  
  incomplete_clean$group_flag[incomplete_clean$group_flag %in% c("", "NA", "N/A")] <- NA
}

# -----------------------------
# 7) Optional subgroup split for incomplete dataset
# -----------------------------
group_a_data <- NULL
group_b_data <- NULL

if ("group_flag" %in% names(incomplete_clean)) {
  group_a_data <- incomplete_clean %>%
    filter(group_flag == "Group_A")
  
  group_b_data <- incomplete_clean %>%
    filter(group_flag == "Group_B")
}

# -----------------------------
# 8) Save cleaned datasets
# -----------------------------
write_xlsx(small_clean, "data_demo/cleaned/demo_small_data_clean.xlsx")
write_xlsx(incomplete_clean, "data_demo/cleaned/demo_incomplete_data_clean.xlsx")

if (!is.null(group_a_data)) {
  write_xlsx(group_a_data, "data_demo/cleaned/demo_incomplete_group_a.xlsx")
}

if (!is.null(group_b_data)) {
  write_xlsx(group_b_data, "data_demo/cleaned/demo_incomplete_group_b.xlsx")
}

# -----------------------------
# 9) Console summary
# -----------------------------
cat("Cleaning and preparation completed successfully.\n\n")

cat("Saved files:\n")
cat("- data_demo/cleaned/demo_small_data_clean.xlsx\n")
cat("- data_demo/cleaned/demo_incomplete_data_clean.xlsx\n\n")

cat("Dimensions:\n")
cat("small_clean:", nrow(small_clean), "rows x", ncol(small_clean), "columns\n")
cat("incomplete_clean:", nrow(incomplete_clean), "rows x", ncol(incomplete_clean), "columns\n")

cat("\nMissing values per analysis column (small dataset):\n")
print(colSums(is.na(small_clean[analysis_vars])))

cat("\nMissing values per analysis column (incomplete dataset):\n")
print(colSums(is.na(incomplete_clean[analysis_vars])))