# Healthcare statistical analysis in R

## Project overview

This project shows a statistical analysis workflow in R based on work I did for a medical thesis.

The goal of the original work was to explore relationships between measured variables across two different conditions.

The data in this repository is fully simulated. The original dataset cannot be shared, so the focus here is on the workflow, the statistical methods, and the way the data was handled.

---

## What the project does

The project is built as a simple step-by-step pipeline:

1. Generate demo data

   * Simulated dataset with missing values and messy inputs
   * Includes typical issues like `NA`, `?`, and `<value` formats

2. Clean and prepare the data

   * Convert character values to numeric
   * Handle missing values
   * Standardize variables

3. Run statistical analysis

   * Compare values between two conditions (before_treatment vs after_treatment)
   * Choose test based on normality (t-test or Wilcoxon)
   * Run correlation analysis between molecules and measured parameters
   * Apply multiple testing correction

4. Create visualizations

   * Barplots for before_treatment vs after_treatment comparisons
   * Q-Q plots for distribution checking
   * Correlation heatmaps
   * Scatter plots for selected relationships

---

## Project structure

```
.
├── data_demo/
│   ├── demo_small_data.xlsx
│   ├── demo_incomplete_data.xlsx
│   └── cleaned/
│       ├── demo_small_data_clean.xlsx
│       └── demo_incomplete_data_clean.xlsx
│
├── scripts/
│   ├── 01_generate_demo_data.R
│   ├── 02_clean_and_prepare_data.R
│   ├── 03_paired_tests_and_correlations.R
│   └── 04_visualizations.R
│
├── outputs/
│   ├── tables/
│   └── plots/
│
└── README.md
```

---

## Methods used

* Shapiro–Wilk test for normality
* Paired t-test and Wilcoxon test
* Spearman correlation (with optional Pearson)
* Benjamini–Hochberg correction

---

## Notes on the data

The dataset in this repository is simulated.

In the original work, I worked with:

* a small, clean dataset
* a larger, highly incomplete dataset

The demo data tries to reflect similar problems (missing values, mixed formats), but does not contain any real information.

---

## Author

Bátori Szilvia  
Independent statistical analysis (R) for a medical thesis project
