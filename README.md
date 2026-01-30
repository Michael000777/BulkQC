# BulkQC

BulkQC is an RShiny application (built with `{golem}`) for quality control and exploratory analysis of large omics datasets. It is designed for production-style use on real project data, where teams need a consistent way to review sample-level metrics, spot issues early, and export reproducible summaries for downstream work and reporting.

## What it does

- **QC overview**: summarize key sample metrics and identify outliers  
- **Exploration**: interactive plots and tables for drilling into distributions and sample comparisons  
- **Reproducible exports**: download tables/figures and a project snapshot for sharing with collaborators  
- **Modular structure**: organized as a `{golem}` app so features can be extended without turning into a single-file Shiny script  

## Typical workflow

1. Upload a count matrix (and optional metadata)
2. Review QC summaries and distributions
3. Investigate flagged samples and explore project-level patterns
4. Export results for collaboration or inclusion in project updates

## Inputs

BulkQC expects two flat files: a count matrix and a sample metadata table.

### Counts file (CSV/TSV/TXT)
- **Shape:** Genes × Samples  
- **Samples:** sample IDs must be the **column names**  
- **Gene IDs:** by default, BulkQC assumes the **first column is `gene_id`** (toggleable in the UI)
- **Values:** counts are coerced to numeric and must be **non-negative** with **no missing values**

### Metadata file (CSV/TSV/TXT)
- **Shape:** Samples × Covariates  
- Must include a sample identifier column (default **`Sample_id`**, configurable in the UI)
- Metadata is **aligned to the counts matrix** by matching `Sample_id` values to the counts **column names**
- If metadata is missing any sample present in the counts file, BulkQC will report which sample IDs are missing

## Input checks (what BulkQC validates)
- Counts can be converted to numeric with no `NA` introduced
- Counts contain no negative values
- Metadata contains the specified sample ID column
- Metadata contains a row for every sample in the counts matrix

## Tech stack

- **R / Shiny**
- **{golem}** application structure
- **{ggplot2}** + **{plotly}** for interactive visualizations
- Additional packages are listed in `DESCRIPTION`

## Running locally

### Run from R

```r
# install dependencies
remotes::install_deps(dependencies = TRUE)

# run the app
golem::run_dev()
