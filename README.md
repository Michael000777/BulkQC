# BulkQC

BulkQC is a Shiny app for quick QC of bulk RNA-seq count matrices. The app loads a counts table and sample metadata, shows a few sample-level checks, and exports the tables and plots that are useful for review.

## What it does

- Upload counts and sample metadata
- Preview the parsed input files
- Review per-sample QC metrics
- Plot QC metric distributions and raw count distributions
- Run PCA and color samples by metadata columns
- Download selected QC outputs as a ZIP

## Typical workflow

1. Upload a count matrix (and optional metadata)
2. Review QC summaries and distributions
3. Check the PCA plot for sample-level patterns
4. Export selected tables and plots

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

## Export

The Export tab downloads a ZIP containing the selected outputs:

- `qc_table.csv`
- `pca_plot.html`
- `qc_histogram.html`
- `count_distribution.html`

## Tech stack

- R / Shiny
- `{golem}`
- `{ggplot2}` and `{plotly}`
- `{DT}` for table previews

## Running locally

```r
setwd("BulkQC")

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::restore()
source("dev/run_dev.R")
```
