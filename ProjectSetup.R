# Installing necessary packages

install.packages(c(
  "golem", "usethis", "devtools", "testthat", "lintr",
  "config", "here", "DT", "bslib", "plotly", "renv"
))


#Creating golem project

library(golem)
create_golem("BulkQC")

# Intial scaffold
library(golem)
use_recommended_tests()
use_recommended_deps()
use_utils_ui()

# Initializing modules
golem::add_module(name = "upload")
golem::add_module(name = "filters")
golem::add_module(name = "qc_overview")
golem::add_module(name = "pca")
golem::add_module(name = "export")

usethis::use_r("qc_validate")
usethis::use_r("qc_metrics")
usethis::use_r("qc_dimred")
usethis::use_r("qc_outliers")


dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
dir.create("inst/report_templates", recursive = TRUE, showWarnings = FALSE)
dir.create("inst/app/www", recursive = TRUE, showWarnings = FALSE)


golem::run_dev()



usethis::use_package("readr")
usethis::use_package("DT")
usethis::use_package("dplyr")
usethis::use_package("tibble")
usethis::use_package("matrixStats")

usethis::use_package("ggplot2")
usethis::use_package("plotly")

usethis::use_package("htmlwidgets")
usethis::use_package("zip")
usethis::use_package("rlang", type = "Imports")



devtools::document()

renv::init()
renv::snapshot()


usethis::use_testthat()
usethis::use_package("testthat", "Suggests")

usethis::use_github_action("check-standard")
