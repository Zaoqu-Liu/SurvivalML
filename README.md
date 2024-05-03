## :bar_chart: Overview
## Efficient Discovery of Robust Prognostic Biomarkers and Signatures in Solid Tumors via SurvivalML

## :arrow_double_down: Preparation
**You can download the data files used in SurvivalML:**
https://www.synapse.org/#!Synapse:syn58922557

## :arrow_double_down: environment configuration
```R
##----------------------------------------------------------------------
## This script is designed for loading R packages in the main shiny app
##----------------------------------------------------------------------

#### Set packages resources ####
options(BioC_mirror="https://mirrors.tuna.tsinghua.edu.cn/bioconductor")
options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))

message("[++] It will cost a while for the first time.")

#### Load basic packages ####
if (!requireNamespace("pacman")) {
  install.packages("pacman")
}
library(pacman)

if (!requireNamespace("devtools")) {
  install.packages("devtools")
}
library(devtools)

#### Load necessary packages ####
## Packages from Github
## InteractiveComplexHeatmap and ComplexHeatmap packages MUST be installed from Github to use the latest version
if (!requireNamespace("ComplexHeatmap")) {
  message("[++] Installing ComplexHeatmap")
  tryCatch(
    devtools::install_github("jokergoo/ComplexHeatmap"),
    error = function(e) {
      message("[++] ERROR: R packages [ComplexHeatmap] from Github can NOT be installed.")
    }
  )
}
library(ComplexHeatmap)

if (!requireNamespace("summaryBox")) {
  message("[++] Installing summaryBox")
  tryCatch(
    devtools::install_github("deepanshu88/summaryBox"),
    error = function(e) {
      message("[++] ERROR: R packages [summaryBox] from Github can NOT be installed.")
    }
```
