##----------------------------------------------------------------------
## This script is designed for loading R packages in the main shiny app
##----------------------------------------------------------------------

#### Set packages resources ####
options(BioC_mirror="https://mirrors.tuna.tsinghua.edu.cn/bioconductor")
options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))  

### Load basic packages ####
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
  )
}
library(summaryBox)

if (!requireNamespace("bsplus")) {
  message("[++] Installing bsplus")
  tryCatch(
    devtools::install_github("ijlyttle/bsplus"),
    error = function(e) {
      message("[++] ERROR: R packages [bsplus] from Github can NOT be installed.")
    }
  )
}
library(bsplus)

# Packages from cran and bioconductor
pkgs.to.check <- c(
  "shiny",
  "shinyjs",
  "bslib",
  "waiter",
  "openxlsx",
  "plotly",
  "ggplot2",
  "stringr",
  "slickR",
  "shinyvalidate",
  "markdown",
  "bs4Dash",
  "DT",
  "shinyWidgets",
  "readr",
  "GetoptLong",
  "stringr",
  "heatmaply",
  "gplots",
  "shinyFeedback",
  "patchwork",
  "cowplot",
  "openxlsx",
  "ggsci",
  "tidyverse",
  "RColorBrewer",
  "ggpubr",
  "flexdashboard",
  "shinypop",
  "BiocParallel"
)

for (pkg.name in pkgs.to.check) {
  if (!requireNamespace(pkg.name)) {
    message(paste("[++] Installing", pkg.name))
    tryCatch(
      pacman::p_install(pkg.name,character.only = T),
      error = function(e) {
        message(paste("[++] ERROR: R packages ["), pkg.name, "] can NOT be installed.")
      }
    )
  }
  library(pkg.name,character.only = T)
}

# packages to install
devtools::install_github("Github-Yilei/ggcor")
pacman::p_load("circlize")
pacman::p_load("ggridges")
pacman::p_load("RobustRankAggreg")
pacman::p_load("ggpubr")
pacman::p_load("org.Hs.eg.db")
pacman::p_load("GSVA")
pacman::p_load("survminer")
remotes::install_github("YuLab-SMU/clusterProfiler")
pacman::p_load("pROC")
pacman::p_load("WGCNA")
pacman::p_load("randomForestSRC")
pacman::p_load("glmnet")
pacman::p_load("gbm")
pacman::p_load("survivalsvm")
pacman::p_load("plsRcox")
pacman::p_load("RSpectra")
pacman::p_load("rARPACK")
pacman::p_load("mixOmics")
pacman::p_load("survcomp")
pacman::p_load("superpc")
devtools::install_github("binderh/CoxBoost")
pacman::p_load("timeROC")
pacman::p_load("rms")
pacman::p_load("snowfall")
pacman::p_load("flexdashboard")
remotes::install_github("dreamRs/shinypop")
