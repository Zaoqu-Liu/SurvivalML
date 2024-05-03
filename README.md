## :bar_chart: Overview
## Efficient Discovery of Robust Prognostic Biomarkers and Signatures in Solid Tumors via SurvivalML

## :arrow_double_down: Preparation
**You can download the data files used in SurvivalML:**
https://www.synapse.org/#!Synapse:syn58922557
<img src="man/1.png" width="80%" />

**After you download, move the db folder to the SurvivalML project.**

## :arrow_double_down: Environment configuration
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
  "gplots",
  "shinyFeedback",
  "patchwork",
  "cowplot",
  "openxlsx",
  "ggsci",
  "tidyverse",
  "RColorBrewer",
  "ggpubr",
  "BiocParallel",
  "shinypop",
  'remotes'

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

# # packages to install
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
pacman::p_load("dcurves")
pacman::p_load("snowfall")
```

## :beginner: Running App
```R

# -----Packages-----
message("[+] Checking depedencies...")
source("./lib/load_packages.R")

# -----Global functions-----
source("./lib/global.R")
source("./lib/shinyLink.R")

# -----Setting up-----
options(shiny.maxRequestSize=1024*1024^2)
message("[+] Starting...")



# -----UI part-----
## ------UI files-----
# read main page
pages_file <- dir("./ui", pattern = "\\.R$", full.names = TRUE)
sapply(pages_file, function(x, y) source(x, local = y), y = environment())

## -----Set up UI-----
ui <- shinyUI(
  fluidPage(
    tags$head(
      tags$title("SurvivalML"),
      # includeHTML("google-analytics.html"),
      tags$link(rel = "stylesheet", type = "text/css", href = "./static/css/mystyle.css"),
      tags$link(rel = "shortcut icon", href = "logo.ico"),
      tags$script(src = "./static/js/shinyLink.js"), # see https://github.com/davidruvolo51/shinyAppTutorials for more details
      tags$script(src = "./static/js/baidu.js")
	),
    
    shinyjs::useShinyjs(),
    
    # waiting time
    use_waiter(),
    waiter_on_busy(html = spin_3k(), color = transparent(0.7)),
    
    #navbar
    bslib::page_navbar( # use bslib to place tabs to the right, see https://github.com/rstudio/bslib/pull/319
      id = "navbar",
      bg = "#296D7F",
      title = div(
        tags$a(img(src = "logo_short_small_white.png", height = 50, width = 200, style="padding-top:0px;padding-bottom:0px;"),href="")
      ),
      
      ##### web pages ####
      ## 自定义css控制nav字体大小
      !!!list(
        
        # nav spacer moves all tabs to the right
        nav_spacer(),
        
        # home page
        nav("Home",ui.page_home(),icon = icon("home")),
        
        # dataset page
        nav("Dataset", ui.page_dataset(),icon = icon("database")),
        
        # help page, four sub pages
        nav_menu("Help",
                 icon = icon("question-circle"),
                 nav("Term list",ui.page_help_termlist()),
                 ),
        nav("Contact",ui.page_contact(),icon = icon("paper-plane")),
        nav(NULL, ui.page_about()),
        nav(title = NULL, value = "PrognosticModel",ui.page_module_prognostic_model()),
        nav(title = NULL, value = "ConsensusModel",ui.page_module_consensus_model()),
        
        nav(title = NULL,value="PrivacyPolicy",ui.page_help_privacy_policy()),
        nav(title = NULL,value = "TermsAndConditions",ui.page_help_terms_and_conditions())
      ),
      footer = ui.footer(),
      collapsible = TRUE,
      theme = bs_theme(bootswatch = "yeti")
    )
  )
)
  

# -----Server part-----
server <- function(input, output, session) {
  # pages in nav bar
  source("./server/home.R", local = TRUE)
  source("./server/contact.R", local = TRUE)
  source("./server/dataset.R", local = TRUE)
  source("./server/help.R", local = TRUE)
  source("./server/module_prognostic_model.R", local = TRUE)
  source("./server/module_consensus_model.R", local = TRUE)


  # prognostic model submodules
  source("./server/module_pm_clinical_association.R", local = TRUE)
  source("./server/module_pm_prognostic_analysis.R", local = TRUE)
  source("./server/module_pm_functional_analysis.R", local = TRUE)
  source("./server/module_pm_cell_infiltration.R", local = TRUE)
  source("./server/module_pm_immunomodulator.R", local = TRUE)
  source("./server/module_pm_immunotherapy.R", local = TRUE)
  source("./server/module_pm_candidate_agents.R", local = TRUE)
  source("./server/module_pm_genomic_alteration.R", local = TRUE)

  # consensus model submodules
  source("./server/module_cm_clinical_association.R", local = TRUE)
  source("./server/module_cm_prognostic_analysis.R", local = TRUE)
  source("./server/module_cm_functional_analysis.R", local = TRUE)
  source("./server/module_cm_cell_infiltration.R", local = TRUE)
  source("./server/module_cm_immunomodulator.R", local = TRUE)
  source("./server/module_cm_immunotherapy.R", local = TRUE)
  source("./server/module_cm_candidate_agents.R", local = TRUE)
  source("./server/module_cm_genomic_alteration.R", local = TRUE)
  
  # success info
  message("[+] Shiny app run successfully! Enjoy it!\n") 
}

# ----- Main wrapper -----
shiny::shinyApp(
  ui = ui,
  server = server
)
```
<img src="man/2.png" width="80%" />

## :paperclip: Start the exploration
<img src="man/3.png" width="100%" />













