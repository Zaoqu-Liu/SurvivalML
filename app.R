
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
