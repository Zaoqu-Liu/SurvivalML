# this script render the UI of analysis page of 
# module prognostic model: prognostic analysis

# set up with UI
ui.page_module_pm_prognostic_analysis <- function() {
  fluidPage(
    
    div(
      bs4Dash::box(
        
        #### survival analysis configurations ####
        # title
        fluidRow(
          column(
            4,
            p("Group cutoff method",style="font-size:110%;font-weight:bold;")
          ),
          column(
            6,
            div(
              p("Define percentage of cutoff value",style="font-size:110%;font-weight:bold;"),
              
              id = "PrognosticModelPageSubmodulePrognosticAnalysisCutoffValuePercentSelectionTitleDiv",
              class = "hidden-element"
            )
          ),
          column(
            2
          )
        ),
        # elements
        fluidRow(
          useShinyjs(),
          
          column(
            4,
            selectInput(
              inputId = "PrognosticModelPageSubmodulePrognosticAnalysisGroupSplitMethodSelection",
              label = NULL,
              width = "100%",
              choices = c('median','mean','quantile','optimal','custom')
            )
          ),
          column(
            6,
            div(
              sliderInput(
                inputId = "PrognosticModelPageSubmodulePrognosticAnalysisCutoffValuePercentSelection",
                label = NULL,
                width = "100%",
                min = 0.1,
                max = 0.9,
                value = 0.3,
                step = 0.05,
                ticks = T,
                animate = T
              ),
              
              id = "PrognosticModelPageSubmodulePrognosticAnalysisCutoffValuePercentSelectionDiv",
              class = "hidden-element"
            )
          ), 
          column(
            2
          )
        ),
        
        
        #### user input, plot configuration ####
        # title
        fluidRow(
          column(
            4,
            p("Color palette",style="font-size:110%;font-weight:bold;")
          ),
          column(
            6,
            p("Color transparency",style="font-size:110%;font-weight:bold;")
          ),
          column(
            2
          )
        ),
        
        ## input elements
        fluidRow(
          
          # plot settings
          column(
            4,
            selectInput(
              inputId = "PrognosticModelPageSubmodulePrognosticAnalysisPlotColorSelection",
              label = NULL,
              choices = c("npg","nejm","jco","d3","lancet","jama"),
              width = "100%"
            )
          ),
          
          # transparency
          column(
            6,
            sliderInput(
              inputId = "PrognosticModelPageSubmodulePrognosticAnalysisPlotTransparencySelection",
              label = NULL,
              min = 0.2,
              max = 1,
              value = 0.8,
              step = 0.1,
              ticks = TRUE,
              animate = TRUE,
              width = "100%"
            )
          ),
          
          # space
          column(
            2,
            
            shiny::actionButton(
              inputId = "PrognosticModelPageSubmodulePrognosticAnalysisSubmitBtn",
              label = "Start",
              icon = icon("rocket"),
              style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
            )
          )
        ),
        
        id = "HomePageLastInfoCard", 
        title = "Analysis settings", 
        icon = icon("cogs"), 
        collapsible = FALSE, 
        width = 12,
        closable = FALSE, 
        maximizable = FALSE
      ), 
      
      style = "box-shadow:2px 2px 5px 2px #ccc;"
    ),
    
    
    
    ## plots
    div(
      #space
      tags$hr(),
      tags$br(),
      
      tabsetPanel(
        id = "PrognosticModelPageSubmodulePrognostsicAnalysisPanel",
        
        tabPanel(
          title = "Cox Regression",
          value = "PagePrognosticModelPrognosticAnalysisSubmodelCoxRegressionPanel",
          
          tags$br(),
          
          ## multiple plots
          htmlOutput(
            outputId = "PrognosticModelPageSubmodulePrognosticAnalysisResultCoxPlotHtml"
          ),
          
          tags$br(),
          
          ## download
          fluidRow(
            span(
              shiny::downloadButton(
                outputId = "PrognosticModelPageSubmodulePrognosticAnalysisResultCoxPlotDownloadPdfBtn",
                label = "Download PDF",
                icon = icon("file-pdf-o"),
                style = "color:#FF6B6B;background-color:#ffffff;"
              ),
              shiny::downloadButton(
                outputId = "PrognosticModelPageSubmodulePrognosticAnalysisResultCoxPlotDownloadPngBtn",
                label = "Download PNG",
                icon = icon("file-image-o"),
                style = "color:#FF6B6B;background-color:#ffffff;"
              )
            )
          )
          
        ),
        tabPanel(
          title = "Kaplan-Meier",
          value = "PagePrognosticModelPrognosticAnalysisSubmodelKaplanMeierPanel",
          
          tags$br(),
          
          
          ## multiple plots
          htmlOutput(
            outputId = "PrognosticModelPageSubmodulePrognosticAnalysisResultKMPlotHtml"
          ),
          
          tags$br(),
          
          ## download
          fluidRow(
            span(
              shiny::downloadButton(
                outputId = "PrognosticModelPageSubmodulePrognosticAnalysisResultKMPlotDownloadPdfBtn",
                label = "Download PDF",
                icon = icon("file-pdf-o"),
                style = "color:#FF6B6B;background-color:#ffffff;"
              ),
              shiny::downloadButton(
                outputId = "PrognosticModelPageSubmodulePrognosticAnalysisResultKMPlotDownloadPngBtn",
                label = "Download PNG",
                icon = icon("file-image-o"),
                style = "color:#FF6B6B;background-color:#ffffff;"
              )
            )
          )
          
        )
        
      ),
      
      tags$br(),
      
      # div id
      id = "PrognosticModelPageSubmodulePrognosticAnalysisResultPlotDiv",
      
      # hide
      class = "hidden-element"
    )
    
    
  )
}
