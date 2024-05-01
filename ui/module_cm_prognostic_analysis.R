# this script render the UI of analysis page of 
# module prognostic model: prognostic analysis

# set up with UI
ui.page_module_cm_prognostic_analysis <- function() {
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
              
              id = "ConsensusModelPageSubmodulePrognosticAnalysisCutoffValuePercentSelectionTitleDiv",
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
              inputId = "ConsensusModelPageSubmodulePrognosticAnalysisGroupSplitMethodSelection",
              label = NULL,
              width = "100%",
              choices = c('median','mean','quantile','optimal','custom')
            )
          ),
          column(
            6,
            div(
              sliderInput(
                inputId = "ConsensusModelPageSubmodulePrognosticAnalysisCutoffValuePercentSelection",
                label = NULL,
                width = "100%",
                min = 0.1,
                max = 0.9,
                value = 0.3,
                step = 0.05,
                ticks = T,
                animate = T
              ),
              
              id = "ConsensusModelPageSubmodulePrognosticAnalysisCutoffValuePercentSelectionDiv",
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
              inputId = "ConsensusModelPageSubmodulePrognosticAnalysisPlotColorSelection",
              label = NULL,
              choices = c("npg","nejm","jco","d3","lancet","jama"),
              width = "100%"
            )
          ),
          
          # transparency
          column(
            6,
            sliderInput(
              inputId = "ConsensusModelPageSubmodulePrognosticAnalysisPlotTransparencySelection",
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
              inputId = "ConsensusModelPageSubmodulePrognosticAnalysisSubmitBtn",
              label = "Start",
              icon = icon("rocket"),
              style = "background-color:#EAC428; border-color:#EAC428;color:#ffffff;"
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
        id = "ConsensusModelPageSubmodulePrognostsicAnalysisPanel",
        
        tabPanel(
          title = "Cox Regression",
          value = "PageConsensusModelPrognosticAnalysisSubmodelCoxRegressionPanel",
          
          tags$br(),
          
          ## multiple plots
          htmlOutput(
            outputId = "ConsensusModelPageSubmodulePrognosticAnalysisResultCoxPlotHtml"
          ),
          
          tags$br(),
          
          ## download
          fluidRow(
            span(
              shiny::downloadButton(
                outputId = "ConsensusModelPageSubmodulePrognosticAnalysisResultCoxPlotDownloadPdfBtn",
                label = "Download PDF",
                icon = icon("file-pdf-o"),
                style = "color:#EAC428;background-color:#ffffff;"
              ),
              shiny::downloadButton(
                outputId = "ConsensusModelPageSubmodulePrognosticAnalysisResultCoxPlotDownloadPngBtn",
                label = "Download PNG",
                icon = icon("file-image-o"),
                style = "color:#EAC428;background-color:#ffffff;"
              )
            )
          )
          
        ),
        tabPanel(
          title = "Kaplan-Meier",
          value = "PageConsensusModelPrognosticAnalysisSubmodelKaplanMeierPanel",
          
          tags$br(),
          
          
          ## multiple plots
          htmlOutput(
            outputId = "ConsensusModelPageSubmodulePrognosticAnalysisResultKMPlotHtml"
          ),
          
          tags$br(),
          
          ## download
          fluidRow(
            span(
              shiny::downloadButton(
                outputId = "ConsensusModelPageSubmodulePrognosticAnalysisResultKMPlotDownloadPdfBtn",
                label = "Download PDF",
                icon = icon("file-pdf-o"),
                style = "color:#EAC428;background-color:#ffffff;"
              ),
              shiny::downloadButton(
                outputId = "ConsensusModelPageSubmodulePrognosticAnalysisResultKMPlotDownloadPngBtn",
                label = "Download PNG",
                icon = icon("file-image-o"),
                style = "color:#EAC428;background-color:#ffffff;"
              )
            )
          )
          
        )
        
      ),
      
      tags$br(),
      
      # div id
      id = "ConsensusModelPageSubmodulePrognosticAnalysisResultPlotDiv",
      
      # hide
      class = "hidden-element"
    )
    
    
  )
}
