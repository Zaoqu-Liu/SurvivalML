# this script render the UI of analysis page of module prognostic model

# set up with UI
ui.page_module_cm_candidate_agents <- function() {
  fluidPage(
    
    div(
      bs4Dash::bs4Card(
        #### title and elements ####
        # title
        fluidRow(
          column(
            4,
            p("Maximum number of agents to show",
              style = "font-weight:bold; font-size:110%;")
          ),
          column(
            3,
            p("Select a database",
              style = "font-weight:bold; font-size:110%;")
          ),
          column(
            5
          )
        ),
        
        # elements
        fluidRow(
          useShinyFeedback(),
          
          column(
            4,
            
            shiny::numericInput(
              inputId = "ConsensusModelPageSubmoduleCandidateAgentsMaxNumberSelection",
              label = NULL,
              value = 30,
              min = 10,
              max = 100,
              step = 5,
              width = "100%"
            )
          ),
          column(
            3,
            
            selectInput(
              inputId = "ConsensusModelPageSubmoduleCandidateAgentsDatabaseSelection",
              width = "100%",
              label = NULL,
              choices = c("GDSC1","GDSC2","CTRP","PRISM"),
              multiple = FALSE
            )
          ),
          column(
            5,
            
            span(
              shiny::actionButton(
                inputId = "ConsensusModelPageSubmoduleCandidateAgentsSubmitBtn",
                label = "Start",
                icon = icon("rocket"),
                style = "background-color:#EAC428; border-color:#EAC428;color:#ffffff;"
              ),
              span("Analyze different type of candidate agents.",
                   style = "font-style:italic;color:#3b3b3b;font-size:80%;")
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
    
    
    div(
      useShinyjs(),
      
      tags$hr(),
      tags$br(),
      
      # div(
      #   plotlyOutput(
      #     outputId = "ConsensusModelPageSubmoduleCandidateAgentsHeatmap",
      #     height = "1500px"
      #   ),
      #   
      #   style = "height:600px; overflow-y:scroll;"
      # ),
      
      # plot height unknown, define it in the backend
      htmlOutput(
        outputId = "ConsensusModelPageSubmoduleCandidateAgentsHeatmapHtml"
      ),
      
      tags$br(),
      
      span(
        shiny::downloadButton(
          outputId = "ConsensusModelPageSubmoduleCandidateAgentsHeatmapDownloadPdfBtn",
          label = "Download PDF",
          icon = icon("file-pdf-o"),
          style = "color:#EAC428;background-color:#ffffff;",
        ),
        shiny::downloadButton(
          outputId = "ConsensusModelPageSubmoduleCandidateAgentsHeatmapDownloadPngBtn",
          label = "Download PNG",
          icon = icon("file-image-o"),
          style = "color:#EAC428;background-color:#ffffff;",
        ),
        shiny::downloadButton(
          outputId = "ConsensusModelPageSubmoduleCandidateAgentsHeatmapDownloadTableBtn",
          label = "Download Table",
          icon = icon("Table"),
          style = "color:#EAC428;background-color:#ffffff;",
        )
      ),
      
      
      tags$br(),
      tags$hr(),
      
      ## download and detail corelation plots
      # title
      fluidRow(
        column(
          3,
          p("Select a cohort", 
            style = "font-weight:bold;font-size:110%;")
        ),
        column(
          3,
          p("Select an agent", 
            style = "font-weight:bold;font-size:110%;")
        ),
        column(
          3,
          p("Correlation method",
            style = "font-weight:bold; font-size:110%;")
        ),
        column(
          3 # show plot btn
        )
      ),
      
      # element
      fluidRow(
        
        # cohort
        column(
          3,
          # selectInput(
          #   inputId = "ConsensusModelPageSubmoduleCandidateAgentsSubplotCohortSelection",
          #   width = "100%",
          #   label = NULL,
          #   choices = "",
          #   multiple = FALSE
          # )
          
          # use picker instead of selectInput, use search and group utility
          shinyWidgets::pickerInput(
            inputId = "ConsensusModelPageSubmoduleCandidateAgentsSubplotCohortSelection",
            label = NULL,
            choices = c(""),
            options = list(
              `live-search` = TRUE,
              size = 5,
              style = "btn-default"
            )
          )
        ),
        
        # drug
        column(
          3,
          
          # selectInput(
          #   inputId = "ConsensusModelPageSubmoduleCandidateAgentsSubplotDrugSelection",
          #   width = "100%",
          #   label = NULL,
          #   choices = "",
          #   multiple = FALSE
          # )
          
          # use picker instead of selectInput, use search and group utility
          shinyWidgets::pickerInput(
            inputId = "ConsensusModelPageSubmoduleCandidateAgentsSubplotDrugSelection",
            label = NULL,
            choices = c(""),
            options = list(
              `live-search` = TRUE,
              size = 5,
              style = "btn-default"
            )
          )
        ),
        
        # correlation method
        column(
          3,
          selectInput(
            inputId = "ConsensusModelPageSubmoduleCandidateAgentsSubplotCorMethodSelection",
            width = "100%",
            label = NULL,
            choices = c("spearman","pearson"),
            multiple = FALSE
          )
        ),
        
        # submit btn
        column(
          3,
          shiny::actionButton(
            inputId = "ConsensusModelPageSubmoduleCandidateAgentsSubplotSubmitBtn",
            label = "Show Plot",
            icon = icon("area-chart"),
            style = "background-color:#EAC428; border-color:#EAC428;color:#ffffff;"
          )
        )
      ),
      
      # subplot output
      # sub plot
      # put plot outside the div, show it in the first place, some thing wrong with dropdown menu
      plotOutput(
        outputId = "ConsensusModelPageSubmoduleCandidateAgentsSubplot",
        width = "350px",
        height = "400px"
      ),
      div(
        # download btn
        tags$br(),
        
        span(
          shiny::downloadButton(
            outputId = "ConsensusModelPageSubmoduleCandidateAgentsSubplotDownloadPdfBtn",
            label = "Download PDF",
            icon = icon("file-pdf-o"),
            style = "color:#EAC428;background-color:#ffffff;",
          ),
          shiny::downloadButton(
            outputId = "ConsensusModelPageSubmoduleCandidateAgentsSubplotDownloadPngBtn",
            label = "Download PNG",
            icon = icon("file-image-o"),
            style = "color:#EAC428;background-color:#ffffff;",
          )
        ),
        
        id = "ConsensusModelPageSubmoduleCandidateAgentsSubplotDiv",
        class = "hidden-element"
      ),
      
      tags$br(),
      
      
      
      # id and class
      id = "ConsensusModelPageSubmoduleCandidateAgentsHeatmapDiv",
      
      class = "hidden-element"
      
    )
  )
}
