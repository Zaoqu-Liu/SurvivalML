# this script render the UI of analysis page of module prognostic model


# set up with UI
ui.page_module_pm_immunomodulator <- function() {
  fluidPage(
    
    div(
      span(
        shiny::actionButton(
          inputId = "PrognosticModelPageSubmoduleImmunomodulatorSubmitBtn",
          label = "Start",
          icon = icon("rocket"),
          style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
        ),
        span(
          "Click the START button to analyze different type of immunomodulators.",
          style = "font-style:italic;color:#3b3b3b;font-size:80%;padding-top:10px;"
        )
      ), 
      style="box-shadow:2px 2px 5px 2px #ccc;padding:15px 0px 15px 10px;"
    ),
    
    
    div(
      useShinyjs(),
      
      tags$hr(),
      tags$br(),
      
      htmlOutput(
        outputId = "PrognosticModelPageSubmoduleImmunomodulatorHeatmapHtml"
      ),
      
      
      tags$br(),
      
      
      span(
        shiny::downloadButton(
          outputId = "PrognosticModelPageSubmoduleImmunomodulatorHeatmapDownloadPdfBtn",
          label = "Download PDF",
          icon = icon("file-pdf-o"),
          style = "color:#FF6B6B;background-color:#ffffff;",
        ),
        shiny::downloadButton(
          outputId = "PrognosticModelPageSubmoduleImmunomodulatorHeatmapDownloadPngBtn",
          label = "Download PNG",
          icon = icon("file-image-o"),
          style = "color:#FF6B6B;background-color:#ffffff;",
        )
      ),
      
      
      tags$br(),
      tags$hr(),
      
      ## download and detail correlation plots
      # title
      div(
        fluidRow(
          column(
            3,
            p("Select a cohort", 
              style = "font-weight:bold;font-size:110%;")
          ),
          column(
            4,
            p("Select an immunomodulator signature", 
              style = "font-weight:bold;font-size:110%;")
          ),
          column(
            3,
            p("Correlation method",
              style = "font-weight:bold;font-size:110%;")
          ),
          column(
            3 # show plot btn
          )
        ),
        
        
        # element
        fluidRow(
          
          column(
            3,
            selectInput(
              inputId = "PrognosticModelPageSubmoduleImmunomodulatorSubplotCohortSelection",
              width = "100%",
              label = NULL,
              choices = "",
              multiple = FALSE
            )
          ),
          
          column(
            4,
            selectInput(
              inputId = "PrognosticModelPageSubmoduleImmunomodulatorSubplotSignatureSelection",
              width = "100%",
              label = NULL,
              choices = "",
              multiple = FALSE
            )
          ),
          
          column(
            3,
            selectInput(
              inputId = "PrognosticModelPageSubmoduleImmunomodulatorSubplotCorMethodSelection",
              width = "100%",
              label = NULL,
              choices = c("spearman","pearson"),
              multiple = FALSE
            )
          ),
          
          column(
            2,
            shiny::actionButton(
              inputId = "PrognosticModelPageSubmoduleImmunomodulatorSubplotSubmitBtn",
              label = "Show Plot",
              icon = icon("area-chart"),
              style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
            )
          )
        ),
        style="box-shadow:2px 2px 5px 2px #ccc;padding:15px 0px 15px 10px;"
      ),
      
      tags$br(),
      
      # subplot output
      # sub plot
      # put plot outside the div, show it in the first place, some thing wrong with dropdown menu
      plotOutput(
        outputId = "PrognosticModelPageSubmoduleImmunomodulatorSubplot",
        width = "350px",
        height = "400px"
      ),
      
      div(
        # download btn
        tags$br(),
        
        shiny::downloadButton(
          outputId = "PrognosticModelPageSubmoduleImmunomodulatorSubplotDownloadPdfBtn",
          label = "Download PDF",
          icon = icon("file-pdf-o"),
          style = "color:#FF6B6B;background-color:#ffffff;",
        ),
        shiny::downloadButton(
          outputId = "PrognosticModelPageSubmoduleImmunomodulatorSubplotDownloadPngBtn",
          label = "Download PNG",
          icon = icon("file-image-o"),
          style = "color:#FF6B6B;background-color:#ffffff;",
        ),
        
        id = "PrognosticModelPageSubmoduleImmunomodulatorSubplotDiv",
        class = "hidden-element"
      ),
      
      tags$br(),
      
      
      
      # id and class
      id = "PrognosticModelPageSubmoduleImmunomodulatorHeatmapDiv",
      
      class = "hidden-element"
      
    )
  )
}
