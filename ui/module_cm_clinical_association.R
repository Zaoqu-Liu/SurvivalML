# this script render the UI of analysis page of 
# module consensus model, analysis: clinical association

# set up with UI
ui.page_module_cm_clinical_association <- function() {
  fluidPage(
    
    div(
      bs4Dash::box(
        #### user input, plot configuration
        # title
        fluidRow(
          # column(
          #   3,
          #   p("Select a clinical trait",style="font-size:110%;font-weight:bold;")
          # ),
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
              inputId = "ConsensusModelPageSubmoduleClinlcalAssociationPlotColorSelection",
              label = NULL,
              choices = c("npg","nejm","jco","d3","lancet","jama"),
              width = "100%"
            )
          ),
          
          # transparency
          column(
            6,
            sliderInput(
              inputId = "ConsensusModelPageSubmoduleClinlcalAssociationPlotTransparencySelection",
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
          
          # start btn
          column(
            2,
            # shinyWidgets::actionBttn(
            #   inputId = "ConsensusModelPageSubmoduleClinlcalAssociationSubmitBtn",
            #   label = "Start",
            #   icon = icon("check"),
            #   color = "success",
            #   style = "unite",
            #   block = TRUE,
            #   size = "sm"
            # )
            shiny::actionButton(
              inputId = "ConsensusModelPageSubmoduleClinlcalAssociationSubmitBtn",
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
      
      
      # ## multiple plots
      # htmlOutput(
      #   outputId = "ConsensusModelPageSubmoduleClinlcalAssociationResultPlotHtml"
      # ),
      
      # INFO
      fluidRow(
        column(
          4,
          p("Select a clinical trait",style="font-size:110%;font-weight:bold;"),
        ),
        column(
          8
        )
      ),
      
      # plot selection, choose clinical traits
      fluidRow(
        column(
          4,
          # clinical information
          selectInput(
            inputId = "ConsensusModelPageSubmoduleClinlcalAssociationTargetSelection",
            label = NULL,
            choices = "",
            multiple = FALSE,
            width = "100%"
          )
        ),
        
        # space
        column(
          8
        )
        
      ),
      
      
      # plot output
      htmlOutput(
        outputId = "ConsensusModelPageSubmoduleClinlcalAssociationResultPlotHtmlOutput"
      ),
      
      
      ## download
      # downloadBttn(
      #   outputId = "ConsensusModelPageSubmoduleClinlcalAssociationResultPlotDownloadBtn",
      #   label = "Download",
      #   style = "pill",
      #   color = "success",
      #   size = "sm",
      #   block = FALSE,
      #   icon = icon("cloud-download")
      # ),
      
      tags$br(),
      
      span(
        # save pdf
        shiny::downloadButton(
          outputId = "ConsensusModelPageSubmoduleClinlcalAssociationResultPlotDownloadPdfBtn",
          label = "Download PDF",
          icon = icon("file-pdf-o"),
          style = "color:#EAC428;background-color:#ffffff;"
        ),
        # save png
        shiny::downloadButton(
          outputId = "ConsensusModelPageSubmoduleClinlcalAssociationResultPlotDownloadPngBtn",
          label = "Download PNG",
          icon = icon("file-image-o"),
          style = "color:#EAC428;background-color:#ffffff;"
        )
      ),
      
      # div id
      id = "ConsensusModelPageSubmoduleClinlcalAssociationResultPlotDiv",
      
      # hide
      class = "hidden-element"
    )
    
    
  )
}
