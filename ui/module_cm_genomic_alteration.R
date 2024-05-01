# this script render the UI of analysis page of module prognostic model


# set up with UI
ui.page_module_cm_genomic_alteration <- function() {
  fluidPage(
    div(
      span(
        shiny::actionButton(
          inputId = "ConsensusModelPageSubmoduleGenomicAlterationSubmitBtn",
          label = "Start",
          icon = icon("rocket"),
          style = "background-color:#EAC248; border-color:#EAC248;color:#ffffff;"
        ),
        span(
          "Click the START button to explore SNVs and CNVs correlation with the target gene expression.",
          style = "font-style:italic;color:#3b3b3b;font-size:80%"
        )
      ), 
      class = "conf-box"
    ),
    
    
    div(
      useShinyjs(),
      
      tags$hr(),
      tags$br(),
      
      div(
        plotOutput(
          outputId = "ConsensusModelPageSubmoduleGenomicAlterationHeatmap",
          height = "800px",
          width = "640px"
        ),
        
        style = "height:400px; overflow-y:scroll;overflow-x:auto;"
      ),
      
      tags$br(),
      
      span(
        shiny::downloadButton(
          outputId = "ConsensusModelPageSubmoduleGenomicAlterationHeatmapDownloadPdfBtn",
          label = "Download PDF",
          icon = icon("file-pdf-o"),
          style = "color:#EAC248;background-color:#ffffff;",
        ),
        shiny::downloadButton(
          outputId = "ConsensusModelPageSubmoduleGenomicAlterationHeatmapDownloadPngBtn",
          label = "Download PNG",
          icon = icon("file-image-o"),
          style = "color:#EAC248;background-color:#ffffff;",
        )
      ),
      
      tags$br(),
      
      # id and class
      id = "ConsensusModelPageSubmoduleGenomicAlterationHeatmapDiv",
      
      class = "hidden-element"
      
    )
  )
}
