# this script render the UI of analysis page of module prognostic model


# set up with UI
ui.page_module_pm_genomic_alteration <- function() {
  fluidPage(
    div(
      span(
        shiny::actionButton(
          inputId = "PrognosticModelPageSubmoduleGenomicAlterationSubmitBtn",
          label = "Start",
          icon = icon("rocket"),
          style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
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
          outputId = "PrognosticModelPageSubmoduleGenomicAlterationHeatmap",
          height = "800px",
          width = "640px"
        ),
        
        style = "height:400px; overflow-y:scroll;overflow-x:auto;"
      ),
      
      tags$br(),
      
      span(
        shiny::downloadButton(
          outputId = "PrognosticModelPageSubmoduleGenomicAlterationHeatmapDownloadPdfBtn",
          label = "Download PDF",
          icon = icon("file-pdf-o"),
          style = "color:#FF6B6B;background-color:#ffffff;",
        ),
        shiny::downloadButton(
          outputId = "PrognosticModelPageSubmoduleGenomicAlterationHeatmapDownloadPngBtn",
          label = "Download PNG",
          icon = icon("file-image-o"),
          style = "color:#FF6B6B;background-color:#ffffff;",
        )
      ),
      
      tags$br(),
      
      # id and class
      id = "PrognosticModelPageSubmoduleGenomicAlterationHeatmapDiv",
      
      class = "hidden-element"
      
    )
  )
}
