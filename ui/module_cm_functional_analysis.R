# this script render the UI of analysis page of module consensus model


# set up with UI
ui.page_module_cm_functional_analysis <- function() {
  fluidPage(
    column(
      12,
      
      tabsetPanel(
        id = "ConsensusModelPageSubmoduleFunctionalAnalysisTopTabsetPanel",
        
        # Over Representation 
        tabPanel(
          title = "Over Representation Analysis",
          value = "OverRepresentationAnalysis",
          
          tags$br(),
          
          div(
            bs4Dash::bs4Card(
              
              ## user input for go/kegg
              # element title
              fluidRow(
                
                # number of genes
                column(
                  4,
                  p("Number of genes to analyze",
                    style="font-size:110%;font-weight:bold;")
                ),
                
                # cutoff of significant go terms
                column(
                  2,
                  p("Q-value cutoff",
                    style="font-size:110%;font-weight:bold;"),
                ),
                
                # # go category
                # column(
                #   4,
                #   p("Gene ontology category",
                #     style="font-size:110%;font-weight:bold;"),
                # ),
                
                # go plot width
                column(
                  2,
                  p("GO-plot width",
                    style="font-size:110%;font-weight:bold;")
                ),
                
                # kegg plot width
                column(
                  2,
                  p("KEGG-plot width",
                    style="font-size:110%;font-weight:bold;")
                ),
                
                # start btn
                column(
                  2
                )
              ),
              
              # element
              fluidRow(
                
                # number of genes
                column(
                  4,
                  numericInput(
                    inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraGeneNumInput",
                    min = 50,
                    max = 1000,
                    label = NULL,
                    width = "100%",
                    step = 10,
                    value = 500
                  )
                ),
                
                # q value cutoff
                column(
                  2,
                  shiny::selectizeInput(
                    inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraQvalueInput",
                    label = NULL,
                    choices = c("0.05","0.001","0.01","0.1"),
                    options = list(create = TRUE),
                    width = "100%"
                  )
                ),
                
                # #  go category
                # column(
                #   4,
                #   shiny::selectInput(
                #     inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraGoCategoryInput",
                #     label = NULL,
                #     choices = c("BP","MF","CC"),
                #     selected = "BP",
                #     multiple = FALSE,
                #     width = "100%"
                #   )
                # ), 
                
                # go plot width
                column(
                  2,
                  numericInput(
                    inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraGoPlotWidthInput",
                    label = NULL,
                    value = 0.4,
                    min = 0.2,
                    max = 0.8,
                    step = 0.1
                  )
                ),
                
                # kegg plot width
                column(
                  2,
                  numericInput(
                    inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraKeggPlotWidthInput",
                    label = NULL,
                    value = 0.4,
                    min = 0.2,
                    max = 0.8,
                    step = 0.1
                  )
                ),
                
                # start btn
                column(
                  2,
                  
                  shiny::actionButton(
                    inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraSubmitBtn",
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
          
          
          
          
          ## result tabset panel
          div(
            useShinyjs(),
            
            tags$hr(),
            tags$br(),
            
            tabsetPanel(
              id = "ConsensusModelPageSubmoduleFunctionalAnalysisORATabsetPanel",    
              # type = "pill",
              # vertical = TRUE,
              # side = "left",
              
              # GO
              tabPanel(
                title = "GO",
                value = "ORA_GO",
                
                tags$br(),
                
                # plot output
                htmlOutput(
                  outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraGOPlotHtml"
                ),
                
                # information to show
                tags$br(),
                HTML("<span style=\"font-size:80%;font-style:italic;color:#3b3b3b;\"> <i class=\"fa fa-info-circle\"></i> Red represents positive pathways and blue represents negative pathways. </span>"),
                
                tags$br(),
                ## download
                span(
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraGOPlotDownloadPdfBtn",
                    label = "Download PDF",
                    icon = icon("file-pdf-o"),
                    style = "color:#EAC428;background-color:#ffffff;"
                  ),
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraGOPlotDownloadPngBtn",
                    label = "Download PNG",
                    icon = icon("file-image-o"),
                    style = "color:#EAC428;background-color:#ffffff;"
                  ),
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraGOPlotDownloadTableBtn",
                    label = "Download Table",
                    icon = icon("table"),
                    style = "color:#EAC428;background-color:#ffffff;"
                  )
                  
                ),
                
                tags$br()
                
              ),
              
              # KEGG
              tabPanel(
                title = "KEGG",
                value = "ORA_KEGG",
                
                tags$br(),
                
                # plot output
                htmlOutput(
                  outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraKEGGPlotHtml"
                ),
                
                # information to show
                tags$br(),
                HTML("<span style=\"font-size:80%;font-style:italic;color:#3b3b3b;\"> <i class=\"fa fa-info-circle\"></i> Red represents positive pathways and blue represents negative pathways. </span>"),
                
                tags$br(),
                
                ## download
                span(
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraKEGGPlotDownloadPdfBtn",
                    label = "Download PDF",
                    icon = icon("file-pdf-o"),
                    style = "color:#EAC428;background-color:#ffffff;"
                  ),
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraKEGGPlotDownloadPngBtn",
                    label = "Download PNG",
                    icon = icon("file-image-o"),
                    style = "color:#EAC428;background-color:#ffffff;"
                  ),
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraKEGGPlotDownloadTableBtn",
                    label = "Download Table",
                    icon = icon("table"),
                    style = "color:#EAC428;background-color:#ffffff;"
                  )
                ),
                
                tags$br()
                
              )
            ),
            
            id = "ConsensusModelPageSubmoduleFunctionalAnalysisORATabsetPanelDiv",
            class = "hidden-element"
          )
        ),
        
        # GSEA
        tabPanel(
          title = "Gene Set Enrichment Analysis",
          value = "GeneSetEnrichmentAnalysis",
          
          tags$br(),
          
          # start btn
          div(
            span(
              shiny::actionButton(
                inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaSubmitBtn",
                label = "Start",
                icon = icon("rocket"),
                style = "background-color:#EAC428; border-color:#EAC428;color:#ffffff;"
              ),
              
              span(
                "Before you click the START button, be aware of that there are several calculation steps to go, and it might take a few minutes. Please be patient.",
                style = "font-style:italic;color:#3b3b3b;font-size:80%;padding-top:10px;"
              )
            ), 
            
            style="box-shadow:2px 2px 5px 2px #ccc;padding:15px 0px 15px 10px;"
          ),
          
          ## result tab panel
          div(
            useShinyjs(),
            
            tags$hr(),
            
            tabsetPanel(
              id = "ConsensusModelPageSubmoduleFunctionalAnalysisGSEATabsetPanel",
              # type = "pill",
              # vertical = TRUE, 
              # side = "left",
              
              
              # GO
              tabPanel(
                title = "GO",
                value = "GSEA_GO",
                
                tags$br(),
                
                # plot output
                # plotOutput(
                #   outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlot",
                #   width = "100%",
                #   height = "500px"
                # ),
                htmlOutput(
                  outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotHtml"
                ),
                
                tags$br(),
                
                ## download gsea plot
                span(
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotDownloadPdfBtn",
                    label = "Download PDF",
                    icon = icon("file-pdf-o"),
                    style = "color:#EAC428;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotDownloadPngBtn",
                    label = "Download PNG",
                    icon = icon("file-image-o"),
                    style = "color:#EAC428;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotDownloadTableBtn",
                    label = "Download Table",
                    icon = icon("table"),
                    style = "color:#EAC428;background-color:#ffffff;",
                  ),
                ),
                
                ## download and detail plots
                tags$hr(),
                tags$br(),
                
                div(
                  # title
                  fluidRow(
                    column(
                      6,
                      p("Select a GO term", 
                        style = "font-weight:bold;font-size:110%;")
                    ),
                    column(
                      3 # show plot btn
                    ),
                    column(
                      3
                    )
                  ),
                  
                  # element
                  fluidRow(
                    
                    ## download
                    column(
                      6,
                      selectInput(
                        inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotTermSelection",
                        width = "100%",
                        label = NULL,
                        choices = "",
                        multiple = FALSE
                      )
                    ),
                    
                    column(
                      3,
                      shiny::actionButton(
                        inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotStartBtn",
                        label = "Show Plot",
                        icon = icon("area-chart"),
                        style = "background-color:#EAC428; border-color:#EAC428;color:#ffffff;"
                      )
                    ),
                    column(
                      3
                    )
                  ),
                  
                  style="box-shadow:2px 2px 5px 2px #ccc;padding:15px 0px 0px 10px;"
                ),
                
                
                # subplot output
                # sub plot
                # put plot outside the div, show it in the first place, some thing wrong with dropdown menu
                tags$br(),
                div(
                  plotOutput(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplot",
                    height = "440px",
                    width = "400px"
                  ),
                  style = "overflow-x:auto;"
                  
                ),
                
                div(
                  tags$br(),
                  
                  # download btn
                  span(
                    shiny::downloadButton(
                      outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotDownloadPdfBtn",
                      label = "Download PDF",
                      icon = icon("file-pdf-o"),
                      style = "color:#EAC428;background-color:#ffffff;",
                    ),
                    shiny::downloadButton(
                      outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotDownloadPngBtn",
                      label = "Download PNG",
                      icon = icon("file-image-o"),
                      style = "color:#EAC428;background-color:#ffffff;",
                    ),
                  ),
                  
                  id = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotDiv",
                  class = "hidden-element"
                ),
                
                tags$br()
              ),
              
              # KEGG
              tabPanel(
                title = "KEGG",
                value = "GSEA_KEGG",
                
                tags$br(),
                
                # plot output
                # plotOutput(
                #   outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlot",
                #   width = "100%",
                #   height = "500px"
                # ),
                htmlOutput(
                  outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotHtml"
                ),
                
                tags$br(),
                
                ## download gsea plot
                span(
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotDownloadPdfBtn",
                    label = "Download PDF",
                    icon = icon("file-pdf-o"),
                    style = "color:#EAC428;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotDownloadPngBtn",
                    label = "Download PNG",
                    icon = icon("file-image-o"),
                    style = "color:#EAC428;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotDownloadTableBtn",
                    label = "Download Table",
                    icon = icon("table"),
                    style = "color:#EAC428;background-color:#ffffff;",
                  ),
                ),
                
                ## download and detail plots
                tags$hr(),
                tags$br(),
                
                div(
                  # title
                  fluidRow(
                    column(
                      6,
                      p("Select a KEGG pathway", 
                        style = "font-weight:bold;font-size:110%;")
                    ),
                    column(
                      3 # show plot btn
                    ),
                    column(
                      3
                    )
                  ),
                  
                  # element
                  fluidRow(
                    
                    ## download
                    column(
                      6,
                      selectInput(
                        inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotTermSelection",
                        width = "100%",
                        label = NULL,
                        choices = "",
                        multiple = FALSE
                      )
                    ),
                    
                    column(
                      3,
                      shiny::actionButton(
                        inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotStartBtn",
                        label = "Show Plot",
                        icon = icon("area-chart"),
                        style = "background-color:#EAC428; border-color:#EAC428;color:#ffffff;"
                      )
                    ),
                    column(
                      3
                    )
                  ),
                  
                  style="box-shadow:2px 2px 5px 2px #ccc;padding:15px 0px 0px 10px;"
                ),
                tags$br(),
                # subplot output
                # sub plot
                div(
                  plotOutput(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplot",
                    height = "440px",
                    width = "400px"
                  ),
                  style = "overflow-x:auto;"
                ),
                
                div(
                  
                  # download btn
                  span(
                    shiny::downloadButton(
                      outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotDownloadPdfBtn",
                      label = "Download PDF",
                      icon = icon("file-pdf-o"),
                      style = "color:#EAC428;background-color:#ffffff;",
                    ),
                    shiny::downloadButton(
                      outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotDownloadPngBtn",
                      label = "Download PNG",
                      icon = icon("file-image-o"),
                      style = "color:#EAC428;background-color:#ffffff;",
                    ),
                  ),
                  
                  id = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotDiv",
                  class = "hidden-element"
                ),
                
                tags$br()
                
              ),
              
              # Hallmark
              tabPanel(
                title = "Hallmark",
                value = "GSEA_Hallmark",
                
                tags$br(),
                
                # plot output
                # plotOutput(
                #   outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlot",
                #   width = "100%",
                #   height = "500px"
                # ),
                htmlOutput(
                  outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotHtml"
                ),
                
                tags$br(),
                
                ## download gsea plot
                span(
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotDownloadPdfBtn",
                    label = "Download PDF",
                    icon = icon("file-pdf-o"),
                    style = "color:#EAC428;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotDownloadPngBtn",
                    label = "Download PNG",
                    icon = icon("file-image-o"),
                    style = "color:#EAC428;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotDownloadTableBtn",
                    label = "Download Table",
                    icon = icon("table"),
                    style = "color:#EAC428;background-color:#ffffff;",
                  ),
                ),
                
                ## download and detail plots
                tags$hr(),
                tags$br(),
                
                div(
                  # title
                  fluidRow(
                    column(
                      6,
                      p("Select a Hallmark term", 
                        style = "font-weight:bold;font-size:110%;")
                    ),
                    column(
                      3 # show plot btn
                    ),
                    column(
                      3
                    )
                  ),
                  
                  # element
                  fluidRow(
                    
                    ## download
                    column(
                      6,
                      selectInput(
                        inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotTermSelection",
                        width = "100%",
                        label = NULL,
                        choices = "",
                        multiple = FALSE
                      )
                    ),
                    
                    column(
                      3,
                      shiny::actionButton(
                        inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotStartBtn",
                        label = "Show Plot",
                        icon = icon("area-chart"),
                        style = "background-color:#EAC428; border-color:#EAC428;color:#ffffff;"
                      )
                    ),
                    column(
                      3
                    )
                  ),
                  
                  style="box-shadow:2px 2px 5px 2px #ccc;padding:15px 0px 0px 10px;"
                ),
                
                # subplot output
                # sub plot
                tags$br(),
                div(
                  plotOutput(
                    outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplot",
                    height = "440px",
                    width = "400px"
                  ),
                  style = "overflow-x:auto;"
                ),
                
                div(
                  
                  # download btn
                  span(
                    shiny::downloadButton(
                      outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotDownloadPdfBtn",
                      label = "Download PDF",
                      icon = icon("file-pdf-o"),
                      style = "color:#EAC428;background-color:#ffffff;",
                    ),
                    shiny::downloadButton(
                      outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotDownloadPngBtn",
                      label = "Download PNG",
                      icon = icon("file-image-o"),
                      style = "color:#EAC428;background-color:#ffffff;",
                    ),
                  ),
                  
                  id = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotDiv",
                  class = "hidden-element"
                ),
                
                tags$br()
                
              )
            ),
            
            id = "ConsensusModelPageSubmoduleFunctionalAnalysisGSEATabsetPanelDiv",
            class = "hidden-element" 
          )
        )
      )
    )
  )
}
