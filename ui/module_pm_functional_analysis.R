# this script render the UI of analysis page of module prognostic model


# set up with UI
ui.page_module_pm_functional_analysis <- function() {
  fluidPage(
    column(
      12,
      
      tabsetPanel(
        id = "PrognosticModelPageSubmoduleFunctionalAnalysisTopTabsetPanel",
        
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
                    inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraGeneNumInput",
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
                    inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraQvalueInput",
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
                #     inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraGoCategoryInput",
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
                    inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraGoPlotWidthInput",
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
                    inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraKeggPlotWidthInput",
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
                    inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraSubmitBtn",
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
          
          
          
          
          ## result tabset panel
          div(
            useShinyjs(),
            
            tags$hr(),
            tags$br(),
            
            tabsetPanel(
              id = "PrognosticModelPageSubmoduleFunctionalAnalysisORATabsetPanel",    
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
                  outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraGOPlotHtml"
                ),
                
                # information to show
                tags$br(),
                HTML("<span style=\"font-size:80%;font-style:italic;color:#3b3b3b;\"> <i class=\"fa fa-info-circle\"></i> Red represents positive pathways and blue represents negative pathways. </span>"),
                
                tags$br(),
                ## download
                span(
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraGOPlotDownloadPdfBtn",
                    label = "Download PDF",
                    icon = icon("file-pdf-o"),
                    style = "color:#FF6B6B;background-color:#ffffff;"
                  ),
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraGOPlotDownloadPngBtn",
                    label = "Download PNG",
                    icon = icon("file-image-o"),
                    style = "color:#FF6B6B;background-color:#ffffff;"
                  ),
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraGOPlotDownloadTableBtn",
                    label = "Download Table",
                    icon = icon("table"),
                    style = "color:#FF6B6B;background-color:#ffffff;"
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
                  outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraKEGGPlotHtml"
                ),
                
                # information to show
                tags$br(),
                HTML("<span style=\"font-size:80%;font-style:italic;color:#3b3b3b;\"> <i class=\"fa fa-info-circle\"></i> Red represents positive pathways and blue represents negative pathways. </span>"),
                
                tags$br(),
                
                ## download
                span(
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraKEGGPlotDownloadPdfBtn",
                    label = "Download PDF",
                    icon = icon("file-pdf-o"),
                    style = "color:#FF6B6B;background-color:#ffffff;"
                  ),
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraKEGGPlotDownloadPngBtn",
                    label = "Download PNG",
                    icon = icon("file-image-o"),
                    style = "color:#FF6B6B;background-color:#ffffff;"
                  ),
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisOraKEGGPlotDownloadTableBtn",
                    label = "Download Table",
                    icon = icon("table"),
                    style = "color:#FF6B6B;background-color:#ffffff;"
                  )
                ),
                
                tags$br()
                
              )
            ),
            
            id = "PrognosticModelPageSubmoduleFunctionalAnalysisORATabsetPanelDiv",
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
                inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaSubmitBtn",
                label = "Start",
                icon = icon("rocket"),
                style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
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
              id = "PrognosticModelPageSubmoduleFunctionalAnalysisGSEATabsetPanel",
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
                #   outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaGoPlot",
                #   width = "100%",
                #   height = "500px"
                # ),
                htmlOutput(
                  outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaGoPlotHtml"
                ),
                
                tags$br(),
                
                ## download gsea plot
                span(
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaGoPlotDownloadPdfBtn",
                    label = "Download PDF",
                    icon = icon("file-pdf-o"),
                    style = "color:#FF6B6B;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaGoPlotDownloadPngBtn",
                    label = "Download PNG",
                    icon = icon("file-image-o"),
                    style = "color:#FF6B6B;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaGoPlotDownloadTableBtn",
                    label = "Download Table",
                    icon = icon("table"),
                    style = "color:#FF6B6B;background-color:#ffffff;",
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
                        inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotTermSelection",
                        width = "100%",
                        label = NULL,
                        choices = "",
                        multiple = FALSE
                      )
                    ),
                    
                    column(
                      3,
                      shiny::actionButton(
                        inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotStartBtn",
                        label = "Show Plot",
                        icon = icon("area-chart"),
                        style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
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
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplot",
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
                      outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotDownloadPdfBtn",
                      label = "Download PDF",
                      icon = icon("file-pdf-o"),
                      style = "color:#FF6B6B;background-color:#ffffff;",
                    ),
                    shiny::downloadButton(
                      outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotDownloadPngBtn",
                      label = "Download PNG",
                      icon = icon("file-image-o"),
                      style = "color:#FF6B6B;background-color:#ffffff;",
                    ),
                  ),
                  
                  id = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotDiv",
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
                #   outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaKeggPlot",
                #   width = "100%",
                #   height = "500px"
                # ),
                htmlOutput(
                  outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaKeggPlotHtml"
                ),
                
                tags$br(),
                
                ## download gsea plot
                span(
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaKeggPlotDownloadPdfBtn",
                    label = "Download PDF",
                    icon = icon("file-pdf-o"),
                    style = "color:#FF6B6B;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaKeggPlotDownloadPngBtn",
                    label = "Download PNG",
                    icon = icon("file-image-o"),
                    style = "color:#FF6B6B;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaKeggPlotDownloadTableBtn",
                    label = "Download Table",
                    icon = icon("table"),
                    style = "color:#FF6B6B;background-color:#ffffff;",
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
                        inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotTermSelection",
                        width = "100%",
                        label = NULL,
                        choices = "",
                        multiple = FALSE
                      )
                    ),
                    
                    column(
                      3,
                      shiny::actionButton(
                        inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotStartBtn",
                        label = "Show Plot",
                        icon = icon("area-chart"),
                        style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
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
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplot",
                    height = "440px",
                    width = "400px"
                  ),
                  style = "overflow-x:auto;"
                ),
                
                div(
                  
                  # download btn
                  span(
                    shiny::downloadButton(
                      outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotDownloadPdfBtn",
                      label = "Download PDF",
                      icon = icon("file-pdf-o"),
                      style = "color:#FF6B6B;background-color:#ffffff;",
                    ),
                    shiny::downloadButton(
                      outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotDownloadPngBtn",
                      label = "Download PNG",
                      icon = icon("file-image-o"),
                      style = "color:#FF6B6B;background-color:#ffffff;",
                    ),
                  ),
                  
                  id = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotDiv",
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
                #   outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlot",
                #   width = "100%",
                #   height = "500px"
                # ),
                htmlOutput(
                  outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotHtml"
                ),
                
                tags$br(),
                
                ## download gsea plot
                span(
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotDownloadPdfBtn",
                    label = "Download PDF",
                    icon = icon("file-pdf-o"),
                    style = "color:#FF6B6B;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotDownloadPngBtn",
                    label = "Download PNG",
                    icon = icon("file-image-o"),
                    style = "color:#FF6B6B;background-color:#ffffff;",
                  ),
                  shiny::downloadButton(
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotDownloadTableBtn",
                    label = "Download Table",
                    icon = icon("table"),
                    style = "color:#FF6B6B;background-color:#ffffff;",
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
                        inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotTermSelection",
                        width = "100%",
                        label = NULL,
                        choices = "",
                        multiple = FALSE
                      )
                    ),
                    
                    column(
                      3,
                      shiny::actionButton(
                        inputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotStartBtn",
                        label = "Show Plot",
                        icon = icon("area-chart"),
                        style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
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
                    outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplot",
                    height = "440px",
                    width = "400px"
                  ),
                  style = "overflow-x:auto;"
                ),
                
                div(
                  
                  # download btn
                  span(
                    shiny::downloadButton(
                      outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotDownloadPdfBtn",
                      label = "Download PDF",
                      icon = icon("file-pdf-o"),
                      style = "color:#FF6B6B;background-color:#ffffff;",
                    ),
                    shiny::downloadButton(
                      outputId = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotDownloadPngBtn",
                      label = "Download PNG",
                      icon = icon("file-image-o"),
                      style = "color:#FF6B6B;background-color:#ffffff;",
                    ),
                  ),
                  
                  id = "PrognosticModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotDiv",
                  class = "hidden-element"
                ),
                
                tags$br()
                
              )
            ),
            
            id = "PrognosticModelPageSubmoduleFunctionalAnalysisGSEATabsetPanelDiv",
            class = "hidden-element" 
          )
        )
      )
    )
  )
}
