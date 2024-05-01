# this script render the UI of analysis page of module prognostic model


# set up with UI
ui.page_module_pm_immunotherapy <- function() {
  fluidPage(
    
    ## recalculate gene score input
    # div(
    #   bs4Dash::bs4Card(
    #     fluidRow(
    #       column(
    #         3,
    #         p("Gene list",style="font-size:110%;font-weight:bold;")
    #       ),
    #       column(
    #         3,
    #         p("Gene list name",style="font-size:110%;font-weight:bold;")
    #       ),
    #       column(
    #         6
    #       )
    #     ),
    #     fluidRow(
    #       column(
    #         3,
    #         ## genelist
    #         shiny::textAreaInput(
    #           inputId = "PrognosticModelPageSubmoduleImmunotherapyGenelistTextArea",
    #           label = NULL,
    #           placeholder = "TP53\nEGFR\n",
    #           resize = "vertical",
    #           width = "100%",
    #           height = "150px"
    #         )
    #       ),
    #       
    #       column(
    #         3,
    #         ## genelist name
    #         shiny::textInput(
    #           inputId = "PrognosticModelPageSubmoduleImmunotherapyGenelistNameInput",
    #           label = NULL,
    #           placeholder = "My Gene List 1",
    #           width = "100%"
    #         )
    #       ),
    #       
    #       column(
    #         5,
    #         shiny::actionButton(
    #           inputId = "PrognosticModelPageSubmoduleImmunotherapyRecalGeneScoreSubmitBtn",
    #           label = "Recalculate",
    #           icon = icon("rocket"),
    #           style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
    #         ),
    #         p("Click to recalculate the score of gene list with previous prognostic model in ICI related datasets.",
    #           style = "font-style:italic;color:#3b3b3b;font-size:80%;padding-top:10px;")
    #       )
    #       
    #     ),
    #     
    #     
    #     id = "HomePageLastInfoCard",
    #     title = "Analysis settings",
    #     icon = icon("cogs"),
    #     collapsible = FALSE,
    #     width = 12,
    #     closable = FALSE,
    #     maximizable = FALSE
    #   ),
    #   
    #   style = "box-shadow:2px 2px 5px 2px #ccc;"
    # ), 
    
    div(
      span(
        shiny::actionButton(
          inputId = "PrognosticModelPageSubmoduleImmunotherapyRecalGeneScoreSubmitBtn",
          label = "Start",
          icon = icon("rocket"),
          style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
        ),
        span(
          "Click to recalculate the score of gene list with previous prognostic model in ICI related datasets.",
          style = "font-style:italic;color:#3b3b3b;font-size:80%;padding-top:10px;"
        )
      ), 
      style="box-shadow:2px 2px 5px 2px #ccc;padding:15px 0px 15px 10px;"
    ),
    
    
    # space
    tags$hr(),
    
    ## total tab 
    div(
      tabsetPanel(
        id = "PrognosticModelPageSubmoduleImmunotherapyTopTabsetPanel",
        
        ##### tab 1 #####
        tabPanel(
          title = "Differential Expression Analysis",
          value = "DifferentialExpresesionAnalysis",
          
          tags$br(),
          
          #### user input, plot configuration
          # title
          div(
            bs4Dash::bs4Card(
              fluidRow(
                useShinyjs(),
                
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
                    inputId = "PrognosticModelPageSubmoduleImmunotherapyGeneExpTabColorSelection",
                    label = NULL,
                    choices = c("npg","nejm","jco","d3","lancet","jama"),
                    width = "100%"
                  )
                ),
                
                # transparency
                column(
                  6,
                  sliderInput(
                    inputId = "PrognosticModelPageSubmoduleImmunotherapyGeneExpTabAlphaSelection",
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
                  
                  shiny::actionButton(
                    inputId = "PrognosticModelPageSubmoduleImmunotherapyGeneExpTabSubmitBtn",
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
            
            # plot output
            htmlOutput(
              outputId = "PrognosticModelPageSubmoduleImmunotherapyGeneExpPlotHtml"
            ),
            
            
            # download
            tags$br(),
            span(
              shiny::downloadButton(
                outputId = "PrognosticModelPageSubmoduleImmunotherapyGeneExpPlotDownloadPdfBtn",
                label = "Download PDF",
                icon = icon("file-pdf-o"),
                style = "color:#FF6B6B;background-color:#ffffff;",
              ),
              shiny::downloadButton(
                outputId = "PrognosticModelPageSubmoduleImmunotherapyGeneExpPlotDownloadPngBtn",
                label = "Download PNG",
                icon = icon("file-image-o"),
                style = "color:#FF6B6B;background-color:#ffffff;",
              )
            ),
            
            id = "PrognosticModelPageSubmoduleImmunotherapyGeneExpPlotDiv",
            class = "hidden-element"
            
          ),
          
          div(
            style = "height:400px;",
            id = "PrognosticModelPageSubmoduleImmunotherapyGeneExpPlotDivBlank"
          ),
        ),
        
        ##### tab 2 #####
        tabPanel(
          title = "Immunotherapy Response Prediction",
          value = "ImmunotherapyResponsePrediction",
          
          tags$br(),
          
          div(
            span(
              shiny::actionButton(
                inputId = "PrognosticModelPageSubmoduleImmunotherapyRocTabSubmitBtn",
                label = "Start",
                icon = icon("rocket"),
                style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
              ),
              
              span("Click the START button to evaluate the performance of clinical models for predicting immunotherapy response.",
                   style = "font-style:italic;color:#3b3b3b;font-size:80%;padding-top:10px;")
            ),
            class = "conf-box"
          ),
          
          ## plots
          div(
            #space
            tags$hr(),
            tags$br(),
            
            # plot output
            htmlOutput(
              outputId = "PrognosticModelPageSubmoduleImmunotherapyRocPlotHtml"
            ),
            
            # download
            tags$br(),
            span(
              shiny::downloadButton(
                outputId = "PrognosticModelPageSubmoduleImmunotherapyRocPlotDownloadPdfBtn",
                label = "Download PDF",
                icon = icon("file-pdf-o"),
                style = "color:#FF6B6B;background-color:#ffffff;",
              ),
              shiny::downloadButton(
                outputId = "PrognosticModelPageSubmoduleImmunotherapyRocPlotDownloadPngBtn",
                label = "Download PNG",
                icon = icon("file-image-o"),
                style = "color:#FF6B6B;background-color:#ffffff;",
              )
            ),
            
            
            id = "PrognosticModelPageSubmoduleImmunotherapyRocPlotDiv",
            class = "hidden-element"
            
          ),
          
          div(
            style = "height:400px;",
            id = "PrognosticModelPageSubmoduleImmunotherapyRocPlotDivBlank"
          ),
        ),
        
        ##### tab 3 #####
        tabPanel(
          title = "Immunotherapy Prognosis Assessment",
          value = "ImmunotherapyPrognosisAssessment",
          
          tags$br(),
          
          #### survival analysis configurations ####
          div(
            bs4Dash::bs4Card(
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
                    
                    id = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabCutoffValuePercentSelectionTitleDiv",
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
                    inputId = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabMethodSelection",
                    label = NULL,
                    width = "100%",
                    choices = c('median','mean','quantile','optimal','custom')
                  )
                ),
                column(
                  6,
                  div(
                    sliderInput(
                      inputId = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabCutoffValuePercentSelection",
                      label = NULL,
                      width = "100%",
                      min = 0.1,
                      max = 0.9,
                      value = 0.3,
                      step = 0.05,
                      ticks = T,
                      animate = T
                    ),
                    
                    id = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabCutoffValuePercentSelectionDiv",
                    class = "hidden-element"
                  )
                ), 
                column(
                  2
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
                      inputId = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabColorSelection",
                      label = NULL,
                      choices = c("npg","nejm","jco","d3","lancet","jama"),
                      width = "100%"
                    )
                  ),
                  
                  # transparency
                  column(
                    6,
                    sliderInput(
                      inputId = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabAlphaSelection",
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
                      inputId = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabSubmitBtn",
                      label = "Start",
                      icon = icon("rocket"),
                      style = "background-color:#FF6B6B; border-color:#FF6B6B;color:#ffffff;"
                    )
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
            
            
            ## multiple plots
            htmlOutput(
              outputId = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabResultPlotHtml",
            ),
            
            tags$br(),
            
            ## download
            span(
              shiny::downloadButton(
                outputId = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabResultPlotDownloadPdfBtn",
                label = "Download PDF",
                icon = icon("file-pdf-o"),
                style = "color:#FF6B6B;background-color:#ffffff;",
              ),
              shiny::downloadButton(
                outputId = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabResultPlotDownloadPngBtn",
                label = "Download PNG",
                icon = icon("file-image-o"),
                style = "color:#FF6B6B;background-color:#ffffff;",
              )
            ),
            
            tags$br(),
            
            # div id
            id = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabResultPlotDiv",
            
            # hide
            class = "hidden-element"
          ),
          
          div(
            style = "height:400px;",
            id = "PrognosticModelPageSubmoduleImmunotherapySurvivalTabResultPlotDivBlank"
          ),
        )
      ),
      
      id = "PrognosticModelPageSubmoduleImmunotherapyTotalTabDiv",
      class = "hidden-element"
    )
  )
}
