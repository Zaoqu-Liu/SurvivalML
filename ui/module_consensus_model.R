# this script render the UI of analysis page of module: consensus model
# cancer.datasets <- openxlsx::read.xlsx("./doc/datasetInfo/Bladder.xlsx",sheet = 1)

# set up with UI
ui.page_module_consensus_model <- function() {
    
    fluidPage(
      useShinyjs(),
      use_notiflix_confirm(),
      
      style = "width:80%;",
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "./static/css/dataset.css")
      ),
      
      #some space on the head
      tags$br(),
      
      fluidRow(
        div(
          column(
            8,
            fluidRow(
              column(2, img(src = "model4.png", width = 125), style = "padding-top:10px;"), 
              column(10,
                     h2("Consensus Model", style = "color:#eac428; font-weight: bold;"),
                     p("Users can generate a consensus machine learning-derived model with best power for predicting prognosis based on 10 prevalent machine learning algorithms.", 
                       style="text-align:justify; "))
            ),
          ),
          column(
            4
          ),
          
          # background image
          style = "background-image:url(\"./module_background.png\");background-attachment: scroll; background-size: 1450px 150px"
        )
      ),
      tags$hr(),
      tags$br(),
      
      
      #### if from begining ####
      #### User input ####
      fluidRow(
        # include shinyfeedback
        useShinyFeedback(),
        
        ##### event selection #####
        column(
          3,
          bs4Dash::bs4Card(
            
            # select outcome
            # selectInput(
            #   inputId = "AnalysisPageConsensusModelModuleEventOutcomeSelection",
            #   label = "Select an event outcome",
            #   width = "100%",
            #   choices = c("Overall survival","Disease free survival","Tumor relapse")
            # ),
            p("Select an event outcome"),
            
            shinyWidgets::awesomeRadio(
              inputId = "AnalysisPageConsensusModelModuleEventOutcomeSelection",
              label = NULL,
              choices =
                c(
                  "Overall survival",
                  "Relapse-free survival",
                  "Disease-free survival",
                  "Progression-free survival",
                  "Disease-specific survival"
                ),
              selected = "",
              status = "info",
              checkbox = FALSE,
              width = "100%"
            ),
            
            title = "1. Prognostic Outcomes",
            collapsible = FALSE,
            collapsed = FALSE,
            background = "gray",
            closable = FALSE,
            maximizable = FALSE,
            icon = icon("wheelchair"),
            width = 12,
            height = "390px",
            id = "AnalysisPageConsensusModelModuleUserInput1Outcome"
          )
        ),
        
        
        ##### cohort #####
        column(
          3,
          bs4Dash::bs4Card(
            p("Determine datasets with the survival outcome you selected:"),
            
            # datasets 
            div(
              style = "height:180px;overflow-y:scroll;",
              shinyWidgets::checkboxGroupButtons(
                inputId = "AnalysisPageConsensusModelModuleDatasetGroup",
                label = NULL,
                choiceNames = "", 
                choiceValues = "",
                individual = TRUE,
                direction = "horizontal",
                checkIcon = list(yes = icon("dot-circle-o"), no = icon("circle-o"))
              )
            ),
            
            p(""),
            ## select/disselect all btn
            fluidRow(
              span(
                # select all
                shiny::actionButton(
                  inputId = "AnalysisPageConsensusModelModuleDatasetGroupSelectAllBtn",
                  label = "Select all",
                  icon = icon("check"),
                  style = "color:#17c3b2; background-color:white;"
                ),
                # disselect all
                shiny::actionButton(
                  inputId = "AnalysisPageConsensusModelModuleDatasetGroupDisSelectAllBtn",
                  label = "Disselect all",
                  icon = icon("close"),
                  style = "color:#17c3b2; background-color:white;"
                ),
                style = "text-align:center"
              )
            ),
            p(""),
                        
            ## submit cohort btn
            # shinyWidgets::actionBttn(
            #   inputId = "AnalysisPageConsensusModelModuleDatasetGroupSubmitBtn",
            #   label = "Submit",
            #   icon = icon("check"),
            #   color = "warning",
            #   style = "material-flat",
            #   size = "sm",
            #   block = TRUE
            # ),
            shiny::actionButton(
              inputId = "AnalysisPageConsensusModelModuleDatasetGroupSubmitBtn",
              label = "Submit",
              icon = icon("check"),
              width = "100%",
              style = "color:white; background-color:#17c3b2;text-align:center;"
            ),
            
            
            # card settings
            title = "2. Available Datasets",
            collapsible = FALSE,
            collapsed = FALSE,
            background = "gray",
            closable = FALSE,
            maximizable = FALSE,
            icon = icon("group"),
            width = 12,
            height = "390px",
            id = "AnalysisPageConsensusModelModuleUserInput2"
          )
        ),
        
        ##### stat #####
        column(
          3,
          bs4Dash::bs4Card(
            
            p(" "),
            
            # number of datasets selected
            summaryBox2(title = "Number of Cohorts",
                        value = textOutput(outputId = "AnalysisPageConsensusModelModuleCohortNumber"),
                        width = 12,
                        style = "info",
                        icon = "fa fa-group"),
            
            # number of datasets selected
            summaryBox2(title = "Number of Patients",
                        value = textOutput(outputId = "AnalysisPageConsensusModelModulePatientNumber"),
                        width = 12,
                        style = "danger",
                        icon = "fa fa-user-plus"),
            
            # number of genes filtered
            summaryBox2(title = "Number of Genes",
                        value = textOutput(outputId = "AnalysisPageConsensusModelModuleGeneNumber"),
                        width = 12,
                        style = "primary",
                        icon = "fa fa-google"),
            
            
            # card information
            title = "3. Summary",
            collapsible = FALSE,
            collapsed = FALSE,
            background = "gray",
            closable = FALSE,
            maximizable = FALSE,
            icon = icon("calculator"),
            width = 12,
            height = "390px",
            id = "AnalysisPageConsensusModelModuleUserInput3"
          ),
        ),
        
        ##### datasets,train,validation #####
        column(
          3,
          
          bs4Dash::bs4Card(
            
            p("Split into training or validation datasets:"),
            
            # training datasets
            fluidRow(
              #tags$p("Traning set",style="font-size:120%;"),
              column(
                12,
                
                # use dropdown picker instead of text input
                selectInput(
                  inputId = "AnalysisPageConsensusModelModuleTrainDatasetSelection",
                  label = "Training dataset", 
                  width = "100%", 
                  choices = c("")
                ),
                tags$hr()
              )
            ),
            
            
            p("Validation datasets"),
            # validation set
            # textAreaInput(
            #   inputId = "AnalysisPageConsensusModelModuleValidationDatasetSelection",
            #   label = "Validation datasets",
            #   height = "150px",
            #   resize = "none",
            #   width = "100%"
            # ), 
            
            div(
              verbatimTextOutput(
                outputId = "AnalysisPageConsensusModelModuleValidationDataset"
              ),
              style = "height:70px; overflow-y:auto;"
            ),
            
            p("* Once the training dataset is determined, the remaining datasets are automatically assigned to the validation datasets.",
              style = "color:#3b3b3b; font-size:80%; font-style:italic; text-align:justify;"),
            
            title = "4. Training and Validation",
            collapsible = FALSE,
            collapsed = FALSE,
            background = "gray",
            closable = FALSE,
            maximizable = FALSE,
            icon = icon("database"),
            width = 12,
            height = "390px",
            id = "AnalysisPageConsensusModelModuleUserInput1Pval"
          )
        )
      ),
      
      # # # venn plot or upset plot
      # div(
      #   fluidRow(
      #     column(
      #       2
      #     ),
      #     
      #     column(
      #       8,
      #       # space
      #       tags$br(),
      #       
      #       # venn plot
      #       plotlyOutput(
      #         outputId = "AnalysisPageConsensusModelModuleGeneVennPlot"
      #       ),
      #       
      #       # space
      #       tags$br(),
      #       tags$hr()
      #     ),
      #     
      #     column(
      #       2
      #     )
      #   ),
      #   
      #   # div settings with id
      #   id = "AnalysisPageConsensusModelModuleGeneVennPlotDiv",
      #   # add class
      #   class = "hidden-element"
      # ),
      
      
      
      # #### analyze btn, filter gene analysis ####
      # div(
      #   # space before go analyze btn
      #   tags$br(),
      #   tags$br(),
      #   
      #   fluidRow(
      #     column(
      #       2
      #     ),
      #     
      #     column(
      #       8,
      #       shinyWidgets::actionBttn(
      #         inputId = "AnalysisPageConsensusModelModuleStartGeneAnalysisBtn",
      #         label = "Start Consensus Model",
      #         icon = icon("rocket"),
      #         color = "warning",
      #         style = "unite",
      #         block = TRUE
      #       )
      #     ),
      #     
      #     column(
      #       2
      #     )
      #   ),
      #   
      #   class = "hidden-element",
      #   id = "AnalysisPageConsensusModelModuleStartGeneAnalysisBtnDiv"
      # ),
        
        
            
      #### gene filter ####
      div(
        # shinyfeedback
        useShinyFeedback(),
        
        # space
        tags$br(),
        tags$hr(),
        tags$br(),
        
        fluidRow(
          ##### choose gene #####
          column(
            4,
            
            bs4Dash::box(
              
              ## help button 
              div(
                shiny::actionButton(
                  inputId = "AnalysisPageConsensusModelModuleGeneScoreHelpInfoBtn",
                  style = "background-color:#fff;border-color:#fff",
                  label = NULL,
                  icon = icon("question-circle")
                ),
                style = "text-align:right;"
              ),
              
              ## pvalue 
              fluidRow(
                column(
                  6,
                  p("Cox P-value cutoff",
                    style = "color:black;padding-top:3px;")
                ),
                column(
                  6,
                  shiny::selectizeInput(
                    inputId = "AnalysisPageConsensusModelModulePvalInput",
                    label = NULL,
                    choices = c("0.05","0.01","0.001","0.1"),
                    options = list(create = TRUE),
                    width = "100%"
                  )
                )
              ),
              
              ## filter out genes with inconsistent prognosis
              fluidRow(
                column(
                  10,
                  p("Filter genes with inconsistent prognosis",
                    style = "color:black;")
                ),
                column(
                  2,
                  div(
                    shinyWidgets::materialSwitch(
                      inputId = "AnalysisPageConsensusModelModuleFilterInconsistentGeneInput",
                      label = NULL, 
                      value = TRUE,
                      status = "warning"
                    ),
                    style = "padding-top:8px;"
                  )
                  
                )
              ),
              
              ## screen with cohort number
              # slider input of cohort number
              shiny::sliderInput(
                inputId = "AnalysisPageConsensusModelModuleGeneScreenCohortNumber",
                label = "Number of significant cohort(s)",
                value = 0,
                ticks = FALSE,
                min = 0,
                max = 10,
                step = 1,
                round = TRUE,
                animate = FALSE,
                width = "100%"
              ),
              
              # some help info
              tags$p("Minimum number of datasets for which genes have prognostic significance", 
                     style = "font-size:80%;color:#c5c5c5;font-style:italic;margin-top:0px;"),
              
              # submit button
              fluidRow(
                column(1),
                column(
                  10,
                  # shinyWidgets::actionBttn(
                  #   inputId = "AnalysisPageConsensusModelModuleGeneScorePlotSubmitBtn",
                  #   label = "Submit",
                  #   icon = icon("filter"),
                  #   color = "warning",
                  #   style = "unite",
                  #   block = TRUE,
                  #   size = "sm"
                  # )
                  shiny::actionButton(
                    inputId = "AnalysisPageConsensusModelModuleGeneScorePlotSubmitBtn",
                    label = "Submit",
                    icon = icon("filter"),
                    width = "100%",
                    style = "color:#d4a373;border-radius:20px;background-color:#fff;border:#d4a373 1px solid;"
                  )
                ),
                column(1)
              ),
              
              tags$br(),
              
              id = "CMPageInfoCard", 
              title = "5. Determine candidate genes", 
              icon = icon("filter"), 
              collapsible = FALSE, 
              width = 12,
              closable = FALSE, 
              maximizable = FALSE
            ),
                
            p("  "),
            
          ),
          
          ##### > gene score plot #####
          column(
            8,
            div(
              div(
                p(""),
                p(""),
                style = "height:75px;"
              ),
              column(
                12,
                div(
                  img(src = "static/picture/waiting3.gif",
                      alt = "waiting for calculation",
                      style = "height:200px;"),
                  
                  # div style to make img at center
                  style = "display:flex; align-items: center;justify-content: center;opacity:1;",
                )
              ),
              
              id = "AnalysisPageConsensusModelModuleGeneScorePlotWaitingImgDiv",
              style = "height:450px;"
            ),
            
            div(
              shinyjs::useShinyjs(),
              
              ## draw heatmap by using heatmaply
              htmlOutput(
                outputId = "AnalysisPageConsensusModelModuleGeneFilterPlotHtml"
              ),
              
              p("  "),
              
              # plot download btn
              span(
                shiny::downloadButton(
                  outputId = "AnalysisPageConsensusModelModuleGeneFilterPlotDownloadPdfBtn",
                  label = "Download PDF",
                  icon = icon("file-pdf-o"),
                  style = "color:#eac428;background-color:#ffffff;"
                ),
                shiny::downloadButton(
                  outputId = "AnalysisPageConsensusModelModuleGeneFilterPlotDownloadPngBtn",
                  label = "Download PNG",
                  icon = icon("file-image-o"),
                  style = "color:#eac428;background-color:#ffffff;"
                ),
                shiny::downloadButton(
                  outputId = "AnalysisPageConsensusModelModuleHeatmap2DownloadGenelistCSVBtn",
                  label = "Download Table",
                  icon = icon("table"),
                  style = "color:#eac428;background-color:#ffffff;"
                )
              ),
              
              id = "AnalysisPageConsensusModelModuleGeneFilterPlotHtmlDiv",
              style = "display:none;"
            )
            
          ),
          
          ## start modeling btn
          # # space
          # p(""),
          
        ),
        
        # div settings
        id = "AnalysisPageConsensusModelModuleGeneScorePlotDiv",
        
        # add class
        # class = "hidden-element"
      ),
      
      
      #### model score plot ####
      div(
        id = "AnalysisPageConsensusModelModuleModelSettingDiv",
        style = "display:none;",
        
        shinyjs::useShinyjs(),
        
        # #space
        # tags$hr(),
        # tags$br(),
        
        ###### > algorithm settings######
        div(
          div(
            bs4Dash::box(
              
              # help info
              div(
                shiny::actionButton(
                  inputId = "AnalysisPageConsensusModelAlgorithmSettingHelpInfoBtn",
                  style = "background-color:#fff;border-color:#fff",
                  label = NULL,
                  icon = icon("question-circle")
                ),
                style = "text-align:right;"
              ),
              
              ## default show stepcox
              fluidRow(
                column(
                  1,
                  style = "width:5%"
                ),
                column(
                  2,
                  
                  ###### stepcox #####
                  div(
                    p("StepCox",
                      style = "font-weight:bold;margin-bottom:1px;"),
                    style = "background-color:#fae8b1;text-align:center;"
                  ),
                  
                  tags$hr(style="margin-top:0px;height:3px;"),
                  
                  div(
                    selectInput(
                      inputId = "CM_StepCox_direction",
                      label = "StepCox direction",
                      selected = "backward",
                      choices = c(
                        "forward",
                        "backward",
                        "both"
                      ),
                      width = "100%"
                    ),
                    
                    id = "CMStepCoxSettingDiv",
                    style = "height:160px;overflow-y:auto;"
                  ),
                  style = "width:18%"
                ),
                column(
                  2,
                  
                  ###### RSF ######
                  div(
                    p("RSF",
                      style = "font-weight:bold;margin-bottom:1px;"),
                    style = "background-color:#fae8b1;text-align:center;"
                  ),
                  
                  tags$hr(style="margin-top:0px;height:3px;"),
                  
                  div(
                    numericInput(
                      inputId = "CM_RSF_nodesize",
                      label = "RSF nodesize",
                      min = 3,
                      max = 30,
                      value = 5,
                      step = 1,
                      width = "100%"
                    ),
                    
                    numericInput(
                      inputId = "CM_RSF_nsplit",
                      label = "RSF nsplit",
                      min = 2,
                      max = 20,
                      value = 10,
                      step = 1,
                      width = "100%"
                    ),
                    
                    selectInput(
                      inputId = "CM_RSF_splitrule",
                      label = "RSF splitrule",
                      choices = c(
                        "logrank",
                        "bs.gradient",
                        "logrankscore"
                      ),
                      multiple = FALSE,
                      width = "100%"
                    ),
                    
                    id = "CMRSFSettingDiv",
                    style = "height:160px;overflow-y:auto;"
                  ),
                  style = "width:18%"
                ),
                column(
                  2,
                  
                  ###### Lasso #####
                  div(
                    p("Lasso",
                      style = "font-weight:bold;margin-bottom:1px;"),
                    style = "background-color:#fae8b1;text-align:center;"
                  ),
                  
                  tags$hr(style="margin-top:0px;height:3px;"),
                  
                  div(
                    selectInput(
                      inputId = "CM_Lasso_lamda_rule",
                      label = "Lasso lamda rule",
                      width = "100%",
                      choices = c(
                        "lambda.min",
                        "lambda.1se"
                      ),
                      multiple = FALSE
                    ),
                    
                    id = "CMLassoSettingDiv",
                    style = "height:160px;overflow-y:auto;"
                  ),
                  style = "width:18%"
                ),
                column(
                  2,
                  ###### Ridge #####
                  div(
                    p("Ridge",
                      style = "font-weight:bold;margin-bottom:1px;"),
                    style = "background-color:#fae8b1;text-align:center;"
                  ),
                  
                  tags$hr(style="margin-top:0px;height:3px;"),
                  
                  div(
                    selectInput(
                      inputId = "CM_Ridge_lamda_rule",
                      label = "Ridge lamda rule",
                      width = "100%",
                      choices = c(
                        "lambda.min",
                        "lambda.1se"
                      ),
                      multiple = FALSE
                    ),
                    
                    id = "CMRidgeSettingDiv",
                    style = "height:160px;overflow-y:auto;"
                  ),
                  style = "width:18%"
                ),
                column(
                  2,
                  
                  ###### Coxboost ######
                  div(
                    p("Coxboost",
                      style = "font-weight:bold;margin-bottom:1px;"),
                    style = "background-color:#fae8b1;text-align:center;"
                  ),
                  
                  tags$hr(style="margin-top:0px;height:3px;"),
                  
                  div(
                    selectInput(
                      inputId = "CM_Coxboost_type",
                      label = "Coxboost type",
                      width = "100%",
                      choices = c(
                        "verweij",
                        "naive"
                      ),
                      multiple = FALSE
                    ),
                    
                    id = "CMCoxboostSettingDiv",
                    style = "height:160px;overflow-y:auto;"
                  ),
                  style = "width:18%"
                ),
                column(
                  1,
                  style = "width:5%"
                )
              ),
              
              tags$br(),
              
              fluidRow(
                column(
                  1,
                  style = "width:5%"
                ),
                column(
                  2,
                  ###### Enet #####
                  div(
                    p("Enet",
                      style = "font-weight:bold;margin-bottom:1px;"),
                    style = "background-color:#fae8b1;text-align:center;"
                  ),
                  
                  tags$hr(style="margin-top:0px;height:3px;"),
                  
                  div(
                    numericInput(
                      inputId = "CM_Enet_alpha",
                      label = "Enet alpha",
                      min = 0.1,
                      max = 0.9,
                      value = 0.5,
                      width = "100%",
                      step = 0.1
                    ),
                    
                    selectInput(
                      inputId = "CM_Enet_lamda_rule",
                      label = "Enet lamda rule",
                      width = "100%",
                      choices = c(
                        "lambda.min",
                        "lambda.1se"
                      ),
                      multiple = FALSE
                    ),
                    
                    id = "CMEnetSettingDiv",
                    style = "height:160px;overflow-y:auto;"
                  ),
                  style = "width:18%"
                ),
                column(
                  2,
                  
                  ###### GBM #####
                  div(
                    p("GBM",
                      style = "font-weight:bold;margin-bottom:1px;"),
                    style = "background-color:#fae8b1;text-align:center;"
                  ),
                  
                  tags$hr(style="margin-top:0px;height:3px;"),
                  
                  div(
                    numericInput(
                      inputId = "CM_GBM_nodesize",
                      label = "GBM nodesize",
                      min = 3,
                      max = 30,
                      value = 5,
                      width = "100%",
                      step = 1
                    ),
                    
                    id= "PMGBMSettingDiv",
                    style = "height:160px;overflow-y:auto;"
                  ),
                  style = "width:18%"
                ),
                column(
                  2,
                  
                  ###### SVM #####
                  div(
                    p("SVM",
                      style = "font-weight:bold;margin-bottom:1px;"),
                    style = "background-color:#fae8b1;text-align:center;"
                  ),
                  
                  tags$hr(style="margin-top:0px;height:3px;"),
                  
                  div(
                    selectInput(
                      inputId = "CM_SVM_type",
                      label = "SVM type",
                      width = "100%",
                      choices = c(
                        "vanbelle1",
                        "vanbelle2",
                        "regression",
                        "hybrid"
                      ),
                      multiple = FALSE
                    ),
                    
                    selectInput(
                      inputId = "CM_SVM_diffmeth",
                      label = "SVM diffmeth",
                      width = "100%",
                      selected = "makediff3",
                      choices = c(
                        "makediff1",
                        "makediff2",
                        "makediff3"
                      ),
                      multiple = FALSE
                    ),
                    
                    selectInput(
                      inputId = "CM_SVM_optmeth",
                      label = "SVM optmeth",
                      width = "100%",
                      choices = c(
                        "quadprog",
                        "ipop"
                      ),
                      multiple = FALSE
                    ),
                    
                    selectInput(
                      inputId = "CM_SVM_kernel",
                      label = "SVM kernel",
                      width = "100%",
                      choices = c(
                        "add_kernel",
                        "lin_kernel",
                        "rbf_kernel",
                        "poly_kernel"
                      ),
                      multiple = FALSE
                    ),
                    
                    id = "CMSVMSettingDiv",
                    style = "height:160px;overflow-y:auto;"
                  ),
                  style = "width:18%"
                ),
                column(
                  2,
                  ###### plsRcox #####
                  div(
                    p("plsRcox",
                      style = "font-weight:bold;margin-bottom:1px;"),
                    style = "background-color:#fae8b1;text-align:center;"
                  ),
                  
                  tags$hr(style="margin-top:0px;height:3px;"),
                  
                  div(
                    selectInput(
                      inputId = "CM_plsRcox_lambda_rule",
                      label = "plsRcox lambda rule",
                      width = "100%",
                      choices = c(
                        "lambda.min",
                        "lambda.1se"
                      ),
                      multiple = FALSE
                    ),
                    
                    id = "CMplsRcoxSettingDiv",
                    style = "height:160px;overflow-y:auto;"
                  ),
                  style = "width:18%"
                ),
                column(
                  2,
                  ####### SuperPC ######
                  div(
                    p("SuperPC",
                      style = "font-weight:bold;margin-bottom:1px;"),
                    style = "background-color:#fae8b1;text-align:center;"
                  ),
                  
                  tags$hr(style="margin-top:0px;height:3px;"),
                  
                  div(
                    
                    sliderInput(
                      inputId = "CM_SuperPC_ncomponents",
                      label = "SuperPC ncomponents",
                      min = 1,
                      max = 3,
                      step = 1,
                      value = 1,
                      animate = TRUE,
                      width = "100%"
                    ),
                    
                    id = "CMSuperPCSettingDiv",
                    style = "height:160px;overflow-y:auto;"
                  ),
                  style = "width:18%"
                ),
                column(
                  1,
                  style = "width:5%"
                )
              ),
              p("  "),
              
              id = "CMPageInfoCard",
              title = "6. Model construction algorithm settings",
              icon = icon("cogs"),
              collapsible = FALSE,
              width = 12,
              closable = FALSE,
              maximizable = FALSE
            ),
            style = "display:none;"
          ),
          
          
          ## model construction setting, including traning
          tags$br(),
          fluidRow(
            column(
              6,
              p("Whether to include the performance of the training dataset to evaluate models",
                style = "color:black;"),
            ),
            column(
              1,
              # shinyWidgets::materialSwitch(
              #   inputId = "AnalysisPageConsensusModelModuleIncludeTrainingDataInput",
              #   label = NULL,
              #   value = TRUE,
              #   status = "warning"
              # ),
              switchInput(
                inputId = "AnalysisPageConsensusModelModuleIncludeTrainingDataInput",
                value = TRUE,
                label = NULL,
                onStatus = "warning",
                offStatus = "fail",
                onLabel = "Yes",
                offLabel = "No",
                size = "normal",
                inline = TRUE
                
              ),
              style = "padding-top:6px;"
            ), 
            column(
              5
            )
          ),
              
        ),
        
        # btn here
        tags$br(),
        div(
          fluidRow(
            column(2),
            column(
              8,
              shinyWidgets::actionBttn(
                inputId = "AnalysisPageConsensusModelModuleModelScorePlotSubmitBtn",
                label = "Everything is ready! Start Modeling",
                icon = icon("rocket"),
                color = "warning",
                style = "unite",
                block = TRUE
              )
            ),
            column(2)
          ),
          tags$br(),
          
          id = "AnalysisPageConsensusModelModuleModelScorePlotSubmitBtnDiv",
          # style = "display:none;"
        ),
        
        tags$br(),
      ),
      
      div(
        id = "AnalysisPageConsensusModelModuleModelScorePlotDiv",
        
        tags$hr(),
        tags$br(),
        
        fluidRow(
          # use shinyfeedback
          useShinyFeedback(),
          
          ##### score plot 10models #####
          column(
            8,
            
            div(
              htmlOutput(
                outputId = "AnalysisPageConsensusModelModuleModelScorePlotHtml"
              ),
              
              p("  "),
              
              span(
                shiny::downloadButton(
                  outputId = "AnalysisPageConsensusModelModuleModelScorePlotDownloadPdfBtn",
                  label = "Download PDF",
                  icon = icon("file-pdf-o"),
                  style = "color:#eac428;background-color:#ffffff;"
                ),
                shiny::downloadButton(
                  outputId = "AnalysisPageConsensusModelModuleModelScorePlotDownloadPngBtn",
                  label = "Download PNG",
                  icon = icon("file-image-o"),
                  style = "color:#eac428;background-color:#ffffff;"
                ),
              ),
              
              
              id = "AnalysisPageConsensusModelModuleModelScorePlotHtmlDiv",
              style = "display:none;"
            )
            
          ),
          
          ##### arguments in the right panel #####
          # user input to show 4 plots
          column(
            4,
            
            ###### > optimal model ######
            fluidRow(
              column(
                6,
                span("The optimal model is:",
                     style = "font-size:120%;color:black;font-weight:bold;")
              ),
              column(
                6,
                # best method
                htmlOutput(
                  outputId = "AnalysisPageConsensusModelModuleOptimalModel"
                )
              )
            ),
           
            
            
            ###### > model evaluation conf #######
            # title
            tags$hr(),
            tags$span("You can select a model to see how well the model performs on a", style = "font-size:110%;font-weight:bold;"),
            tags$span("particular dataset",style = "font-size:110%;font-weight:bold;color:#eac428;"),
            tags$br(),
            
            div(
              div(
                # dataset user input, same with validation set
                # shinyWidgets::pickerInput(
                #   inputId = "AnalysisPageConsensusModelModule4PlotsDatasetSelection",
                #   label = "Cohort", 
                #   width = "100%", 
                #   choices = c(""),
                #   options = list(
                #     `live-search` = TRUE, # with search utility
                #     title = "Select a cohort" # the placeholder
                #   )
                # ),
                shiny::selectInput(
                  inputId = "AnalysisPageConsensusModelModule4PlotsDatasetSelection",
                  label = "Cohort",
                  width = "100%",
                  choices = c("")
                ),
                
                # algorithm user input
                # pick a method
                selectInput(
                  inputId = "AnalysisPageConsensusModelModule4PlotsModelMethodSelection",
                  label = "Select an algorithm",
                  choices = c(
                    "StepCox", 
                    "RSF", 
                    "Lasso", 
                    "Ridge", 
                    "Enet", 
                    "GBM", 
                    "SVM", 
                    "plsRcox", 
                    "Coxboost", 
                    "SuperPC"
                  ),
                  width = "100%"
                ),
                
                # plot color
                selectInput(
                  inputId = "AnalysisPageConsensusModelModule4PlotsKMPlotColorSelection",
                  label = "Color palette",
                  choices = c("npg","nejm","jco","d3","lancet","jama"),
                  width = "100%"
                ),
                
                # alpha
                sliderInput(
                  inputId = "AnalysisPageConsensusModelModule4PlotsKMPlotTransparencySelection",
                  label = "Plot transparency",
                  min = 0.2,
                  max = 1,
                  value = 0.8,
                  step = 0.1,
                  ticks = TRUE,
                  animate = TRUE,
                  width = "100%"
                ),
                
                # time point for dca
                selectInput(
                  inputId = "AnalysisPageConsensusModelModule4PlotsDCAPlotTimePoint",
                  label = "Timepoint for DCA",
                  choices = c(),
                  width = "100%"
                ),
                
                # km method
                selectInput(
                  inputId = "AnalysisPageConsensusModelModule4PlotsKMPlotMethodSelection",
                  label = "Group cutoff method for KM analysis",
                  width = "100%",
                  choices = c('median','mean','quantile','optimal','custom')
                ),
                
                # cutoff
                div(
                  sliderInput(
                    inputId = "AnalysisPageConsensusModelModule4PlotsCutoffSelection",
                    label = "Cutoff value",
                    width = "100%",
                    min = 0.1,
                    max = 0.9,
                    value = 0.3,
                    step = 0.05,
                    ticks = T,
                    animate = T
                  ),
                  
                  id = "AnalysisPageConsensusModelModule4PlotsCutoffSelectionDiv",
                  class = "hidden-element"
                ),
                style = "width:95%;"
              ),
              style = "height:560px; overflow-y:auto;"
            )
            
            # # algorithm user input
            # shinyWidgets::actionBttn(
            #   inputId = "AnalysisPageConsensusModelModule4PlotsSubmitBtn",
            #   label = "Plot", 
            #   icon = icon("check"),
            #   style = "jelly",
            #   color = "warning",
            #   size = "sm"
            # )
          ),
          
          ##### model evaluation btn #####
          # evaluation model btn
          p(""),
          fluidRow(
            column(
              2
            ),
            column(
              8,
              shinyWidgets::actionBttn(
                inputId = "AnalysisPageConsensusModelModuleModelEvaluationSubmitBtn",
                size = "md",
                label = "Everything is ready! Start Model Evaluation",
                icon = icon("shield"),
                style = "unite",
                color = "warning",
                block = TRUE
              )
            ),
            column(
              2
            )
          )
          
        ),
        
        # hide it unless run btn is clicked
        class = "hidden-element"
      ),
        
      #### 4 plots (3 plots) ####
      div(
        
        ## space
        tags$hr(),
        tags$br(),
        
        # title of plots
        fluidRow(
          column(
            3,
            div(
              tags$p("Kaplan-Meier Analysis",style="color:#ffffff;"),
              style = "text-align:center;background-color:#4ea653;"
            )
          ),
          column(
            3,
            div(
              tags$p("Time-dependent ROC",style="color:#ffffff;"),
              style = "text-align:center;background-color:#0aa9a2;"
            )
          ),
          column(
            3,
            div(
              tags$p("Calibration Curve",style="color:#ffffff;"),
              style = "text-align:center;background-color:#ff6b6b;"
            )
          ),
          column(
            3,
            div(
              tags$p("Decision Curve Analysis",style="color:#ffffff;"),
              style = "text-align:center;background-color:#fd7e14;"
            )
          )
        ),
        
        # four plots
        fluidRow(
          column(
            3,
            div(
              plotOutput(
                outputId = "AnalysisPageConsensusModelModuleKMPlot",
                height = "340px",
                width = "340px"
              ),
              style = "overflow-x:auto;"
            )
            
          ),
          column(
            3,
            div(
              plotOutput(
                outputId = "AnalysisPageConsensusModelModuleTimeROCPlot",
                height = "340px",
                width = "340px"
              ),
              style = "overflow-x:auto;"
            )
            
          ),
          column(
            3,
            div(
              plotOutput(
                outputId = "AnalysisPageConsensusModelModuleCalCurvePlot",
                height = "340px",
                width = "340px"
              ),
              style = "overflow-x:auto;"
            )
            
          ),
          column(
            3,
            div(
              plotOutput(
                outputId = "AnalysisPageConsensusModelModuleDCAPlot",
                height = "340px",
                width = "340px"
              ),
              style = "overflow-x:auto;"
            )
          )
        ),
        
        # space
        tags$br(),
        
        # button group for saving plots
        fluidRow(
          span(
            shiny::downloadButton(
              outputId = "AnalysisPageConsensusModelModuleSavePdfDownloadBtn",
              label = "Download PDF",
              icon = icon("file-pdf-o"),
              style = "color:#eac428;background-color:#ffffff;"
            ),
            shiny::downloadButton(
              outputId = "AnalysisPageConsensusModelModuleSavePngDownloadBtn",
              label = "Download PNG",
              icon = icon("file-image-o"),
              style = "color:#eac428;background-color:#ffffff;"
            )
          ),
          # column(
          #   3,
          #   shinyWidgets::dropdown(
          #     # content of the dropdown
          #     tags$h6("Save the plots with different formats"),
          #     tags$br(),
          #     
          #     # save pdf
          #     # shinyWidgets::actionBttn(
          #     #   inputId = "AnalysisPageConsensusModelModuleSavePdfBtn",
          #     #   icon = icon("file-pdf-o"),
          #     #   label = "PDF",
          #     #   color = "warning",
          #     #   style = "pill",
          #     #   size = "sm"
          #     # ),
          #     
          #     shiny::downloadButton(
          #       outputId = "AnalysisPageConsensusModelModuleSavePdfDownloadBtn",
          #       label = "PDF",
          #       class = "btn-warning",
          #       icon = icon("file-pdf-o")
          #     ),
          #     
          #     # save pdf
          #     # shinyWidgets::actionBttn(
          #     #   inputId = "AnalysisPageConsensusModelModuleSaveImgBtn",
          #     #   icon = icon("file-image-o"),
          #     #   label = "TIF",
          #     #   color = "warning",
          #     #   style = "pill",
          #     #   size = "sm"
          #     # ),
          #     
          #     shiny::downloadButton(
          #       outputId = "AnalysisPageConsensusModelModuleSaveTiffDownloadBtn",
          #       label = "Tiff",
          #       class = "btn-warning",
          #       icon = icon("file-image-o")
          #     ),
          #     
          #     ## settings of the dropdown button
          #     icon = icon("save"),
          #     label = "Save plots",
          #     size = "sm",
          #     style = "unite",
          #     status = "warning",
          #     animate = animateOptions(
          #       enter = animations$fading_entrances$fadeInDown,
          #       exit = animations$fading_exits$fadeOutUp
          #     )
          #   )
          # ),
          # column(
          #   9
          # )
        ),
        
        id = "AnalysisPageConsensusModelModule4PlotsDiv",
        class = "hidden-element"
      ),
      
      ## space
      tags$hr(),
      tags$br(),
      
      ## single gene analysis tab pages
      ui.page_module_consensus_model_down_section()
    )
  
}
