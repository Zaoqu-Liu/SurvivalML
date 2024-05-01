# this script render the UI of [dataset] page
# read in primary site list
primary.site = openxlsx::read.xlsx("./db/primary_site_datasets_summary.xlsx", sheet = 1)


ui.page_dataset <- function() {
  tabPanel(
    title = "Dataset",
    value = "Dataset",
    
    fluidPage(style = "width:80%;",
              tags$br(),
              fluidRow(
                column(
                  12,
                  tags$h2("Dataset Summary"),
                  tags$p(
                    "Detailed information about datasets in each cancer type.",
                    style = "font-size:100%"
                  ),
                  
                  tags$hr(),
                )
              ),
              
              
                  bs4Dash::box(
                    fluidRow(
                      column(
                        3,
                        
                        # cancer type selection
                        selectInput(
                          inputId = "PageDatasetCanerTypeSelection",
                          choices = c("All",primary.site$Cancer),
                          label = "CANCER TYPE",
                          width = "100%"
                        )
                      ),
                      
                      # dataset number
                      column(
                        3,
                        id = "DatasetPageNumberOfDatasets",
                        summaryBox2(title = "Number of datasets",
                                    value = textOutput(outputId = "DatasetPageDatasetNumber"),
                                    width = 12,
                                    style = "success",
                                    icon = "fa fa-bar-chart"),
                      ),
                      
                      column(
                        3,
                        id = "DatasetPageNumberOfRnaseqMicroarrayDatasets",
                        summaryBox2(title = "RNAseq/Microarray",
                                    value = textOutput(outputId = "DatasetPageRNAseqMicroarrayNumber"),
                                    width = 12,
                                    style = "danger",
                                    icon = "fa fa-newspaper-o"),
                      ),
                      
                      column(
                        3,
                        id = "DatasetPageNumberOfSurvivalCases",
                        summaryBox2(title = "Cases With Survival Info",
                                    value = textOutput(outputId = "DatasetPageNumberOfCasesWithSurvival"),
                                    width = 12,
                                    style = "info",
                                    icon = "fa fa-user-plus"),
                      )
                    ),
                    
                    # space
                    tags$br(),
                    
                    # box config
                    collapsible = FALSE, 
                    solidHeader = TRUE,
                    width = 12,
                    id = "PageDatasetCanerTypeCard"
                  ),
                
                
                
              
              tags$br(),

              tags$br(),
              
              # dataset table
              DT::dataTableOutput(outputId = "DatasetPageSummaryTable")
    )
  )
  
}