### This file is the server-end of dataset page

# read in dataset info
primary.site = openxlsx::read.xlsx("./db/primary_site_datasets_summary.xlsx", sheet = 1)

dataset.info <- openxlsx::read.xlsx("./db/datainfo.xlsx",sheet = 1)
head(dataset.info)

# reactive values
cancer.selected <- reactive({
  if(input$PageDatasetCanerTypeSelection == "All"){
    return("All")
  }else{
    return(primary.site$Abbreviation[input$PageDatasetCanerTypeSelection == primary.site$Cancer])
  }
})

cancer.dataset.num <- reactive({
  if(cancer.selected() == "All"){
    return(nrow(dataset.info))
  }else{
    return(sum(dataset.info$Cancer == cancer.selected()))
  }
})

# patient.num <- reactive({
#   if(cancer.selected() == "All"){
#     return(sum(dataset.info$Number.of.samples))
#   }else{
#     return(sum(dataset.info$Number.of.samples[dataset.info$Cancer == cancer.selected()]))
#   }
# })

case.survival.num <- reactive({
  if(cancer.selected() == "All"){
    return(sum(dataset.info$Number.of.samples.with.survival.info))
  }else{
    return(sum(dataset.info$Number.of.samples.with.survival.info[dataset.info$Cancer == cancer.selected()]))
  }
})

rnaseq.dataset.num <- reactive({
  if(cancer.selected() == "All"){
    return(sum(dataset.info$Technology == "RNA-seq"))
  }else{
    return(sum(dataset.info$Cancer == cancer.selected() & dataset.info$Technology == "RNA-seq"))
  }
})

microarray.dataset.num <- reactive({
  if(cancer.selected() == "All"){
    return(sum(dataset.info$Technology == "Microarray"))
  }else{
    return(sum(dataset.info$Cancer == cancer.selected() & dataset.info$Technology == "Microarray"))
  }
})

# # UI output
# output$DatasetPageDatasetTitle <- renderUI(
#   tags$p(paste("Return ", cancer.dataset.num(), " datasets", sep=""),
#          style = "font-size:150%;font-weight:bold;color:#000000;")
# )

# number of datasets
output$DatasetPageDatasetNumber <- renderText({
  return(cancer.dataset.num())
})

# # total number of patients
# output$DatasetPageCaseNumber <- renderText({
#   return(patient.num())
# })

# total number of microarray/rnaseq
output$DatasetPageRNAseqMicroarrayNumber <- renderText({
  return(paste0(rnaseq.dataset.num(),"/",microarray.dataset.num()))
})

output$DatasetPageNumberOfCasesWithSurvival <- renderText({
  return(case.survival.num())
})


## output table
output$DatasetPageSummaryTable <- renderDataTable({
  table.out <- dataset.info
  if(cancer.selected() == "All"){
    # nothing
  }else{
    table.out <- dataset.info[dataset.info$Cancer == cancer.selected(),]
  }
  
  # output table
  DT::datatable(
    table.out,
    rownames = FALSE,
    options = list(pageLength = 10, info = FALSE),
    class = 'cell-border stripe'
  )
})
