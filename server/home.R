# This file defines the server-end in home page

# read in data 
# primary.site = openxlsx::read.xlsx("./db/primary_site_datasets_summary.xlsx", sheet = 1)

## html output which show organs in the left panel
organ.selected <- reactive(
  input$DatasetOrganSelect
)

observeEvent(input$DatasetOrganSelect,{
  card.selected.idx = which(primary.site$Cancer == organ.selected()) # selected card
  # remove class before add one 
  lapply(1:nrow(primary.site), function(i){
    shinyjs::removeClass(
      id=paste("DatasetPageBlock",i,sep=""), 
      class = "cardstrong")
  })
  
  # add class
  shinyjs::addClass(
    id=paste("DatasetPageBlock",card.selected.idx,sep=""), 
    class = "cardstrong")
  
})


output$DatasetOrgan <- renderUI({
  # image of each organ
  # organImgSrc <- paste("./organ/organ_big/"
  #                      ,str_replace_all(organ.selected()," +","_"), # replace space with _
  #                      ".png",
  #                      sep="")
  
  # 固定图像
  organImgSrc <- paste("body.png")
  tags$img(src=organImgSrc,style="height:520px")
})


#### add js to control cancer type selection
## add shiny js on each card when click, remove borders on previous card
lapply(1:nrow(primary.site), function(i){ # primary.site already read in in ui/dataset.R
  observeEvent(input$DatasetOrganSelect,{
    card.selected.idx = which(primary.site$Cancer == organ.selected()) # previously selected card
    shinyjs::onclick(paste("DatasetPageInnerInnerBlock",i,sep=""),
                     ## add border on each card
                     shinyjs::removeClass(id=paste("DatasetPageBlock",card.selected.idx,sep=""), class = "cardstrong")
    )
  })
})


## add shiny js on each card when click, add borders
lapply(1:nrow(primary.site), function(i){ # primary.site already read in in ui/dataset.R
  shinyjs::onclick(paste("DatasetPageBlock",i,sep=""),
                   ## add border on each card
                   shinyjs::toggleClass(id=paste("DatasetPageBlock",i,sep=""), class = "cardstrong")
                   # shiny::updateSelectInput(inputId = "DatasetOrganSelect", selected = primary.site$Primary.Site[i])
  )
  
})

## add shiny js on each card when click, change input select dropdown list
lapply(1:nrow(primary.site), function(i){ # primary.site already read in in ui/dataset.R
  shinyjs::onclick(paste("DatasetPageInnerBlock",i,sep=""),
                   # add border on each card
                   # shinyjs::toggleClass(id=paste("DatasetPageBlock",i,sep=""), class = "cardstrong")
                   shiny::updateSelectInput(inputId = "DatasetOrganSelect", selected = primary.site$Cancer[i])
  )
})

## add shiny js on each card when click, link to bottom
lapply(1:nrow(primary.site), function(i){ # primary.site already read in in ui/dataset.R
  shinyjs::onclick(paste("DatasetPageInnerInnerInnerBlock",i,sep=""),
                   shinyjs::click(id = "PageHomeLinkToModuleSelection")
  )
})


###### add js to control analysis module selection #####
model.selected <- reactive(
  input$HomePageAnalysisTypeSelection
)

observeEvent(input$HomePageAnalysisTypeSelection,{
  card.selected.idx = model.selected() # selected card
  # remove class before add one 
  lapply(3:4, function(i){
    
    # there are totally 4 modules
    # 1 for single gene
    # 2 for gene list
    # 3 for prognosis model
    # 4 for consensus subtype
    
    shinyjs::removeClass(
      id=paste("HomePageAnalysisModule",i,sep=""), 
      class = paste("cardstrong2m",i,sep=""))
  })
  
  # add class
  shinyjs::addClass(
    id=paste("HomePageAnalysisModule",card.selected.idx,sep=""), 
    class = paste("cardstrong2m",card.selected.idx,sep=""))
})

## add shiny js on each card when click, remove borders on previous card
lapply(3:4, function(i){ 
  observeEvent(input$HomePageAnalysisTypeSelection,{
    card.selected.idx = model.selected() # previously selected card
    shinyjs::onclick(paste("HomePageAnalysisModule",i,"InnerInner",sep=""),
                     ## add border on each card
                     shinyjs::removeClass(
                       id=paste("HomePageAnalysisModule",card.selected.idx,sep=""), 
                       class = paste("cardstrong2m",i,sep=""))
    )
  })
})


## add shiny js on each card when click, add borders
lapply(3:4, function(i){ 
  shinyjs::onclick(paste("HomePageAnalysisModule",i,sep=""),
                   ## add border on each card
                   shinyjs::toggleClass(
                     id=paste("HomePageAnalysisModule",i,sep=""), 
                     class = paste("cardstrong2m",i,sep=""))
                   # shiny::updateSelectInput(inputId = "DatasetOrganSelect", selected = primary.site$Primary.Site[i])
  )
})

## add shiny js on each card when click, change input select dropdown list
lapply(3:4, function(i){ 
  shinyjs::onclick(paste("HomePageAnalysisModule",i,"Inner",sep=""),
                   # add border on each card
                   # shinyjs::toggleClass(id=paste("DatasetPageBlock",i,sep=""), class = "cardstrong2")
                   shiny::updateSelectInput(inputId = "HomePageAnalysisTypeSelection", selected = i)
  )
})

## analyze btn 
observeEvent(input$HomePageAnalysisTypeSelection,{
  card.selected.idx = model.selected() # selected card
  # remove class before add one 
  lapply(3:4, function(i){
    
    # there are totally 4 modules
    # 1 for single gene
    # 2 for gene list
    # 3 for prognosis model
    # 4 for consensus subtype
    
    shinyjs::addClass(
      id=paste("HomePageAnalysisGoBtnDivModule",i,sep=""), 
      class = "hidden-element")
  })
  
  # add class
  shinyjs::removeClass(
    id=paste("HomePageAnalysisGoBtnDivModule",card.selected.idx,sep=""), 
    class = "hidden-element")
})

#### reactive values ####
globReactiveValues <- reactiveValues(
  homePageCancerSelected = ""
)

observeEvent(input$DatasetOrganSelect,{
  globReactiveValues$homePageCancerSelected = primary.site$Abbreviation[primary.site$Cancer == input$DatasetOrganSelect]
})