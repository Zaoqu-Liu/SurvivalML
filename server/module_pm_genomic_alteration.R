# this script render the server of analysis page of 
# module prognostic model, analysis genomic alteration

# set up with server end
#### specific pkgs and options ####

#### check user input ####
# nothing to check before start btn clicked

#### start analysis ####
observeEvent(input$PrognosticModelPageSubmoduleGenomicAlterationSubmitBtn,{
  # return nothing if not clicked
  if(input$PrognosticModelPageSubmoduleGenomicAlterationSubmitBtn == 0){
    return()
  }
  
  # return if no valid gene score
  if(any(is.null(PrognosticModelReactiveValues$score_list))){
    sendSweetAlert(
      session = session,
      title = "Error",
      text = "No valid gene set score. Please check.",
      type = "error",
      btn_labels = "OK",
      closeOnClickOutside = T
    )
    
    return()
  }
  
  input$PrognosticModelPageSubmoduleGenomicAlterationSubmitBtn
  
  isolate({
    
    library(ComplexHeatmap)
    library(circlize)
    
    
    ##### process with data #####
    # send reactive valus in 
    score_list <- PrognosticModelReactiveValues$score_list
    self_name <- PrognosticModelReactiveValues$self_name
    
    # load data
    # "cna_ann"        "tcga_multiomic"
    load(paste(
      "./db/raw_data/",
      str_replace_all(globReactiveValues$homePageCancerSelected, " +", "_"),
      "/tcga_omics.rda",
      sep = ""
    ))
    
    cohort <- names(score_list)[grepl('TCGA', names(score_list))]
    if (length(cohort) == 0) {
      # message('Missing TCGA dataset in the cohorts where you calculated the score')
      sendSweetAlert(
        session = session,
        type = "error",
        title = "Error",
        text = "Missing TCGA dataset in the cohorts where you calculated the score. Please re-select.",
        btn_labels = "OK",
        closeOnClickOutside = TRUE
      )
      return()
    }
    
    # send global reactive values in 
    exp <- score_list[[cohort]] %>% tibble::rownames_to_column('ID')
    exp <- exp[order(exp$score), ]
    exp <- exp[substr(exp$ID, 14, 16) == '01A', ]
    exp$ID <- substr(exp$ID, 1, 12)
    exp <- exp[exp$ID %in% colnames(tcga_multiomic), ]
    
    # plot made by complexheatmap
    plot.gene.alteration = lzq_alter_landscape(rank = exp[, c('ID', 'score')],
                                               Input = self_name,
                                               alterdata = tcga_multiomic,
                                               cnadata = cna_ann)
    
    
    # output plot
    output$PrognosticModelPageSubmoduleGenomicAlterationHeatmap <- renderPlot({
      return(plot.gene.alteration)
    })
    
    # download plot
    # download plot
    output$PrognosticModelPageSubmoduleGenomicAlterationHeatmapDownloadPdfBtn <- downloadHandler(
      filename = function() {
        fname = paste(
          "BEST_PrognosticModel_GeneAlteration_",
          globReactiveValues$homePageCancerSelected,
          "_",
          self_name,
          "_",
          uniqID(idLength = 10),
          ".pdf"
        )
        return(str_remove_all(fname,"\\s+"))
      },
      
      
      content = function(fname){
        pdf(fname, height = 10, width = 12)
        
        # draw plot
        draw(
          plot.gene.alteration,
          padding = unit(c(2, 2, 2, 2), "mm") # add space around the plot
        )
        
        # close device and save plot
        dev.off()
      },
      
      contentType = "pdf"
    )
    
    output$PrognosticModelPageSubmoduleGenomicAlterationHeatmapDownloadPngBtn <- downloadHandler(
      filename = function() {
        fname = paste(
          "BEST_PrognosticModel_GeneAlteration_",
          globReactiveValues$homePageCancerSelected,
          "_",
          self_name,
          "_",
          uniqID(idLength = 10),
          ".png"
        )
        return(str_remove_all(fname,"\\s+"))
      },
      
      
      content = function(fname){
        png(fname, height = 1000, width = 1200)
        
        # draw plot
        draw(
          plot.gene.alteration,
          padding = unit(c(2, 2, 2, 2), "mm") # add space around the plot
        )
        
        # close device and save plot
        dev.off()
      },
      
      contentType = "png"
    )
    
    
    shinyjs::show(
      id = "PrognosticModelPageSubmoduleGenomicAlterationHeatmapDiv",
      anim = TRUE,
      animType = "fade"
    )
    
  }) # within isolate
  
})