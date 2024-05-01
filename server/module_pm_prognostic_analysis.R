# this script render the server of analysis page of 
# module prognostic model, analysis prognostic analysis

### set up with server end

#### specific pkgs and options ####
options(digits = 2)

#### user input ####
# cut off value
observeEvent(input$PrognosticModelPageSubmodulePrognosticAnalysisGroupSplitMethodSelection,{
  
  ## show/hide slider input, according to group split method
  if(input$PrognosticModelPageSubmodulePrognosticAnalysisGroupSplitMethodSelection == "custom"){
    shinyjs::show(
      id = "PrognosticModelPageSubmodulePrognosticAnalysisCutoffValuePercentSelectionDiv",
      anim = TRUE,
      animType = "fade"
    )
    shinyjs::show(
      id = "PrognosticModelPageSubmodulePrognosticAnalysisCutoffValuePercentSelectionTitleDiv",
      anim = TRUE,
      animType = "fade"
    )
  }
  
  if(input$PrognosticModelPageSubmodulePrognosticAnalysisGroupSplitMethodSelection != "custom"){
    shinyjs::hide(
      id = "PrognosticModelPageSubmodulePrognosticAnalysisCutoffValuePercentSelectionDiv",
      anim = TRUE,
      animType = "fade"
    )
    shinyjs::hide(
      id = "PrognosticModelPageSubmodulePrognosticAnalysisCutoffValuePercentSelectionTitleDiv",
      anim = TRUE,
      animType = "fade"
    )
  }
  
})

#### start survival analysis ####
observeEvent(input$PrognosticModelPageSubmodulePrognosticAnalysisSubmitBtn,{
  
  # return nothing if not clicked
  if(input$PrognosticModelPageSubmodulePrognosticAnalysisSubmitBtn == 0){
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
  
  
  # isolate calculation
  input$PrognosticModelPageSubmodulePrognosticAnalysisSubmitBtn
  
  isolate({
    
    library(survival)
    library(survminer)
    
    #### prepare data first ####
    # send reactive values in here
    total_clin_list <- cancer.data$total_clin_list
    gtf <- gtf_data$gtf
    self_name <- PrognosticModelReactiveValues$self_name
    score_list <- PrognosticModelReactiveValues$score_list
    Sur_ids <- c('OS','OS.time','RFS','RFS.time','DFS','DFS.time',
                 'PFS','PFS.time','DSS','DSS.time') ## 生存变量ID
    
    all_cohort_name <- PrognosticModelReactiveValues$all_cohort_name
    total_clin_list <- total_clin_list[all_cohort_name]
    names <- names(total_clin_list)
    
    
    # -------------------------------------------------------------------------
    ### 汇总临床变量 ###
    # -------------------------------------------------------------------------
    
    clin_var_list <- lapply(total_clin_list,function(x){colnames(x)[-1]})
    clin_vars <- data.frame()
    for (i in names) {
      clin_vars <- rbind(clin_vars,data.frame(Cohort=i,Var=clin_var_list[[i]]))
    }
    clin_vars <- clin_vars[order(clin_vars$Var),]
    Sur_vars <- clin_vars[clin_vars$Var%in%Sur_ids,]
    Sur_ids <- unique(Sur_vars$Var)
    
    # -------------------------------------------------------------------------
    ### Prepare score ###
    # -------------------------------------------------------------------------
    
    score_list <- lapply(score_list,function(x){
      x <- data.frame(ID=rownames(x),Score=x[,3])
      return(x)
    })
    
    # -------------------------------------------------------------------------
    ### Prognostic Significance ###
    # -------------------------------------------------------------------------
    
    #### set up user input ####
    # plot color palette
    Color_palette <- input$PrognosticModelPageSubmodulePrognosticAnalysisPlotColorSelection
    
    # plot transparency
    alpha <- input$PrognosticModelPageSubmodulePrognosticAnalysisPlotTransparencySelection
    
    # set colors
    if (Color_palette == 'npg') {
      cols <- pal_npg(alpha = alpha)(10)
    }
    if (Color_palette == 'nejm') {
      cols <- pal_nejm(alpha = alpha)(8)
    }
    if (Color_palette == 'jco') {
      cols <- pal_jco(alpha = alpha)(10)
    }
    if (Color_palette == 'd3') {
      cols <- pal_d3(alpha = alpha)(10)
    }
    if (Color_palette == 'lancet') {
      cols <- pal_lancet(alpha = alpha)(9)
    }
    if (Color_palette == 'jama') {
      cols <- pal_jama(alpha = alpha)(7)
    }
    
    # survival analysis settings
    cutoff <- input$PrognosticModelPageSubmodulePrognosticAnalysisGroupSplitMethodSelection
    percent <- input$PrognosticModelPageSubmodulePrognosticAnalysisCutoffValuePercentSelection
    
    Sur_ids2 <- gsub('.time', '', Sur_ids) %>% unique()
    Sur_vars$Var <- gsub('.time', '', Sur_vars$Var)
    Sur_vars <- distinct(Sur_vars, Cohort, Var, .keep_all = T)
    
    # generate plots first
    plotsur_list <- list()
    coxr <- data.frame()
    counter <- 0
    for (i in Sur_ids2) {
      for (j in Sur_vars$Cohort[Sur_vars$Var == i]) {
        counter = counter + 1
        tmp <- na.omit(merge(total_clin_list[[j]][, c('ID', i, paste0(i, '.time'))], score_list[[j]], by = 1)[, -1])
        tmp <- as.data.frame(apply(tmp, 2, as.numeric))
        fit <- summary(coxph(gsurv(select_survars = i), tmp))
        coxr <-
          rbind(
            coxr,
            data.frame(
              Sur_var = i,
              Cohort = j,
              HR = fit$conf.int[, 1],
              HRL = fit$conf.int[, 3],
              HRH = fit$conf.int[, 4],
              P = fit$coefficients[, 5]
            )
          )
        plotsur_list[[paste0(i, '-', j)]] <-
          lzq_survplot2(
            Sur_ids = colnames(tmp),
            cohort = j,
            data = tmp,
            gene = 'Score',
            Input = self_name,
            cols = cols,
            cutoff = cutoff,
            percent = percent
          )
      }
    }
    
    # cox plot
    plot.cox <- lzq_coxplot(coxr)
    plotHeightCox = paste(counter * 20 + 100,"px",sep="")
    
    # surv plot  
    plotHeightSurvival = paste(counter / 3 * 385,"px",sep="")
    plotWidthSurvival = paste0(ifelse(counter > 3, 3, counter) * 350,"px")
    plot.survival.out <- plot_grid(
      plotlist = plotsur_list,
      ncol = 3
    )
    
    ## plot html output
    # cox plot
    output$PrognosticModelPageSubmodulePrognosticAnalysisResultCoxPlotHtml <- renderUI({
      div(
        tags$br(),
        plotOutput(
          outputId = "PrognosticModelPageSubmodulePrognosticAnalysisCoxPlot",
          height = plotHeightCox,
          # width = "50%"
          width = "300px"
        ),
        
        style = "height:400px; overflow-y:scroll; overflow-x:auto;"
      )
    })
    
    output$PrognosticModelPageSubmodulePrognosticAnalysisCoxPlot <- renderPlot({
      return(plot.cox)
    })
    
    # KM plot
    output$PrognosticModelPageSubmodulePrognosticAnalysisResultKMPlotHtml <- renderUI({
      div(
        plotOutput(
          outputId = "PrognosticModelPageSubmodulePrognosticAnalysisKMPlot",
          height = plotHeightSurvival,
          width = plotWidthSurvival
        ),
        
        style = "height:400px; overflow-y:scroll; overflow-x:auto;"
      )
    })
    
    output$PrognosticModelPageSubmodulePrognosticAnalysisKMPlot <- renderPlot({
      return(plot.survival.out)
    })
    
    ## download plot
    # download cox
    output$PrognosticModelPageSubmodulePrognosticAnalysisResultCoxPlotDownloadPdfBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_PrognosticModel_SurvivalAnalysis_CoxPlot_",
            self_name,
            "_",
            uniqID(),
            ".pdf",
            sep = ""
          ),
          " +",
          "_"
        )
      },
      
      content = function(fname) {
        # pdf plot height
        pdfPlotHeight = ceiling(counter * 0.3 + 1)
        # pdfPlotHeight = ceiling(length(plot.surv.list) / 3) * 6
        
        # save plot
        save_plot(
          fname,
          plot.cox,
          base_height = pdfPlotHeight,
          base_width = 5)
      },
      
      
      contentType = ".pdf"
    )
    
    output$PrognosticModelPageSubmodulePrognosticAnalysisResultCoxPlotDownloadPngBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_PrognosticModel_SurvivalAnalysis_CoxPlot_",
            self_name,
            "_",
            uniqID(),
            ".png",
            sep = ""
          ),
          " +",
          "_"
        )
      },
      
      content = function(fname) {
        # pdf plot height
        pngPlotHeight = ceiling(counter * 0.3 + 1)
        
        # save plot
        save_plot(
          fname,
          plot.cox,
          base_height = pngPlotHeight,
          base_width = 5)
      },
      
      
      contentType = ".png"
    )
    
    ## download km plot pdf
    # generate and save plots
    # create folders
    pdf.dir.name = paste("BEST_PrognosticModel_SurvivalAnalysis_KM_Plot_",self_name,"_",uniqID(),sep="")
    tmp.dir = tempdir()
    pdf.plot.file.path = paste(tmp.dir,"/",pdf.dir.name,sep="")
    dir.create(pdf.plot.file.path,recursive = FALSE)
    
    
    lapply(1:counter, function(i){
      dataset.name <- names(plotsur_list)[i]
      p <- plotsur_list[[i]]
      
      # save plot
      save_plot(
        filename = paste(pdf.plot.file.path,"/Plot_",dataset.name,".pdf",sep=""),
        plot = p,
        base_height = 5.5,
        base_width = 5
      )
    })
    
    output$PrognosticModelPageSubmodulePrognosticAnalysisResultKMPlotDownloadPdfBtn <- downloadHandler(
      filename = function(){
        paste(pdf.dir.name,"*.zip",sep="")
      },
      
      content = function(filename){
        wd.now = getwd()
        setwd(tmp.dir)
        
        zip(filename, files = pdf.dir.name)
        
        setwd(wd.now)
      },
      
      contentType = ".zip"
    )
    
    ## download km plot png
    # generate and save plots
    # create folders
    png.dir.name = paste("BEST_PrognosticModel_SurvivalAnalysis_KM_Plot_",self_name,"_",uniqID(),sep="")
    tmp.dir = tempdir()
    png.plot.file.path = paste(tmp.dir,"/",png.dir.name,sep="")
    dir.create(png.plot.file.path,recursive = FALSE)
    
    
    lapply(1:counter, function(i){
      dataset.name <- names(plotsur_list)[i]
      p <- plotsur_list[[i]]
      
      # save plot
      save_plot(
        filename = paste(png.plot.file.path,"/Plot_",dataset.name,".png",sep=""),
        plot = p,
        base_height = 5.5,
        base_width = 5
      )
    })
    
    output$PrognosticModelPageSubmodulePrognosticAnalysisResultKMPlotDownloadPngBtn <- downloadHandler(
      filename = function(){
        paste(png.dir.name,"*.zip",sep="")
      },
      
      content = function(filename){
        wd.now = getwd()
        setwd(tmp.dir)
        
        zip(filename, files = png.dir.name)
        
        setwd(wd.now)
      },
      
      contentType = ".zip"
    )
    
    #### generate plots one by one and save with zip, 
    #### do NOT work well, full of bugs
    # # generate plots UI
    # output$PrognosticModelPageSubmodulePrognosticAnalysisResultPlotHtml <- renderUI(
    #   
    #   div(
    #     style = "height:500px;overflow-y:scroll;",
    #     
    #     # plot ui
    #     fluidRow(
    #       width = 12,
    #       
    #       # generate plots, number equal to datasets num
    #       lapply(1:nrow(Sur_vars), function(i){
    #         column(
    #           width = 4,
    #           plotOutput(
    #             outputId = paste("PrognosticModelPageSubmodulePrognosticAnalysisResultPlot",i,sep="")
    #           )
    #         )
    #       })
    #     )
    #   )
    # )
    # 
    # # generate and save plots
    # # create folders
    # dir.name = paste("BEST_PrognosticModel_PrognosticAnalysis_",geneInput$geneName,"_",uniqID(),sep="")
    # tmp.dir = tempdir()
    # plot.file.path = paste(tmp.dir,"/",dir.name,sep="")
    # dir.create(plot.file.path,recursive = FALSE)
    # 
    # # generate plots
    # tmp.list <- list()
    # j.list <- list()
    # surv.type <- list()
    # 
    # counter = 1
    # for (i in Sur_ids2) {
    #   for (j in Sur_vars$Cohort[Sur_vars$Var == i]) {
    #     tmp <- na.omit(
    #       merge(
    #         total_clin_list[[j]][, c('ID', i, paste0(i, '.time'))],
    #         t(total_expr_list[[j]][geneInput$geneID, ]),
    #         by.x = 1,
    #         by.y = 0
    #       )[, -1]
    #     )
    #     tmp <- as.data.frame(apply(tmp, 2, as.numeric))
    #     
    #     tmp.list[[counter]] = tmp
    #     j.list[[counter]] = j
    #     surv.type[[counter]] = i
    #     
    #     counter = counter + 1
    #   }
    # }
    #   
    # #print(j.list)
    # #print(tmp.list)
    # 
    # 
    # lapply(1:nrow(Sur_vars),function(i){
    #   output[[paste("PrognosticModelPageSubmodulePrognosticAnalysisResultPlot", i, sep = "")]] <- renderPlot({
    #     
    #     # save plot
    #     pdf(paste(plot.file.path,"/",j.list[[i]],"_",surv.type[[i]],".pdf",sep=""),width = 6,height = 8)
    #     lzq_survplot2(
    #       Sur_ids = colnames(tmp.list[[i]]),
    #       cohort = j.list[[i]],
    #       data = tmp.list[[i]],
    #       gene = gene,
    #       Input = Input,
    #       cols = cols,
    #       cutoff = cutoff,
    #       percent = percent
    #     )
    #     dev.off()
    #     
    #     # return plot
    #     lzq_survplot2(
    #       Sur_ids = colnames(tmp.list[[i]]),
    #       cohort = j.list[[i]],
    #       data = tmp.list[[i]],
    #       gene = gene,
    #       Input = Input,
    #       cols = cols,
    #       cutoff = cutoff,
    #       percent = percent
    #     )
    #     
    #   })
    # })
    #     
    # 
    # # lapply(1:nrow(cancer.dataset()), function(i){
    # #   output[[paste("PrognosticModelPageSubmodulePrognosticAnalysisResultPlot",i,sep="")]] <- renderPlot({
    # #     p = demoPlot()
    # #     
    # #     # save plot
    # #     save_plot(
    # #       filename = paste(plot.file.path,"/Plot",i,".pdf",sep=""),
    # #       plot = p,
    # #       base_height = 5,
    # #       base_width = 5
    # #     )
    # #     
    # #     # return plot
    # #     return(p)
    # #   })
    # # })
    # 
    # # download plots
    # output$PrognosticModelPageSubmodulePrognosticAnalysisResultPlotDownloadBtn <- downloadHandler(
    #   filename = function(){
    #     paste(dir.name,"*.zip",sep="")
    #   },
    # 
    #   content = function(filename){
    #     wd.now = getwd()
    #     setwd(tmp.dir)
    # 
    #     zip(filename, files = dir.name)
    # 
    #     setwd(wd.now)
    #   },
    # 
    #   contentType = ".zip"
    # )
    # 
    # # remove dir
    # # file.remove(plot.file.path)
    
    # show plots
    shinyjs::show(
      id = "PrognosticModelPageSubmodulePrognosticAnalysisResultPlotDiv",
      anim = TRUE,
      animType = "fade"
    )
    
  }) # within isolate
  
  
  
})


