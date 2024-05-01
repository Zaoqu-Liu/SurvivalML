# this script render the server of analysis page of 
# module consensus model, analysis immunotherapy

# set up with server end
#### specific pkgs and options ####
options(digits = 2)

#### check user input ####
# nothing to check before start btn clicked

#### reactive values ####
ConsensusModelModuleImmunotherapyReactiveVals <- reactiveValues(
  score_list = NULL, # recalculated gene score
  self_name = "Consensus score", # user input gene list name
  select_cohorts = NULL,
  select_cohorts2 = NULL,
  index = NULL
)


#### start analysis ####
##### recalculate gene score #####
observeEvent(input$ConsensusModelPageSubmoduleImmunotherapyRecalGeneScoreSubmitBtn,{
  if(input$ConsensusModelPageSubmoduleImmunotherapyRecalGeneScoreSubmitBtn == 0){
    return()
  }
  
  # return if no valid gene score
  if(any(is.null(ConsensusModelReactiveValues$score_list))){
    sendSweetAlert(
      session = session,
      title = "Error",
      text = "No valid gene set score detected. Please check.",
      type = "error",
      btn_labels = "OK",
      closeOnClickOutside = T
    )
    
    return()
  }
  
  input$ConsensusModelPageSubmoduleImmunotherapyRecalGeneScoreSubmitBtn
  
  isolate({
    
    ## load and import data
    # load data
    load(
      paste0(
        "./db/raw_data/common",
        '/ICI_data-symbol.rda'
      )
    )
    
    # send reactive values in here
    select_ids <- ConsensusModelReactiveValues$select_ids
    score_list <- ConsensusModelReactiveValues$score_list
    gtf <- gtf_data$gtf
    results <- ConsensusModelReactiveValues$results
    self_name <- "Consensus score"
    
    
    # -------------------------------------------------------------------------
    ### 看看多少个队列有这些基因 ###
    # -------------------------------------------------------------------------
    
    index <- sapply(ICI_data,function(x){mean(select_ids%in%rownames(x$expr))==1})
    select_cohorts <- ICI_data[index]
    select_cohorts2 <- lapply(select_cohorts,function(x){as.data.frame(t(x$expr[select_ids,]))})
    
    
    if(length(select_cohorts2) == 0){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "No ICI dataset includes input genes. Please check and enter some other genes.",
        type = "error",
        btn_labels = "OK",
        closeOnClickOutside = TRUE
      )
      return()
    }
    
    
    # save values
    ConsensusModelModuleImmunotherapyReactiveVals$select_cohorts <- select_cohorts
    ConsensusModelModuleImmunotherapyReactiveVals$select_cohorts2 <- select_cohorts2
    ConsensusModelModuleImmunotherapyReactiveVals$index <- index
    
    # -------------------------------------------------------------------------
    ### calculating risk score ###
    # -------------------------------------------------------------------------
    score_list <- lzq_refit(results, testdata = select_cohorts2)

    # save value
    ConsensusModelModuleImmunotherapyReactiveVals$score_list = score_list
    
    
    # show the big div
    shinyjs::show(
      id = "ConsensusModelPageSubmoduleImmunotherapyTotalTabDiv",
      anim = TRUE,
      animType = "fade"
    )
    
  })# within isolate
  
})


#### gene expression ####
observeEvent(input$ConsensusModelPageSubmoduleImmunotherapyGeneExpTabSubmitBtn,{
  # return nothing if not clicked
  if(input$ConsensusModelPageSubmoduleImmunotherapyGeneExpTabSubmitBtn == 0){
    return()
  }
  
  # return if no valid gene score
  if(any(is.null(ConsensusModelModuleImmunotherapyReactiveVals$score_list))){
    sendSweetAlert(
      session = session,
      title = "Error",
      text = "No valid gene set score detected in ICI data sets. Please recalculate.",
      type = "error",
      btn_labels = "OK",
      closeOnClickOutside = T
    )
    
    return()
  }
  
  # isolate calculation
  input$ConsensusModelPageSubmoduleClinlcalAssociationSubmitBtn
  
  isolate({
    
    # load and import data
    # send reactive values in here
    self_name <- ConsensusModelModuleImmunotherapyReactiveVals$self_name
    Gene_Input <- ConsensusModelReactiveValues$Gene_Input
    gtf <- gtf_data$gtf
    score_list <- ConsensusModelModuleImmunotherapyReactiveVals$score_list
    select_cohorts <- ConsensusModelModuleImmunotherapyReactiveVals$select_cohorts
    select_cohorts2 <- ConsensusModelModuleImmunotherapyReactiveVals$select_cohorts2
    
    
    # load data
    load(
      paste0(
        "./db/raw_data/common",
        '/ICI_data-symbol.rda'
      )
    )
    
    # get user input
    Color_palette <- input$ConsensusModelPageSubmoduleImmunotherapyGeneExpTabColorSelection
    alpha <- input$ConsensusModelPageSubmoduleImmunotherapyGeneExpTabAlphaSelection
    
    # set cols
    if(Color_palette == 'npg') {
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
    
    # plot now
    plotbox_list <- list()
    for (i in names(select_cohorts)) {
      dd <- merge(ICI_data[[i]]$clin[,c('ID','Response')],score_list[[i]],by=1)%>%na.omit()
      test_type <- ifelse(shapiro.test(dd[,'score'])$p.val<0.05,'wilcox.test','t.test')
      plotbox_list[[i]] <-
        ggplot(dd, aes_string('Response', 'score')) +
        geom_jitter(
          shape = 21,
          size = 2,
          width = 0.2,
          aes_string(fill = 'Response', color = 'Response')
        ) +
        geom_boxplot(
          outlier.colour = NA,
          aes_string(fill = 'Response'),
          color = 'black',
          size = 0.6,
          alpha = 0.65
        ) +
        geom_violin(
          alpha = 0.5,
          aes_string(fill = 'Response'),
          color = NA,
          trim = T
        ) +
        stat_compare_means(
          label.y = max(dd[, 'score']) * 1.05,
          method = test_type,
          color = 'black',
          size = 5
        ) +
        scale_fill_manual(values = cols) +
        scale_color_manual(values = cols) +
        expand_limits(y = max(dd[, 'score']) * 1.1) +
        theme_bw(base_rect_size = 2) +
        labs(y = self_name, title = paste0(
          gsub(' \\(.*\\)', '', unlist(strsplit(
            ICI_data[[i]]$ann, '---'
          ))[1]),
          '\n',
          gsub('\\(|\\)', '', str_extract(unlist(
            strsplit(ICI_data[[i]]$ann, '---')
          )[1], '\\(.*\\)'))
        )) +
        theme(
          axis.text.y = element_text(size = 11, colour = 'black'),
          axis.text.x = element_text(size = 14, colour = 'black'),
          axis.title.x = element_text(
            size = 15,
            colour = 'darkred',
            face = 'bold'
          ),
          axis.title.y = element_text(
            size = 15,
            colour = 'darkred',
            face = 'bold'
          ),
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "#cacfd2", linetype = "dashed"),
          panel.background = element_rect(fill = '#f3f6f6'),
          plot.title = element_text(
            hjust = 0.5,
            size = 15,
            colour = 'darkred',
            face = 'bold'
          ),
          legend.position = 'none',
          axis.ticks.x = element_blank(),
          plot.margin = margin(
            t = 10,
            b = 10,
            l = 20,
            r = 20
          )
        )
    }
    
    plot.geneExp.out <- plot_grid(plotlist = plotbox_list,ncol = 5)
    plot.num <- length(names(plotbox_list))
    
    # output plot
    output$ConsensusModelPageSubmoduleImmunotherapyGeneExpPlotHtml <- renderUI({
      plot.height <- ceiling(plot.num / 5) * 400
      # plot.width = ifelse(plot.num > 5, 5, plot.num) * 200
      plot.width = 230 * 5 # should be 1000, because 5 cols of figs already defined
      
      div(
        div(
          plotOutput(
            outputId = "ConsensusModelPageSubmoduleImmunotherapyGeneExpPlot",
            #width = paste(div.width,"%",sep=""),
            width = paste0(plot.width,"px"),
            height = paste0(plot.height,"px")
          )
        ),
        style="height:400px;overflow-y:scroll;overflow-x:auto;"
      )
      
    })
    
    output$ConsensusModelPageSubmoduleImmunotherapyGeneExpPlot <- renderPlot({
      return(plot.geneExp.out)
    })
    
    # download pdf plot
    pdf.dir.name = paste("BEST_ConsensusModel_Immunotherapy_Expression_",self_name,"_",uniqID(),sep="")
    tmp.dir = tempdir()
    pdf.plot.file.path = paste(tmp.dir,"/",pdf.dir.name,sep="")
    dir.create(pdf.plot.file.path,recursive = FALSE)
    
    
    lapply(1:plot.num, function(i){
      dataset.name <- names(plotbox_list)[i]
      p <- plotbox_list[[i]]
      
      # save plot
      save_plot(
        filename = paste(pdf.plot.file.path,"/Plot_",dataset.name,".pdf",sep=""),
        plot = p,
        base_height = 7,
        base_width = 4.4
      )
    })
    
    
    output$ConsensusModelPageSubmoduleImmunotherapyGeneExpPlotDownloadPdfBtn <- downloadHandler(
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
    
    # download png plot
    png.dir.name = paste("BEST_ConsensusModel_Immunotherapy_Expression_",self_name,"_",uniqID(),sep="")
    tmp.dir = tempdir()
    png.plot.file.path = paste(tmp.dir,"/",png.dir.name,sep="")
    dir.create(png.plot.file.path,recursive = FALSE)
    
    
    lapply(1:plot.num, function(i){
      dataset.name <- names(plotbox_list)[i]
      p <- plotbox_list[[i]]
      
      # save plot
      save_plot(
        filename = paste(png.plot.file.path,"/Plot_",dataset.name,".png",sep=""),
        plot = p,
        base_height = 7,
        base_width = 4.4
      )
    })
    
    
    output$ConsensusModelPageSubmoduleImmunotherapyGeneExpPlotDownloadPngBtn <- downloadHandler(
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
    
    # show div
    shinyjs::show(
      id = "ConsensusModelPageSubmoduleImmunotherapyGeneExpPlotDiv",
      anim = TRUE,
      animType = "fade"
    )
    
    shinyjs::hide(
      id = "ConsensusModelPageSubmoduleImmunotherapyGeneExpPlotDivBlank"
    )
    
  })# isolate
})

#### roc ####
observeEvent(input$ConsensusModelPageSubmoduleImmunotherapyRocTabSubmitBtn,{
  # return nothing if not clicked
  if(input$ConsensusModelPageSubmoduleImmunotherapyRocTabSubmitBtn == 0){
    return()
  }
  
  # return if no valid gene score
  if(any(is.null(ConsensusModelModuleImmunotherapyReactiveVals$score_list))){
    sendSweetAlert(
      session = session,
      title = "Error",
      text = "No valid gene set score detected in ICI data sets. Please recalculate.",
      type = "error",
      btn_labels = "OK",
      closeOnClickOutside = T
    )
    
    return()
  }
  
  # isolate calculation
  input$ConsensusModelPageSubmoduleImmunotherapyRocTabSubmitBtn
  
  isolate({
    
    library(pROC)
    
    ## load and import data
    # load data
    load(
      paste0(
        "./db/raw_data/common",
        '/ICI_data-symbol.rda'
      )
    )
    
    # send reactive values in here
    self_name <- ConsensusModelModuleImmunotherapyReactiveVals$self_name
    gtf <- gtf_data$gtf
    score_list <- ConsensusModelModuleImmunotherapyReactiveVals$score_list
    select_cohorts <- ConsensusModelModuleImmunotherapyReactiveVals$select_cohorts
    select_cohorts2 <- ConsensusModelModuleImmunotherapyReactiveVals$select_cohorts2
    index <- ConsensusModelModuleImmunotherapyReactiveVals$index
    
    
    auc_cols <- c(pal_npg()(10),pal_d3()(10)[-c(3:6)],pal_jco()(3))[index]
    names(auc_cols) <- names(score_list)
    
    plotroc_list <- list()
    
    for (i in names(select_cohorts)) {
      dd <- merge(ICI_data[[i]]$clin[,c('ID','Response')],score_list[[i]],by=1)%>%na.omit()
      fit <- roc(dd[,2],dd[,3],auc=T)
      rr <- data.frame(x=1-fit$specificities,y=fit$sensitivities)
      rr <- rr[order(rr$y,rr$x),]
      
      plotroc_list[[i]] <- ggplot(rr, aes(x, y)) +
        geom_line(size = 1, color = auc_cols[[i]]) +
        labs(x = '1-Specificity', y = 'Sensitivity', color = NULL) +
        theme_bw(base_rect_size = 2) +
        geom_abline(slope = 1, color = 'grey70') +
        scale_x_continuous(expand = c(0.01, 0.01)) +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        labs(title = unlist(strsplit(ICI_data[[i]]$ann, '---'))[1],
             subtitle = unlist(strsplit(ICI_data[[i]]$ann, '---'))[2]) +
        theme(
          axis.text.y = element_text(size = 11, colour = 'black'),
          axis.text.x = element_text(size = 11, colour = 'black'),
          axis.title.x = element_text(
            size = 15,
            colour = 'darkred',
            face = 'bold'
          ),
          axis.title.y = element_text(
            size = 15,
            colour = 'darkred',
            face = 'bold'
          ),
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "#cacfd2", linetype = "dashed"),
          panel.background = element_rect(fill = '#f3f6f6'),
          plot.title = element_text(
            hjust = 0.5,
            size = 15,
            colour = 'darkred',
            face = 'bold'
          ),
          plot.subtitle = element_text(
            hjust = 0.5,
            size = 11,
            colour = 'black'
          ),
          legend.text = element_text(size = 12),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.position = c(0.995, 0.03),
          legend.justification = c(1, 0)
        ) +
        annotate(
          'text',
          x = 0.75,
          y = 0.1,
          label = paste0('AUC = ', sprintf("%.3f", fit$auc)),
          size = 4.8,
          color = 'black'
        )
    }
    
    plot.roc.out <- plot_grid(plotlist = plotroc_list,ncol = 3)
    plot.num <- length(names(plotroc_list))
    
    # output plot
    output$ConsensusModelPageSubmoduleImmunotherapyRocPlotHtml <- renderUI({
      plot.height <- ceiling(plot.num / 3) * 375
      # plot.width = ifelse(plot.num > 3, 3, plot.num) * 360
      plot.width  = 360 * 3
      
      div(
        div(
          plotOutput(
            outputId = "ConsensusModelPageSubmoduleImmunotherapyRocPlot",
            #width = paste(div.width,"%",sep=""),
            width = paste0(plot.width,"px"),
            height = paste0(plot.height,"px")
          )
        ),
        style="height:400px;overflow-y:scroll;overflow-x:auto;"
      )
      
    })
    
    output$ConsensusModelPageSubmoduleImmunotherapyRocPlot <- renderPlot({
      return(plot.roc.out)
    })
    
    
    # download pdf plot
    pdf.dir.name = paste("BEST_ConsensusModel_Immunotherapy_ROC_",self_name,"_",uniqID(),sep="")
    tmp.dir = tempdir()
    pdf.plot.file.path = paste(tmp.dir,"/",pdf.dir.name,sep="")
    dir.create(pdf.plot.file.path,recursive = FALSE)
    
    
    lapply(1:plot.num, function(i){
      dataset.name <- names(plotroc_list)[i]
      p <- plotroc_list[[i]]
      
      # save plot
      save_plot(
        filename = paste(pdf.plot.file.path,"/Plot_",dataset.name,".pdf",sep=""),
        plot = p,
        base_height = 5,
        base_width = 4.8
      )
    })
    
    
    output$ConsensusModelPageSubmoduleImmunotherapyRocPlotDownloadPdfBtn <- downloadHandler(
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
    
    # download png plot
    png.dir.name = paste("BEST_ConsensusModel_Immunotherapy_ROC_",self_name,"_",uniqID(),sep="")
    tmp.dir = tempdir()
    png.plot.file.path = paste(tmp.dir,"/",png.dir.name,sep="")
    dir.create(png.plot.file.path,recursive = FALSE)
    
    
    lapply(1:plot.num, function(i){
      dataset.name <- names(plotroc_list)[i]
      p <- plotroc_list[[i]]
      
      # save plot
      save_plot(
        filename = paste(png.plot.file.path,"/Plot_",dataset.name,".png",sep=""),
        plot = p,
        base_height = 5,
        base_width = 4.8
      )
    })
    
    
    output$ConsensusModelPageSubmoduleImmunotherapyRocPlotDownloadPngBtn <- downloadHandler(
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
    
    # show div
    shinyjs::show(
      id = "ConsensusModelPageSubmoduleImmunotherapyRocPlotDiv",
      anim = TRUE,
      animType = "fade"
    )
    
    shinyjs::hide(
      id = "ConsensusModelPageSubmoduleImmunotherapyRocPlotDivBlank"
    )
    
  }) # within isolate
  
}) # ROC, within observeEvent start btn

#### survival ####
###### user input ######
# cut off value
observeEvent(input$ConsensusModelPageSubmoduleImmunotherapySurvivalTabMethodSelection,{
  
  ## show/hide slider input, according to group split method
  if(input$ConsensusModelPageSubmoduleImmunotherapySurvivalTabMethodSelection == "custom"){
    shinyjs::show(
      id = "ConsensusModelPageSubmoduleImmunotherapySurvivalTabCutoffValuePercentSelectionDiv",
      anim = TRUE,
      animType = "fade"
    )
    shinyjs::show(
      id = "ConsensusModelPageSubmoduleImmunotherapySurvivalTabCutoffValuePercentSelectionTitleDiv",
      anim = TRUE,
      animType = "fade"
    )
  }
  
  if(input$ConsensusModelPageSubmoduleImmunotherapySurvivalTabMethodSelection != "custom"){
    shinyjs::hide(
      id = "ConsensusModelPageSubmoduleImmunotherapySurvivalTabCutoffValuePercentSelectionDiv",
      anim = TRUE,
      animType = "fade"
    )
    shinyjs::hide(
      id = "ConsensusModelPageSubmoduleImmunotherapySurvivalTabCutoffValuePercentSelectionTitleDiv",
      anim = TRUE,
      animType = "fade"
    )
  }
  
})

observeEvent(input$ConsensusModelPageSubmoduleImmunotherapySurvivalTabSubmitBtn,{
  # return nothing if not clicked
  if(input$ConsensusModelPageSubmoduleImmunotherapySurvivalTabSubmitBtn == 0){
    return()
  }
  
  # return if no valid gene score
  if(any(is.null(ConsensusModelModuleImmunotherapyReactiveVals$score_list))){
    sendSweetAlert(
      session = session,
      title = "Error",
      text = "No valid gene set score detected in ICI data sets. Please recalculate.",
      type = "error",
      btn_labels = "OK",
      closeOnClickOutside = T
    )
    
    return()
  }
  
  # isolate calculation
  input$ConsensusModelPageSubmoduleImmunotherapySurvivalTabSubmitBtn
  
  isolate({
    
    library(survival)
    library(survminer)
    
    ##### process with data #####
    # load data
    load(
      paste0(
        "./db/raw_data/common",
        '/ICI_data-symbol.rda'
      )
    )
    
    # load and import data
    # send reactive values in here
    self_name <- ConsensusModelModuleImmunotherapyReactiveVals$self_name
    gtf <- gtf_data$gtf
    score_list <- ConsensusModelModuleImmunotherapyReactiveVals$score_list
    select_cohorts <- ConsensusModelModuleImmunotherapyReactiveVals$select_cohorts
    select_cohorts2 <- ConsensusModelModuleImmunotherapyReactiveVals$select_cohorts2
    
    
    # user input
    # plot color palette
    Color_palette <- input$ConsensusModelPageSubmoduleImmunotherapySurvivalTabColorSelection
    
    # plot transparency
    alpha <- input$ConsensusModelPageSubmoduleImmunotherapySurvivalTabAlphaSelection
    
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
    cutoff <- input$ConsensusModelPageSubmoduleImmunotherapySurvivalTabMethodSelection
    percent <- input$ConsensusModelPageSubmoduleImmunotherapySurvivalTabCutoffValuePercentSelection
    
    os_cohort <- os_cohort[os_cohort %in% names(select_cohorts)]
    pfs_cohort <- pfs_cohort[pfs_cohort %in% names(select_cohorts)]
    
    # generate plot
    plotos_list <- list()
    if(length(os_cohort)!=0){
      for (i in os_cohort) {
        dd <- merge(ICI_data[[i]]$clin[,c('ID','OS','OS.time')],score_list[[i]],by=1)%>%na.omit()
        plotos_list[[i]] <- tryCatch(lzq_survplot2(Sur_ids = c('OS','OS.time'),cohort = unlist(strsplit(ICI_data[[i]]$ann,'---'))[1],
                                                   data = dd,gene = 'score',Input = self_name,
                                                   cols = cols,cutoff = cutoff,percent = percent),error=function(e)NA)
      }
    }
    
    plotpfs_list <- list()
    if(length(pfs_cohort)!=0){
      for (i in pfs_cohort) {
        dd <- merge(ICI_data[[i]]$clin[,c('ID','PFS','PFS.time')],score_list[[i]],by=1)%>%na.omit()
        plotpfs_list[[i]] <- tryCatch(lzq_survplot2(Sur_ids = c('PFS','PFS.time'),cohort = unlist(strsplit(ICI_data[[i]]$ann,'---'))[1],
                                                    data = dd,gene = 'score',Input = self_name,
                                                    cols = cols,cutoff = cutoff,percent = percent),error=function(e)NA)
        
      }
    }
    
    if(length(pfs_cohort)==0&length(os_cohort)==0){
      # message('No available datasets with prognostic information')
      sendSweetAlert(
        session = session,
        title = "Attention",
        text = "No available datasets with prognostic information.",
        type = "error",
        btn_labels = "OK",
        closeOnClickOutside = TRUE
      )
      return()
    }
    
    # merge 2 plot list
    plot.list.merge = list()
    if(length(plotos_list) == 0){
      plot.list.merge = plotpfs_list
    }else if(length(plotpfs_list) == 0){
      plot.list.merge = plotos_list
    }else{
      plot.list.merge = do.call(c, list(plotos_list, plotpfs_list))
    }
    
    # print(length(names(plotos_list)))
    # print(length(names(plotpfs_list)))
    # plot.list.merge
    
    plot.num = length(names(plot.list.merge))
    #print(plot.num)
    plot.survival.out <- plot_grid(
      plotlist = plot.list.merge,
      ncol = 3
    )
    
    # output plot
    # plot number unknown, so change plotoutput height accordingly
    plot.num = length(names(plot.list.merge))
    
    output$ConsensusModelPageSubmoduleImmunotherapySurvivalTabResultPlotHtml <- renderUI({
      plotHeight = paste(ceiling(plot.num / 3) * 385,"px",sep="")
      # plotWidth = paste0(ifelse(plot.num > 3, 3, plot.num) * 350,"px")
      plotWidth = 350 * 3
      
      div(
        plotOutput(
          outputId = "ConsensusModelPageSubmoduleImmunotherapySurvivalTabResultPlot",
          height = plotHeight,
          width = plotWidth
        ),
        
        style = "height:400px; overflow-y:scroll;overflow-x:auto;"
      )
    })
    
    output$ConsensusModelPageSubmoduleImmunotherapySurvivalTabResultPlot <- renderPlot({
      return(plot.survival.out)
    })
    
    # download pdf plot
    pdf.dir.name = paste("BEST_ConsensusModel_Immunotherapy_Survival_",self_name,"_",uniqID(),sep="")
    tmp.dir = tempdir()
    pdf.plot.file.path = paste(tmp.dir,"/",pdf.dir.name,sep="")
    dir.create(pdf.plot.file.path,recursive = FALSE)
    
    lapply(1:plot.num, function(i){
      dataset.name <- names(plot.list.merge)[i]
      p <- plot.list.merge[[i]]
      
      # save plot
      save_plot(
        filename = paste(pdf.plot.file.path,"/Plot_",dataset.name,".pdf",sep=""),
        plot = p,
        base_height = 5.5,
        base_width = 5
      )
    })
    
    output$ConsensusModelPageSubmoduleImmunotherapySurvivalTabResultPlotDownloadPdfBtn <- downloadHandler(
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
    
    # download png plot
    png.dir.name = paste("BEST_ConsensusModel_Immunotherapy_Survival_",self_name,"_",uniqID(),sep="")
    tmp.dir = tempdir()
    png.plot.file.path = paste(tmp.dir,"/",png.dir.name,sep="")
    dir.create(png.plot.file.path,recursive = FALSE)
    
    
    lapply(1:plot.num, function(i){
      dataset.name <- names(plot.list.merge)[i]
      p <- plot.list.merge[[i]]
      
      # save plot
      save_plot(
        filename = paste(png.plot.file.path,"/Plot_",dataset.name,".png",sep=""),
        plot = p,
        base_height = 5.5,
        base_width = 5
      )
    })
    
    
    output$ConsensusModelPageSubmoduleImmunotherapySurvivalTabResultPlotDownloadPngBtn <- downloadHandler(
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
    
    
    # show div
    shinyjs::show(
      id = "ConsensusModelPageSubmoduleImmunotherapySurvivalTabResultPlotDiv",
      anim = TRUE,
      animType = "fade"
    )
    
    shinyjs::hide(
      id = "ConsensusModelPageSubmoduleImmunotherapySurvivalTabResultPlotDivBlank"
    )
    
  }) #isolate
  
})