# this script render the server of analysis page of 
# module prognostic model, analysis candidate agents

# set up with server end
#### specific pkgs and options ####

#### load data ####
# set reactive values
# allims_cor_ids <- reactiveValues(
#   allgene_ids = NULL
# )
# observeEvent(input$HomePageAnalysisGoBtn1,{
#   load(paste(
#     "./db/raw_data/",
#     str_replace_all(globReactiveValues$homePageCancerSelected, " +", "_"),
#     "/allims_cor_ids.rda",
#     sep = ""
#   ))
#   
#   allims_cor_ids$allgene_ids <- allgene_ids
# })

#### check user input ####
PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelectionCheck <- reactiveValues(
  stauts = 0 #0 for failed ,1 for ok
)

observeEvent(input$PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelection,{
  max_num = input$PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelection
  #  detect input contains char not 0-9, not empty
  if(str_detect(max_num,"[^0-9]") | max_num == "" | is.na(max_num)){ 
    shinyFeedback::showFeedback(
      inputId = "PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelection",
      text = "An integer >= 10 and <= 100 is required.",
      color = "red",
      icon = icon("warning"),
      session = session
    )
    PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelectionCheck$status = 0
    return()
  }else {
    if(max_num < 10 | max_num > 100){
      shinyFeedback::showFeedback(
        inputId = "PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelection",
        text = "An integer >= 10 and <= 100 is required.",
        color = "red",
        icon = icon("warning"),
        session = session
      )
      PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelectionCheck$status = 0
      return()
    }else{
      shinyFeedback::hideFeedback(
        inputId = "PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelection",
        session = session
      )
      PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelectionCheck$status = 1
    }
  }
})

#### start analysis ####
observeEvent(input$PrognosticModelPageSubmoduleCandidateAgentsSubmitBtn, {
  # return nothing if not clicked
  if(input$PrognosticModelPageSubmoduleCandidateAgentsSubmitBtn == 0){
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
  
  
  input$PrognosticModelPageSubmoduleCandidateAgentsSubmitBtn
  
  isolate({
    
    library(ComplexHeatmap)
    library(circlize)
    library(heatmaply)
    library(RobustRankAggreg)
    
    ##### process with data #####
    # send reactive valus in 
    load(
      paste0(
        "./db/raw_data/",
        str_replace_all(globReactiveValues$homePageCancerSelected, " +", "_"),
        '/Drugdata.rda'
      )
    )
    
    # -------------------------------------------------------------------------
    ### Prepare score ###
    # -------------------------------------------------------------------------
    self_name <- PrognosticModelReactiveValues$self_name
    score_list <- PrognosticModelReactiveValues$score_list
    score_list <- lapply(score_list,function(x){
      x <- data.frame(ID=rownames(x),Score=x[,3])
      return(x)
    })
    
    Drug_list <- lapply(Drug_list,function(x){x[names(score_list)]})
    
    
    ## user input
    Database = input$PrognosticModelPageSubmoduleCandidateAgentsDatabaseSelection
    max_num = input$PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelection
    
    ## check max_num
    #  detect input contains char not 0-9, not empty
    if(str_detect(max_num,"[^0-9]") | max_num == "" | is.na(max_num)){
      shinyFeedback::showFeedback(
        inputId = "PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelection",
        text = "An integer >= 10 and <= 100 is required.",
        color = "red",
        icon = icon("warning"),
        session = session
      )
      PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelectionCheck$status = 0
      return()
    }else {
      if(max_num < 10 | max_num > 100){
        shinyFeedback::showFeedback(
          inputId = "PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelection",
          text = "An integer >= 10 and <= 100 is required.",
          color = "red",
          icon = icon("warning"),
          session = session
        )
        PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelectionCheck$status = 0
        return()
      }else{
        shinyFeedback::hideFeedback(
          inputId = "PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelection",
          session = session
        )
        PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelectionCheck$status = 1
      }
    }
    
    if(PrognosticModelPageSubmoduleCandidateAgentsMaxNumberSelectionCheck$status == 0){
      return()
    }
    
    # -------------------------------------------------------------------------
    #### plot heatmap ####
    # -------------------------------------------------------------------------
    
    # choose database
    dd <- Drug_list[[Database]]
    
    cor_list <- map2(score_list,dd,function(x,y){
      ID <- intersect(x$ID,rownames(y))
      x <- x[match(ID,x$ID),]
      y <- y[ID,]
      l <- cor(y,x$Score)%>%as.data.frame()%>%tibble::rownames_to_column('ID')
      colnames(l)[2] <- 'Cor'
      return(l)
    })
    
    if(length(names(cor_list))>1){
      cor_data <- Reduce(function(x,y){merge(x,y,by=1,all = T)},cor_list)%>%
        tibble::column_to_rownames('ID')
    }
    
    if(length(names(cor_list))==1){
      cor_data <- cor_list[[1]]
      rownames(cor_data) <- NULL
      cor_data <- cor_data%>%tibble::column_to_rownames('ID')
    }
    
    colnames(cor_data) <- names(score_list)
    
    # -------------------------------------------------------------------------
    
    cordata <- cor_data
    poslist <- lapply(as.list(cordata),function(x){
      x <- data.frame(ID=rownames(cordata),R=x)%>%na.omit()
      x <- x[x$R>0,]%>%arrange(desc(R))
      return(x$ID)
    })
    neglist <- lapply(as.list(cordata),function(x){
      x <- data.frame(ID=rownames(cordata),R=x)%>%na.omit()
      x <- x[x$R<0,]%>%arrange(R)
      return(x$ID)
    })
    
    pos_agg=aggregateRanks(poslist)
    pos_agg <- pos_agg[pos_agg$Score<0.05,]
    neg_agg=aggregateRanks(neglist)
    neg_agg <- neg_agg[neg_agg$Score<0.05,]
    
    max_num <- max_num ##可以自己设置
    pos_num <- ifelse(nrow(pos_agg)<max_num,nrow(pos_agg),max_num)
    neg_num <- ifelse(nrow(neg_agg)<max_num,nrow(neg_agg),max_num)
    
    if(ncol(cordata)!=1){
      posdata <- cordata[pos_agg$Name[1:pos_num],]
      negdata <- cordata[neg_agg$Name[1:neg_num],]
    }else{
      posdata <- cordata[pos_agg$Name[1:pos_num],]%>%as.data.frame()
      rownames(posdata) <- pos_agg$Name[1:pos_num]
      colnames(posdata) <- names(total_expr_list)[show_cohort_index]
      negdata <- cordata[neg_agg$Name[1:neg_num],]%>%as.data.frame()
      rownames(negdata) <- neg_agg$Name[1:neg_num]
      colnames(negdata) <- names(total_expr_list)[show_cohort_index]
    }
    
    # cordata <- cor_tmp[[Database]]
    # idx = apply(cordata, 2, function(x){
    #   !any(is.na(x))
    # })
    # cordata <- cordata[,idx]
    # #print(head(cordata))
    # 
    # poslist <- lapply(as.list(cordata), function(x) {
    #   return(rownames(cordata)[x > 0])
    # })
    # neglist <- lapply(as.list(cordata), function(x) {
    #   return(rownames(cordata)[x < 0])
    # })
    # 
    # pos_agg = aggregateRanks(poslist)
    # pos_agg <- pos_agg[pos_agg$Score < 0.05, ]
    # neg_agg = aggregateRanks(neglist)
    # neg_agg <- neg_agg[neg_agg$Score < 0.05, ]
    # 
    # ## max_num <- 30 ##可以自己设置
    # pos_num <- ifelse(nrow(pos_agg) < max_num, nrow(pos_agg), max_num)
    # neg_num <- ifelse(nrow(neg_agg) < max_num, nrow(neg_agg), max_num)
    # 
    # posdata <- cordata[pos_agg$Name[1:pos_num], ]
    # negdata <- cordata[neg_agg$Name[1:neg_num], ]
    
    # -------------------------------------------------------------------------
    # #### heatmap here ####
    # ##### heatmaply first #####
    # # use plot div, because plot height unknown
    # output$PrognosticModelPageSubmoduleCandidateAgentsHeatmapHtml <- renderUI({
    #   div(
    #     #title
    #     span("Candidate agents",
    #          style = "font-size:100%;color:black;"), 
    #     span("Positively", 
    #          style = "font-weight:bold; font-size:110%; color:#e3170a;"),
    #     span("correlated with gene expression",
    #          style = "font-size:100%;color:black;"), 
    #     
    #     # space
    #     p(""),
    #     
    #     # pos plot
    #     div(
    #       plotlyOutput(
    #         outputId = "PrognosticModelPageSubmoduleCandidateAgentsHeatmapPosCor",
    #         height = plot.height
    #       ),
    #       style = "height:400px; overflow-y:scroll;"
    #     ),
    #     
    #     # space
    #     tags$br(),
    #     tags$hr(),
    #     tags$br(),
    #     
    #     # title
    #     span("Candidate agents",
    #          style = "font-size:100%;color:black;"),  
    #     span("Negtively", 
    #          style = "font-weight:bold; font-size:110%; color:#0077b6;"),
    #     span("correlated with gene expression",
    #          style = "font-size:100%;color:black;"), 
    #     
    #     # space
    #     p(""),
    #     
    #     # neg plot
    #     div(
    #       plotlyOutput(
    #         outputId = "PrognosticModelPageSubmoduleCandidateAgentsHeatmapNegCor",
    #         height = plot.height
    #       ),
    #       style = "height:400px; overflow-y:scroll;"
    #     )
    #   )
    # })
    # 
    # ## output heatmaply
    # # pos
    # output$PrognosticModelPageSubmoduleCandidateAgentsHeatmapPosCor <- renderPlotly({
    #   heatmaply::heatmaply(
    #     as.matrix(posdata),
    #     Rowv = FALSE,
    #     colors = c('#0077b6','#48cae4','white',alpha('#e3170a',0.5),'#e3170a'),
    #     Colv = TRUE,
    #     na.value = "grey50",
    #     hide_colorbar = FALSE,
    #     row_dend_left = FALSE,
    #     branches_lwd = 0.3,
    #     height = plot.height
    #   )
    # })
    # # neg
    # output$PrognosticModelPageSubmoduleCandidateAgentsHeatmapNegCor <- renderPlotly({
    #   heatmaply::heatmaply(
    #     as.matrix(negdata),
    #     Rowv = FALSE,
    #     colors = c('#0077b6','#48cae4','white',alpha('#e3170a',0.5),'#e3170a'),
    #     Colv = TRUE,
    #     na.value = "grey50",
    #     hide_colorbar = FALSE,
    #     row_dend_left = FALSE,
    #     branches_lwd = 0.3,
    #     height = plot.height
    #   )
    # })
    
    #### plot with complexheatmap #####
    cols <- colorRamp2(c(-1,-0.5,0,0.5,1),c('#0077b6','#48cae4','white',alpha('#e3170a',0.5),'#e3170a'))
    # print(head(posdata))
    # print(class(posdata))
    # as.matrix(posdata)
    
    left_ann1 <-
      HeatmapAnnotation(
        type = anno_block(
          gp = gpar(fill = '#edae49'),
          width = unit(5.8, 'mm'),
          labels = 'High expression indicates resistance',
          labels_gp = gpar(
            cex = 1,
            col = "white",
            fontface = 'bold'
          )
        ),
        border = T,
        which = 'row',
        show_annotation_name = F
      )
    left_ann2 <-
      HeatmapAnnotation(
        type = anno_block(
          gp = gpar(fill = '#2a9d8f'),
          width = unit(5.8, 'mm'),
          labels = 'High expression indicates sensitivity',
          labels_gp = gpar(
            cex = 1,
            col = "white",
            fontface = 'bold'
          )
        ),
        border = T,
        which = 'row',
        show_annotation_name = F
      )
    
    p1 <- Heatmap(
      as.matrix(posdata),
      na_col = NULL,
      name = 'Correlation',
      border = F,
      col = cols,
      row_names_side = 'right',
      cluster_rows = T,
      left_annotation = left_ann1,
      width = ncol(posdata) * unit(5.5, "mm"),
      height = nrow(posdata) * unit(5, "mm"),
      show_row_dend = F,
      show_column_dend = F
    )
    
    p2 <- Heatmap(
      as.matrix(negdata),
      na_col = NULL,
      name = 'Correlation',
      border = F,
      col = cols,
      row_names_side = 'right',
      cluster_rows = T,
      left_annotation = left_ann2,
      width = ncol(negdata) * unit(5.5, "mm"),
      height = nrow(negdata) * unit(5, "mm"),
      show_row_dend = F,
      show_column_dend = F
    )
    
    data.output.merge <- rbind(posdata,negdata)
    data.output.merge$Correlation.Type <- c(rep("Positive",nrow(posdata)),
                                            rep("Negtive",nrow(negdata)))
    
    plot.heatmap <- draw(p1%v%p2)
    
    plot.height = max_num * 30 + 100
    plot.width = ncol(posdata) * 30 + 300
    
    #### plot output ####
    output$PrognosticModelPageSubmoduleCandidateAgentsHeatmapHtml <- renderUI({
      div(
        plotOutput(
          outputId = "PrognosticModelPageSubmoduleCandidateAgentsHeatmap",
          height = paste0(plot.height,"px"),
          width = paste0(plot.width,"px")
        ),
        style = "height:400px; overflow-y:scroll; overflow-x:auto; "
      )
    })
    
    output$PrognosticModelPageSubmoduleCandidateAgentsHeatmap <- renderPlot({
      return(plot.heatmap)
    })
    
    # plot.heatmap.pos <- ComplexHeatmap::Heatmap(
    #   posdata, # DO NOT use as.matrix() here, BAD things will happen!!!
    #   na_col = NULL,
    #   name = 'Correlation',
    #   border = F,
    #   col = cols,
    #   row_names_side = 'left',
    #   row_title = "Positively correlated agents",
    #   cluster_rows = F
    # )
    # 
    # plot.heatmap.neg <-
    #   ComplexHeatmap::Heatmap(
    #     negdata,
    #     na_col = NULL,
    #     name = 'Correlation',
    #     border = F,
    #     col = cols,
    #     row_names_side = 'left',
    #     row_title = "Negtively correlated agents",
    #     cluster_rows = F
    #   )
    
    ### generate plots one by one and save with zip,
    # generate and save plots
    # create folders
    # dir.name = str_replace_all(
    #   paste("BEST_PrognosticModel_CandidateAgents_",
    #         Input,
    #         "_",
    #         uniqID(),
    #         sep = ""),
    #   " +",
    #   "_"
    # )
    # tmp.dir = tempdir()
    # plot.file.path = paste(tmp.dir,"/",dir.name,sep="")
    # dir.create(plot.file.path,recursive = FALSE)
    
    #### save plots ####
    output$PrognosticModelPageSubmoduleCandidateAgentsHeatmapDownloadPdfBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_PrognosticModel_CandidateAgents_",
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
      content = function(fname){
        pdfPlotHeight = round(max_num / 10) * 4 + 4
        pdf.plot.width = ncol(posdata) * 0.4 + 4
        
        pdf(fname, height = pdfPlotHeight, width = pdf.plot.width)
        
        # draw plot
        draw(
          plot.heatmap,
          padding = unit(c(2, 2, 2, 2), "mm") # add space around the plot
        )
        
        # close device and save plot
        dev.off()
      },
      contentType = ".pdf"
    )
    output$PrognosticModelPageSubmoduleCandidateAgentsHeatmapDownloadPngBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_PrognosticModel_CandidateAgents_",
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
      content = function(fname){
        png.plot.height = max_num * 40 + 400
        png.plot.width = ncol(posdata) * 40 + 400
        
        # open device
        png(fname, height = png.plot.height, width = png.plot.width)
        
        # draw plot
        draw(
          plot.heatmap,
          padding = unit(c(2, 40, 2, 2), "mm") # add space around the plot
        )
        
        # close device and save plot
        dev.off()
      },
      contentType = ".png"
    )
    # save table
    output$PrognosticModelPageSubmoduleCandidateAgentsHeatmapDownloadTableBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_PrognosticModel_CandidateAgents_",
            self_name,
            "_",
            uniqID(),
            ".csv",
            sep = ""
          ),
          " +",
          "_"
        )
      },
      content = function(fname){
        write.csv(
          data.output.merge,
          fname,
          quote = FALSE
        )
      },
      contentType = ".csv"
    )
    
    
    # # pdf plot height
    # pdfPlotHeight = round(max_num / 10) * 5
    # ### pos cor plot
    # # open device
    # filename = paste(plot.file.path,"/Positively_correlated_agents.pdf",sep="")
    # pdf(filename, height = pdfPlotHeight, width = 8)
    # 
    # # draw plot
    # draw(
    #   plot.heatmap,
    #   padding = unit(c(2, 2, 2, 2), "mm") # add space around the plot
    # )
    # 
    # # close device and save plot
    # dev.off()
    # 
    # ### neg cor plot
    # # open device
    # filename = paste(plot.file.path,"/Negtively_correlated_agents.pdf",sep="")
    # pdf(filename, height = pdfPlotHeight, width = 8)
    # 
    # # draw plot
    # draw(
    #   plot.heatmap,
    #   padding = unit(c(2, 2, 2, 2), "mm") # add space around the plot
    # )
    # 
    # # close device and save plot
    # dev.off()
    # 
    # ## output btn, download zip
    # output$PrognosticModelPageSubmoduleCandidateAgentsHeatmapDownloadBtn <- downloadHandler(
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
    
    ## show big div
    shinyjs::show(
      id = "PrognosticModelPageSubmoduleCandidateAgentsHeatmapDiv",
      anim = TRUE,
      animType = "fade"
    )
    
    ##### sub plot #####
    ###### update sub plot user input #####
    
    # cohorts in posdata and negdata may be different
    cohort.to.select = colnames(posdata)
    
    updatePickerInput(
      session = session,
      inputId = "PrognosticModelPageSubmoduleCandidateAgentsSubplotCohortSelection",
      choices = cohort.to.select
    )
    
    # drugs in pos and neg are different
    drug.to.select = list("Positively correlated agents" = rownames(posdata),
                          "Negtively correlated agents" = rownames(negdata))
    
    updatePickerInput(
      session = session,
      inputId = "PrognosticModelPageSubmoduleCandidateAgentsSubplotDrugSelection",
      choices = drug.to.select
    )
    
    ###### start sub plot analysis #####
    observeEvent(input$PrognosticModelPageSubmoduleCandidateAgentsSubplotSubmitBtn, {
      if(input$PrognosticModelPageSubmoduleCandidateAgentsSubplotSubmitBtn == 0){
        return()
      }
      
      input$PrognosticModelPageSubmoduleCandidateAgentsSubplotSubmitBtn
      
      isolate({
        # load drug data
        load(
          paste0(
            "./db/raw_data/",
            str_replace_all(globReactiveValues$homePageCancerSelected, " +", "_"),
            '/Drugdata.rda'
          )
        )
        
        # user input
        Cohort = input$PrognosticModelPageSubmoduleCandidateAgentsSubplotCohortSelection
        Drug_ID = input$PrognosticModelPageSubmoduleCandidateAgentsSubplotDrugSelection
        Database = input$PrognosticModelPageSubmoduleCandidateAgentsDatabaseSelection
        cor.method = input$PrognosticModelPageSubmoduleCandidateAgentsSubplotCorMethodSelection
        
        # start calculate
        #print(head(t(total_expr_list[[Cohort]][gene, ])))
        #print(head(Drug_list[[Database]][[Cohort]]))
        cor_tmp <- merge(score_list[[Cohort]], cbind(rownames(Drug_list[[Database]][[Cohort]]), Drug_list[[Database]][[Cohort]][, Drug_ID]), by = 1)
        cor_tmp <- na.omit(cor_tmp)
        cor_tmp$V2 <- as.numeric(cor_tmp$V2)
        
        fit <- cor.test(cor_tmp[, 2], cor_tmp[, 3], method = cor.method) ##可自定义方法
        
        # sub plot now
        plot.subplot <- ggplot(cor_tmp, aes_string('Score', 'V2')) +
          geom_point(col = '#6a4c93', alpha = 0.8, size = 2) +
          geom_smooth(
            method = lm,
            se = T,
            na.rm = T,
            fullrange = T,
            size = 1.8,
            col = "#1982c4"
          ) +
          geom_rug(col = "#3d405b", size = 1) +
          theme_bw(base_rect_size = 1.5) +
          labs(
            x = self_name,
            y = paste0('IC50 of ', Drug_ID, ' (', Database, ')'),
            title = Cohort,
            subtitle = paste0(
              'Cor = ',
              sprintf("%.3f", fit$estimate),
              '; Pval = ',
              format(fit$p.value, scientific = T)
            )
          ) +
          theme(
            plot.title = element_text(
              hjust = 0.5,
              size = 14,
              colour = 'darkred',
              face = 'bold'
            ),
            plot.subtitle = element_text(
              hjust = 0.5,
              size = 11,
              colour = 'black'
            ),
            axis.text = element_text(size = 10, colour = 'black'),
            axis.title.x = element_text(
              size = 13,
              colour = 'darkred',
              face = 'bold'
            ),
            axis.title.y = element_text(
              size = 13,
              colour = 'darkred',
              face = 'bold'
            ),
            axis.ticks = element_line(size = 1),
            panel.grid = element_blank(),
            panel.grid.major = element_line(color = "#cacfd2", linetype = "dashed"),
            panel.background = element_rect(fill = '#f3f6f6')
          )
        
        # output
        output$PrognosticModelPageSubmoduleCandidateAgentsSubplot <- renderPlot({
          return(plot.subplot)
        })
        
        # download plot
        output$PrognosticModelPageSubmoduleCandidateAgentsSubplotDownloadPdfBtn <- downloadHandler(
          filename = function() {
            fname = paste(
              "BEST_PrognosticModel_CandidateAgents_",
              globReactiveValues$homePageCancerSelected,
              "_",
              Cohort,
              "_",
              Drug_ID,
              "_CorPlot",
              uniqID(idLength = 10),
              ".pdf"
            )
            return(str_remove_all(fname,"\\s+"))
          },
          
          
          content = function(fname){
            save_plot(
              fname,
              plot.subplot,
              base_height = 4.1,
              base_width = 4.5
            )
          },
          
          contentType = "pdf"
        )
        
        output$PrognosticModelPageSubmoduleCandidateAgentsSubplotDownloadPngBtn <- downloadHandler(
          filename = function() {
            fname = paste(
              "BEST_PrognosticModel_CandidateAgents_",
              globReactiveValues$homePageCancerSelected,
              "_",
              Cohort,
              "_",
              Drug_ID,
              "_CorPlot",
              uniqID(idLength = 10),
              ".png"
            )
            return(str_remove_all(fname,"\\s+"))
          },
          
          
          content = function(fname){
            save_plot(
              fname,
              plot.subplot,
              base_height = 4.1,
              base_width = 4.5
            )
          },
          
          contentType = "png"
        )
        
        
        # show div
        shinyjs::show(
          id = "PrognosticModelPageSubmoduleCandidateAgentsSubplotDiv",
          anim = TRUE,
          animType = "fade"
        )
        
      })# sub plot isolate
    }) # sub plot event
    
    
  }) #isolate
})
