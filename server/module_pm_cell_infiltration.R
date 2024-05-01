# this script render the server of analysis page of 
# module prognostic model, analysis cell infiltration

# set up with server end
#### specific pkgs and options ####
options(digits = 2)

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
# nothing to check before start btn clicked


#### start analysis ####
observeEvent(input$PrognosticModelPageSubmoduleCellInfiltrationSubmitBtn,{
  # return nothing if not clicked
  if(input$PrognosticModelPageSubmoduleCellInfiltrationSubmitBtn == 0){
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
  
  
  input$PrognosticModelPageSubmoduleCellInfiltrationSubmitBtn
  
  isolate({
    
    library(ComplexHeatmap)
    library(circlize)
    
    
    ##### process with data #####
    # send reactive valus in 
    # load  data
    load(
      paste0(
        "./db/raw_data/",
        str_replace_all(globReactiveValues$homePageCancerSelected, " +", "_"),
        "/Cell-infiltration-list.rda"
      )
    )
    cell_list <-
      lapply(cell_list, function(x) {
        x[, -1] <- scale(x[, -1])
        return(x)
      })
    
    # -------------------------------------------------------------------------
    ### Prepare score ###
    # -------------------------------------------------------------------------
    self_name <- PrognosticModelReactiveValues$self_name
    score_list <- PrognosticModelReactiveValues$score_list
    score_list <- lapply(score_list,function(x){
      x <- data.frame(ID=rownames(x),Score=x[,1],row.names = rownames(x))
      return(x)
    })
    
    Cohort <- intersect(names(cell_list), names(score_list))
    cell_list <- cell_list[Cohort]
    score_list <- score_list[Cohort]
    
    cor_list <- map2(score_list,cell_list,function(x,y){
      id <- intersect(x$ID,y$ID)
      x <- x[id,]
      rownames(y) <- y$ID
      y <- y[id,]
      l <- cor(y[,-1],x$Score)%>%as.data.frame()%>%tibble::rownames_to_column('ID')
      colnames(l)[2] <- 'Cor'
      return(l)
    })
    
    if (length(names(cor_list)) > 1) {
      cor_data <-
        Reduce(function(x, y) {
          merge(x, y, by = 1, all = T)
        }, cor_list) %>%
        tibble::column_to_rownames('ID')
    }
    
    if (length(names(cor_list)) == 1) {
      cor_data <- cor_list[[1]]
      rownames(cor_data) <- NULL
      cor_data <- cor_data %>% tibble::column_to_rownames('ID')
    }
    
    colnames(cor_data) <- names(score_list)
    
    
    # -------------------------------------------------------------------------
    ### plot heatmap ###
    # -------------------------------------------------------------------------
    
    load('./db/raw_data/common/cellann.rda')
    
    cols <- colorRamp2(c(-1,-0.5,0,0.5,1),c('#0077b6','#48cae4','white',alpha('#e3170a',0.5),'#e3170a'))
    
    if(ncol(cor_data) == 1) {
      tmp <- cor_data %>% as.data.frame()
      tmp <-
        tmp[match(cellann$cells, rownames(tmp)), ] %>% as.data.frame()
      rownames(tmp) <- cellann$cells
      colnames(tmp) <- colnames(cor_data)
      cell.name <- rownames(tmp)
      ll <- gsub('_[A-Za-z0-9]{1,}$', '', rownames(tmp))
      ll <- gsub('_CIBERSORT', '', ll)
      rownames(tmp) <- NULL
    } else{
      tmp <- cor_data
      tmp <- tmp[match(cellann$cells, rownames(tmp)), ]
      cell.name <- rownames(tmp)
      ll <- gsub('_[A-Za-z0-9]{1,}$', '', rownames(tmp))
      ll <- gsub('_CIBERSORT', '', ll)
      rownames(tmp) <- NULL
    }
    
    
    
    ann <-
      data.frame(ID = rownames(tmp), Algorithms = rep(
        c(
          'ESTIMATE',
          'CIBERSORT',
          'CIBERSORT_ABS',
          'TIMER',
          'Quantiseq',
          'MCPcounter',
          'EPIC',
          'xCell'
        ),
        times = c(3, 
                  22, 
                  22, 
                  6, 
                  10, 
                  10, 
                  7, 
                  64)
      ))
    
    
    ##### complexheatmap later #####
    # used for download pdf
    left_ann <-
      HeatmapAnnotation(
        Algorithms = ann$Algorithms,
        which = 'row',
        show_legend = T,
        show_annotation_name = F,
        simple_anno_size = unit(2, 'mm'),
        col = list(
          Algorithms = c(
            'ESTIMATE' = pal_nejm(alpha = 0.8)(8)[1],
            'CIBERSORT' = pal_nejm(alpha = 0.8)(8)[2],
            'CIBERSORT_ABS' = pal_nejm(alpha = 0.8)(8)[3],
            'TIMER' = pal_nejm(alpha = 0.8)(8)[4],
            'Quantiseq' = pal_nejm(alpha = 0.8)(8)[5],
            'MCPcounter' = pal_nejm(alpha = 0.8)(8)[6],
            'EPIC' = pal_nejm(alpha = 0.8)(8)[7],
            'xCell' = pal_nejm(alpha = 0.8)(8)[8]
          )
        )
      )
    
    plot.res.heatmap <-
      ComplexHeatmap::Heatmap(
        as.matrix(tmp),
        na_col = NULL,
        name = 'Correlation',
        border = F,
        col = cols,
        show_row_names = T,
        row_names_side = 'left',
        row_labels = ll,
        row_split = rep(c(1, 2, 3, 4, 5, 6, 7, 8), 
                        times = c(3, 22, 22, 6, 10, 10, 7, 64)),
        row_gap = unit(5, 'mm'),
        left_annotation = left_ann,
        row_title = NULL,
        cluster_rows = T,
        width= ncol(tmp) * unit(5.5, "mm"),
        height= nrow(tmp) * unit(5, "mm"),
        show_row_dend = F
      )
    
    ##### output #####
    output$PrognosticModelPageSubmoduleCellInfiltrationHeatmapHtml <- renderUI({
      plot.width = paste0((ncol(tmp) * 25 + 300),"px")
      div(
        plotOutput(
          outputId = "PrognosticModelPageSubmoduleCellInfiltrationHeatmap",
          height = "2200px",
          width = plot.width
        ),
        style = "height:400px; overflow-y:scroll; overflow-x:auto; "
      )
      
    })
    
    
    output$PrognosticModelPageSubmoduleCellInfiltrationHeatmap <- renderPlot({
      return(plot.res.heatmap)
    })
    
    ##### download file #####
    output$PrognosticModelPageSubmoduleCellInfiltrationHeatmapDownloadPdfBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_PrognosticModel_CellInfiltration_",
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
        
        plot.width = ncol(tmp) * 0.4 + 4
        
        # open device
        pdf(fname, height = 32, width = plot.width)
        
        # draw plot
        draw(
          plot.res.heatmap,
          padding = unit(c(2, 40, 2, 2), "mm") # add space around the plot
        )
        
        # close device and save plot
        dev.off()
      },
      
      contentType = ".pdf"
    )
    
    output$PrognosticModelPageSubmoduleCellInfiltrationHeatmapDownloadPngBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_PrognosticModel_CellInfiltration_",
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
        
        plot.width = ncol(tmp) * 40 + 400
        
        # open device
        png(fname, height = 3200, width = plot.width)
        
        # draw plot
        draw(
          plot.res.heatmap,
          padding = unit(c(2, 40, 2, 2), "mm") # add space around the plot
        )
        
        # close device and save plot
        dev.off()
      },
      
      contentType = ".png"
    )
    
    ##### subplot #####
    ###### update user input ######
    cohort.name = colnames(tmp)
    
    shiny::updateSelectInput(
      session = session,
      inputId = "PrognosticModelPageSubmoduleCellInfiltrationSubplotCohortSelection",
      choices = cohort.name
    )
    
    shiny::updateSelectInput(
      session = session,
      inputId = "PrognosticModelPageSubmoduleCellInfiltrationSubplotCellTypeSelection",
      choices = cell.name
    )
    
    ##### subplot start #####
    observeEvent(input$PrognosticModelPageSubmoduleCellInfiltrationSubplotSubmitBtn,{
      
      ###### get user input ######
      Cohort = input$PrognosticModelPageSubmoduleCellInfiltrationSubplotCohortSelection
      Cell_ID = input$PrognosticModelPageSubmoduleCellInfiltrationSubplotCellTypeSelection
      Cell_ID2 <- Cell_ID
      cor.method.selected = input$PrognosticModelPageSubmoduleCellInfiltrationSubplotCorMethodSelection
      
      ###### load data ######
      ###### prepare data ######
      cor_tmp <- merge(score_list[[Cohort]], cell_list[[Cohort]][, c('ID', Cell_ID)], by = 1)[, -1]
      cor_tmp <- na.omit(cor_tmp)
      colnames(cor_tmp) <- gsub('-', '_', colnames(cor_tmp))
      Cell_ID <- gsub('-', '_', Cell_ID)
      colnames(cor_tmp) <- gsub('\\+', '', colnames(cor_tmp))
      Cell_ID <- gsub('\\+', '', Cell_ID)
      
      # line regression
      fit <- cor.test(cor_tmp[, 1], cor_tmp[, 2], method = cor.method.selected) ##可自定义相关性计算方法
      
      ###### cor plot ######
      plot.cor.plot <- ggplot(cor_tmp, aes_string('Score', Cell_ID)) +
        geom_point(col = 'darkblue', alpha = 0.8, size = 2) +
        geom_smooth(
          method = lm,
          se = T,
          na.rm = T,
          fullrange = T,
          size = 1.8,
          col = "#e9c46a"
        ) +
        geom_rug(col = "#62A398", size = 1) +
        theme_bw(base_rect_size = 1.5) +
        labs(
          x = self_name,
          y = Cell_ID2,
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
      
      # subplot output
      output$PrognosticModelPageSubmoduleCellInfiltrationSubplot <- renderPlot({
        return(plot.cor.plot)
      })
      
      # download subplot
      output$PrognosticModelPageSubmoduleCellInfiltrationSubplotDownloadPdfBtn <- downloadHandler(
        filename = function(){
          str_replace_all(
            paste(
              "BEST_PrognosticModel_CellInfiltration_",
              self_name,
              "_",
              Cohort,
              "_",
              Cell_ID,
              "_CorPlot_",
              uniqID(),
              ".pdf",
              sep = ""
            ),
            " +",
            "_"
          )
        },
        
        content = function(fname){
          
          save_plot(
            fname,
            plot.cor.plot,
            base_width = 4.1,
            base_height = 4.5
          )
          
        },
        
        contentType = ".pdf"
      )
      
      output$PrognosticModelPageSubmoduleCellInfiltrationSubplotDownloadPngBtn <- downloadHandler(
        filename = function(){
          str_replace_all(
            paste(
              "BEST_PrognosticModel_CellInfiltration_",
              self_name,
              "_",
              Cohort,
              "_",
              Cell_ID,
              "_CorPlot_",
              uniqID(),
              ".png",
              sep = ""
            ),
            " +",
            "_"
          )
        },
        
        content = function(fname){
          
          save_plot(
            fname,
            plot.cor.plot,
            base_width = 4.1,
            base_height = 4.5
          )
          
        },
        
        contentType = ".png"
      )
      
      ## show div
      shinyjs::show(
        id = "PrognosticModelPageSubmoduleCellInfiltrationSubplotDiv",
        anim = TRUE,
        animType = "fade"
      )
    })
    
    ## show heatmap div 
    shinyjs::show(
      id = "PrognosticModelPageSubmoduleCellInfiltrationHeatmapDiv",
      anim = TRUE,
      animType = "fade"
    )
    
    
  }) # within isolate
})


