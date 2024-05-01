# this script render the server of analysis page of 
# module prognostic model, analysis immunomodulator

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
observeEvent(input$ConsensusModelPageSubmoduleImmunomodulatorSubmitBtn,{
  # return nothing if not clicked
  if(input$ConsensusModelPageSubmoduleImmunomodulatorSubmitBtn == 0){
    return()
  }
  
  # return if no valid gene score
  if(any(is.null(ConsensusModelReactiveValues$score_list))){
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
  
  input$ConsensusModelPageSubmoduleImmunomodulatorSubmitBtn
  
  isolate({
    
    library(ComplexHeatmap)
    library(circlize)
    # library(heatmaply)
    
    ##### process with data #####
    
    load(
      paste0(
        "./db/raw_data/",
        str_replace_all(globReactiveValues$homePageCancerSelected, " +", "_"),
        '/ims_expression.rda'
      )
    )
    
    # -------------------------------------------------------------------------
    ### Prepare score ###
    # -------------------------------------------------------------------------
    #### 定义self_name 评分的名字
    self_name <- 'Consensus score'
    score_list <- ConsensusModelReactiveValues$score_list
    score_list <- lapply(score_list, function(x) {
      x <- data.frame(ID = rownames(x),
                      Score = x[, 1],
                      row.names = rownames(x))
      return(x)
    })
    
    ims_expr_list <- ims_expr_list[names(score_list)]
    
    cor_list <- map2(score_list,ims_expr_list,function(x,y){
      id <- intersect(rownames(x),colnames(y))
      x <- x[id,]
      y <- y[,id]
      l <- cor(t(y),x$Score)%>%as.data.frame()%>%tibble::rownames_to_column('ID')
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
    
    cols <- colorRamp2(c(-1,-0.5,0,0.5,1),c('#0077b6','#48cae4','white',alpha('#e3170a',0.5),'#e3170a'))
    
    if(ncol(cor_data)==1) {
      ims <- ims[ims$V3 %in% rownames(cor_data), ]
      tmp <-
        cor_data[match(ims$V3, rownames(cor_data)), ] %>% as.data.frame()
      rownames(tmp) <- ims$V3
      colnames(tmp) <- colnames(cor_data)
    } else{
      tmp <- cor_data[match(ims$V3, rownames(cor_data)), ]
    }
    
    left_ann <-
      HeatmapAnnotation(
        Immunomodulators = ims$V2,
        which = 'row',
        show_legend = T,
        show_annotation_name = F,
        simple_anno_size = unit(2, 'mm'),
        col = list(
          Immunomodulators = c(
            'Antigen presentation' = pal_nejm(alpha = 0.8)(8)[1],
            'Immunoinhibitor' = pal_nejm(alpha = 0.8)(8)[2],
            'Immunostimulator' = pal_nejm(alpha = 0.8)(8)[3],
            'Chemokine' = pal_nejm(alpha = 0.8)(8)[4],
            'Receptor' = pal_nejm(alpha = 0.8)(8)[5]
          )
        )
      )
    
    # ##### heatmaply first #####
    # # used for showing in web
    # # check https://www.rdocumentation.org/packages/heatmaply/versions/1.3.0/topics/heatmaply for more information
    # output$ConsensusModelPageSubmoduleImmunomodulatorHeatmap <-
    #   renderPlotly({
    #     heatmaply::heatmaply(
    #       as.matrix(tmp),
    #       Rowv = FALSE,
    #       colors = c(
    #         '#0077b6',
    #         '#48cae4',
    #         'white',
    #         alpha('#e3170a', 0.5),
    #         '#e3170a'
    #       ),
    #       Colv = TRUE,
    #       na.value = "grey50",
    #       hide_colorbar = FALSE,
    #       row_dend_left = FALSE,
    #       branches_lwd = 0.3,
    #       row_side_colors = data.frame(IMS.Type = ann),
    #       height = "2000"
    #     )
    #   })
    # 
    ##### complexheatmap later #####
    # used for download pdf
    #print(dim(tmp))
    tmp[is.na(tmp)] = 0
    plot.res.heatmap <-
      tryCatch(
        Heatmap(
          as.matrix(tmp),
          na_col = NULL,
          name = 'Correlation',
          border = F,
          col = cols,
          row_names_side = 'left',
          row_split = rep(c(1, 2, 3, 4, 5), times = as.numeric(table(ims$V2))),
          row_gap = unit(5, 'mm'),
          left_annotation = left_ann,
          row_title = NULL,
          width = ncol(tmp) * unit(5.5, "mm"),
          height = nrow(tmp) * unit(5, "mm"),
          cluster_rows = T,
          show_row_dend = F
        ),
        error = function(e) {
          Heatmap(
            as.matrix(tmp),
            na_col = NULL,
            name = 'Correlation',
            border = F,
            col = cols,
            row_names_side = 'left',
            row_split = rep(c(1, 2, 3, 4, 5), times =
                              as.numeric(table(ims$V2))),
            row_gap = unit(5, 'mm'),
            left_annotation = left_ann,
            row_title = NULL,
            width = ncol(tmp) * unit(5.5, "mm"),
            height = nrow(tmp) * unit(5, "mm"),
            cluster_rows = F,
            show_row_dend = F
          )
        }
      )
    
    ##### output #####
    output$ConsensusModelPageSubmoduleImmunomodulatorHeatmapHtml <- renderUI({
      plot.width = paste0((ncol(tmp) * 25 + 300),"px")
      div(
        plotOutput(
          outputId = "ConsensusModelPageSubmoduleImmunomodulatorHeatmap",
          height = "2200px",
          width = plot.width
        ),
        style = "height:400px; overflow-y:scroll; overflow-x:auto; "
      )
      
    })
    
    output$ConsensusModelPageSubmoduleImmunomodulatorHeatmap <- renderPlot({
      return(plot.res.heatmap)
    })
    
    # download file
    output$ConsensusModelPageSubmoduleImmunomodulatorHeatmapDownloadPdfBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_ConsensusModel_Immunomodulator_",
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
    
    output$ConsensusModelPageSubmoduleImmunomodulatorHeatmapDownloadPngBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_ConsensusModel_CellInfiltration_",
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
    ims.name = rownames(tmp)
    
    shiny::updateSelectInput(
      session = session,
      inputId = "ConsensusModelPageSubmoduleImmunomodulatorSubplotCohortSelection",
      choices = cohort.name
    )
    
    shiny::updateSelectInput(
      session = session,
      inputId = "ConsensusModelPageSubmoduleImmunomodulatorSubplotSignatureSelection",
      choices = ims.name
    )
    
    ##### subplot start #####
    observeEvent(input$ConsensusModelPageSubmoduleImmunomodulatorSubplotSubmitBtn,{
      
      ###### get user input ######
      Cohort = input$ConsensusModelPageSubmoduleImmunomodulatorSubplotCohortSelection
      IMS_ID = input$ConsensusModelPageSubmoduleImmunomodulatorSubplotSignatureSelection
      
      if(!IMS_ID%in%rownames(ims_expr_list[[Cohort]])){
        # message(paste0(Cohort,' absents ',IMS_ID,', please change another gene with red or blue color!'))
        sendSweetAlert(
          session = session,
          title = "Error",
          text = paste0(
            Cohort,
            ' absents ',
            IMS_ID,
            ', please change another gene with red or blue color!'
          ),
          closeOnClickOutside = TRUE,
          type = "error",
          btn_labels = "Ok"
        )
        return()
      }
      
      IMS_ID2 <- IMS_ID
      cor.method.selected = input$ConsensusModelPageSubmoduleImmunomodulatorSubplotCorMethodSelection
      
      ###### prepare data ######
      cor_tmp <-
        merge(score_list[[Cohort]],
              t(ims_expr_list[[Cohort]][IMS_ID, ]),
              by.x = 1,
              by.y = 0)[, -1]
      cor_tmp <- na.omit(cor_tmp)
      colnames(cor_tmp) <- gsub('-', '_', colnames(cor_tmp))
      IMS_ID <- gsub('-', '_', IMS_ID)
      
      # line regression
      fit <- cor.test(cor_tmp[, 1], cor_tmp[, 2], method = cor.method.selected) ##可自定义相关性计算方法
      
      ###### cor plot ######
      plot.cor.plot <- ggplot(cor_tmp, aes_string('Score', IMS_ID)) +
        geom_point(col = '#ef476f', alpha = 0.9, size = 2) +
        geom_smooth(
          method = lm,
          se = T,
          na.rm = T,
          fullrange = T,
          size = 1.8,
          col = "#9a8c98"
        ) +
        geom_rug(col = "#f9c74f", size = 1) +
        theme_bw(base_rect_size = 1.5) +
        labs(
          x = self_name,
          y = paste0(IMS_ID2, ' Expression (z-score)'),
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
      output$ConsensusModelPageSubmoduleImmunomodulatorSubplot <- renderPlot({
        return(plot.cor.plot)
      })
      
      # download subplot
      output$ConsensusModelPageSubmoduleImmunomodulatorSubplotDownloadPdfBtn <- downloadHandler(
        filename = function(){
          str_replace_all(
            paste(
              "BEST_ConsensusModel_Immunomodulator_",
              self_name,
              "_",
              Cohort,
              "_",
              IMS_ID,
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
      
      output$ConsensusModelPageSubmoduleImmunomodulatorSubplotDownloadPngBtn <- downloadHandler(
        filename = function(){
          str_replace_all(
            paste(
              "BEST_ConsensusModel_Immunomodulator_",
              self_name,
              "_",
              Cohort,
              "_",
              IMS_ID,
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
        id = "ConsensusModelPageSubmoduleImmunomodulatorSubplotDiv",
        anim = TRUE,
        animType = "fade"
      )
    })
    
    ## show heatmap div 
    shinyjs::show(
      id = "ConsensusModelPageSubmoduleImmunomodulatorHeatmapDiv",
      anim = TRUE,
      animType = "fade"
    )
    
    
  }) # within isolate
})


