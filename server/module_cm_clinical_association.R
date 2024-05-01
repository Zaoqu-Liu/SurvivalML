# this script render the server of analysis page of 
# module gene list, analysis clinical association

### set up with server end

#### specific packages and options ####
options(digits = 2)

#### start clinical association analysis ####
plot.out <- reactiveValues(
  plot = NULL, # ggplot object
  plot.num = 0 # number of sub plots
)

observeEvent(input$ConsensusModelPageSubmoduleClinlcalAssociationSubmitBtn,{
  
  # return nothing if not clicked
  if(input$ConsensusModelPageSubmoduleClinlcalAssociationSubmitBtn == 0){
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
  
  
  # isolate calculation
  input$ConsensusModelPageSubmoduleClinlcalAssociationSubmitBtn
  
  isolate({
    
    
    #### prepare data first ####
    # send reactive values in here
    total_clin_list <- cancer.data$total_clin_list
    gtf <- gtf_data$gtf
    score_list <- ConsensusModelReactiveValues$score_list
    
    Sur_ids <- c('OS','OS.time','RFS','RFS.time','DFS','DFS.time',
                 'PFS','PFS.time','DSS','DSS.time') ## 生存变量ID
    total_clin_list <- total_clin_list[names(score_list)]
    names <- names(total_clin_list)
    
    #### 定义self_name 评分的名字
    self_name <- 'Consensus score'
    
    # -------------------------------------------------------------------------
    ### 汇总临床变量 ###
    # -------------------------------------------------------------------------
    clin_var_list <- lapply(total_clin_list,function(x){colnames(x)[-1]})
    clin_vars <- data.frame()
    for (i in names) {
      clin_vars <- rbind(clin_vars,data.frame(Cohort=i,Var=clin_var_list[[i]]))
    }
    clin_vars <- clin_vars[order(clin_vars$Var),]
    Sur_ids <- clin_vars[clin_vars$Var%in%Sur_ids,]
    clin_vars <- clin_vars[!clin_vars$Var%in%Sur_ids$Var,]
    clin_ids <- unique(clin_vars$Var)
    clin_ids <- clin_ids[clin_ids!='Tissue']
    
    # print(clin_ids)
    
    # -------------------------------------------------------------------------
    ### Prepare score ###
    # -------------------------------------------------------------------------
    
    score_list <- lapply(score_list,function(x){
      x <- data.frame(ID=rownames(x),Score=x[,1])
      return(x)
    })
    
    # -------------------------------------------------------------------------
    ### Clinical association ###
    # -------------------------------------------------------------------------
    
    #### set up user input ####
    # plot color palette
    Color_palette <- input$ConsensusModelPageSubmoduleClinlcalAssociationPlotColorSelection
    
    # plot transparency
    alpha <- input$ConsensusModelPageSubmoduleClinlcalAssociationPlotTransparencySelection
    
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
    # print(cols)
    
    # update target selection 
    updateSelectInput(
      session = session,
      inputId = "ConsensusModelPageSubmoduleClinlcalAssociationTargetSelection",
      choices = clin_ids
    )
    
    # library(showtext)
    # showtext_auto()
    
    # generate plot
    clin_plot_list <- list()
    for (i in clin_ids) {
      plots <- list()
      for (j in clin_vars$Cohort[clin_vars$Var == i]) {
        tmp <- na.omit(merge(total_clin_list[[j]][,c('ID',i)],score_list[[j]],by=1)[,-1])
        tmp[,1] <- as.character(tmp[,1])
        width <- length(unique(tmp[,1]))
        
        if (width > 1) {
          if (shapiro.test(tmp[, 2])$p.val < 0.05) {
            test_type <- ifelse(width > 2, 'kruskal.test', 'wilcox.test')
          } else{
            test_type <- ifelse(width > 2, 'anova', 't.test')
          }
          
          plots[[j]] <- ggplot(tmp, aes_string(i, 'Score')) +
            geom_jitter(
              shape = 21,
              size = 2,
              width = 0.2,
              aes_string(fill = i, color = i)
            ) +
            geom_boxplot(
              outlier.colour = NA,
              aes_string(fill = i),
              color = 'black',
              size = 0.6,
              alpha = 0.65
            ) +
            geom_violin(
              alpha = 0.5,
              aes_string(fill = i),
              color = NA,
              trim = T
            ) +
            stat_compare_means(
              label.y = max(tmp[, 'Score']) * 1.05,
              method = test_type,
              color = 'black',
              size = 5
            ) +
            scale_fill_manual(values = cols) +
            scale_color_manual(values = cols) +
            expand_limits(y = max(tmp[, 'Score']) * 1.1) +
            theme_bw(base_rect_size = 2) +
            labs(y = self_name, title = j) +
            theme(
              axis.text.y = element_text(size = 11, colour = 'black'),
              axis.text.x = element_text(
                size = 14,
                colour = 'black',
                angle = 50,
                hjust = 1
                # family = "STKaiti"
              ),
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
              axis.ticks.x = element_blank()
            ) +
            theme(plot.margin = margin(
              t = 10,
              b = 10,
              l = 20,
              r = 20
            ))
          
          plots[[j]]$width <- width
        }
      }
      clin_plot_list[[i]] <- plots
    }
    clin_plot_list <- clin_plot_list[sapply(clin_plot_list,length)!=0]
    
    output_clin_plots <- list()
    for (i in names(clin_plot_list)) {
      width_vector <-
        sapply(clin_plot_list[[i]], function(x) {
          x$width
        }) %>% as.numeric()
      width_vector <-
        ifelse(width_vector == 2, 1, ifelse(width_vector == 3, 1.4, ifelse(width_vector ==
                                                                             4, 1.8, 2.2)))
      #nrow <- ifelse(length(width_vector)<=5,1,ifelse(length(width_vector)<=10,2,ifelse(length(width_vector)<=15,3,4)))
      if(length(names(clin_plot_list[[i]])) <= 5){
        output_clin_plots[[i]] <-
          plot_grid(plotlist = clin_plot_list[[i]],
                    rel_widths = width_vector,
                    nrow = 1)
      }else{
        output_clin_plots[[i]] <-
          plot_grid(plotlist = clin_plot_list[[i]],
                    rel_widths = width_vector,
                    ncol = 5)
      }
    }
    
    ## output plot 
    observeEvent(input$ConsensusModelPageSubmoduleClinlcalAssociationTargetSelection,{
      
      output$ConsensusModelPageSubmoduleClinlcalAssociationResultPlotHtmlOutput <- renderUI({
        
        # # 图像显示宽度
        # div.width = length(names(clin_plot_list[[input$ConsensusModelPageSubmoduleClinlcalAssociationTargetSelection]])) * 20
        # if(div.width >= 100){
        #   div.width = 100
        # }
        
        plot.num = length(names(clin_plot_list[[input$ConsensusModelPageSubmoduleClinlcalAssociationTargetSelection]]))
        plot.height = ceiling(plot.num / 5) * 400
        plot.width = ifelse(plot.num > 5, 5, plot.num) * 230
        
        # 图片的宽和高在第一次（未选择表型之前）等于1，引发报错
        if(plot.height == 0 | plot.width == 0){
          return()
        }
        
        div(
          div(
            plotOutput(
              outputId = "ConsensusModelPageSubmoduleClinlcalAssociationResultPlotOutput",
              # width = paste(div.width,"%",sep=""),
              # width = "100%",
              width = paste0(plot.width,"px"),
              height = paste0(plot.height,"px")
            )
          ),
          style="height:400px;overflow-y:scroll;"
        )
      })
      
      output$ConsensusModelPageSubmoduleClinlcalAssociationResultPlotOutput <- renderPlot({
        
        # generate plot
        p.out = output_clin_plots[[input$ConsensusModelPageSubmoduleClinlcalAssociationTargetSelection]]
        
        # set global values
        plot.out$plot <- p.out
        plot.out$plot.num <- length(clin_plot_list[[input$ConsensusModelPageSubmoduleClinlcalAssociationTargetSelection]])
        
        # return value
        return(p.out)
        
      })
    })
    
    # download 
    # generate plot in pdf
    output$ConsensusModelPageSubmoduleClinlcalAssociationResultPlotDownloadPdfBtn <- downloadHandler(
      filename = function() {
        fname = paste(
          "BEST_ConsensusModel_ClinicalAssociation_",
          globReactiveValues$homePageCancerSelected,
          "_",
          input$ConsensusModelPageSubmoduleClinlcalAssociationTargetSelection,
          "_",
          uniqID(idLength = 10),
          ".pdf"
        )
        return(str_remove_all(fname,"\\s+"))
      },
      
      
      content = function(fname){
        
        plot.num = length(names(clin_plot_list[[input$ConsensusModelPageSubmoduleClinlcalAssociationTargetSelection]]))
        plot.height = ceiling(plot.num / 5) * 6
        
        save_plot(
          fname,
          plot.out$plot,
          base_height = plot.height,
          base_width = 3.6 * ifelse(plot.out$plot.num < 5, plot.out$plot.num, 5)  # set plot width according to plot numbers
        )
      },
      
      contentType = "pdf"
    )
    
    # generate plot in png
    output$ConsensusModelPageSubmoduleClinlcalAssociationResultPlotDownloadPngBtn <- downloadHandler(
      filename = function() {
        fname = paste(
          "BEST_ConsensusModel_ClinicalAssociation_",
          globReactiveValues$homePageCancerSelected,
          "_",
          input$ConsensusModelPageSubmoduleClinlcalAssociationTargetSelection,
          "_",
          uniqID(idLength = 10),
          ".png"
        )
        return(str_remove_all(fname,"\\s+"))
      },
      
      
      content = function(fname){
        
        plot.num = length(names(clin_plot_list[[input$ConsensusModelPageSubmoduleClinlcalAssociationTargetSelection]]))
        plot.height = ceiling(plot.num / 5) * 6
        
        ggsave(
          fname,
          plot.out$plot,
          device = "png",
          units = "in",
          bg = "white",
          height = plot.height,
          width = 3.6 * ifelse(plot.out$plot.num < 5, plot.out$plot.num, 5)  # set plot width according to plot numbers
        )
      },
      
      contentType = "png"
    )
    
    # # generate multiple plots and save in zip
    # # generate plots UI
    # output$ConsensusModelPageSubmoduleClinlcalAssociationResultPlotHtml <- renderUI(
    #   
    #   div(
    #     style = "height:500px;overflow-y:scroll;",
    #     
    #     # plot ui
    #     fluidRow(
    #       width = 12,
    #       
    #       # generate plots, number equal to datasets num
    #       lapply(1:nrow(cancer.dataset()), function(i){
    #         column(
    #           width = 4,
    #           plotOutput(
    #             outputId = paste("ConsensusModelPageSubmoduleClinlcalAssociationResultPlot",i,sep="")
    #           )
    #         )
    #       })
    #     )
    #   )
    # )
    # 
    # # generate and save plots
    # # create folders
    # dir.name = paste("BEST_ConsensusModel_ClinicalAssociation_",uniqIDWithDate(),sep="")
    # tmp.dir = tempdir()
    # plot.file.path = paste(tmp.dir,"/",dir.name,sep="")
    # dir.create(plot.file.path,recursive = FALSE)
    # 
    # # generate plots
    # 
    # # output$ConsensusModelPageSubmoduleClinlcalAssociationResultPlot1 <- renderPlot(
    # #   demoPlot()
    # # )
    # lapply(1:nrow(cancer.dataset()), function(i){
    #   output[[paste("ConsensusModelPageSubmoduleClinlcalAssociationResultPlot",i,sep="")]] <- renderPlot({
    #     #### need mod ####
    #     p = demoPlot()
    #     
    #     # save plot
    #     save_plot(
    #       filename = paste(plot.file.path,"/Plot",i,".pdf",sep=""),
    #       plot = p,
    #       base_height = 5,
    #       base_width = 5
    #     )
    #     
    #     # return plot
    #     return(p)
    #   })
    # })
    # 
    # # download plots
    # output$ConsensusModelPageSubmoduleClinlcalAssociationResultPlotDownloadBtn <- downloadHandler(
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
    # remove dir
    # file.remove(plot.file.path)
    
  }) # within isolate
  
  # show plots
  shinyjs::show(
    id = "ConsensusModelPageSubmoduleClinlcalAssociationResultPlotDiv",
    anim = TRUE,
    animType = "fade"
  )
  
})


