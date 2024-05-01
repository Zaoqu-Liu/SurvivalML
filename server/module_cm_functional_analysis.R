# this script render the server of analysis page of 
# module consensus model, analysis functional analysis

# set up with server end
#### specific pkgs and options ####
options(digits = 2)

#### load data ####
# set reactive values
# allgene.cor.ids <- reactiveValues(
#   total_gene_ids = NULL
# )
# observeEvent(input$HomePageAnalysisGoBtn1,{
#   load(paste(
#     "./db/raw_data/",
#     str_replace_all(globReactiveValues$homePageCancerSelected, " +", "_"),
#     "/allgene_cor_ids.rda",
#     sep = ""
#   ))
#   
#   allgene.cor.ids$total_gene_ids <- total_gene_ids
# })

#### reactive value ####
ConsensusModelPageReactiveValue <- (
  cordata = NULL # 相关联基因数据
)

#### check user input ####
# nothing to check before start btn clicked

#### start Over representitive analysis ####
observeEvent(input$ConsensusModelPageSubmoduleFunctionalAnalysisOraSubmitBtn,{
  # return nothing if not clicked
  if(input$ConsensusModelPageSubmoduleFunctionalAnalysisOraSubmitBtn == 0){
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
  
  # return error if qvlaue not set
  if(input$ConsensusModelPageSubmoduleFunctionalAnalysisOraQvalueInput == "") {
    sendSweetAlert(
      session = session,
      title = "Error",
      text = "Please select a cutoff q value.",
      type = "error",
      btn_labels = "OK",
      closeOnClickOutside = TRUE
    )
    
    return()
  }
  
  input$ConsensusModelPageSubmoduleFunctionalAnalysisOraSubmitBtn
  
  isolate({
    
    library(clusterProfiler)
    library(org.Hs.eg.db)
    library(ggridges)
    library(KEGG.db)
    
    # help information before start 
    sendSweetAlert(
      session = session,
      title = "Attention",
      text = "The calculation will cost several minutes. Please be patient...",
      type = "info",
      btn_labels = "Ok",
      closeOnClickOutside = TRUE
    )
    
    # -------------------------------------------------------------------------
    ### Prepare expression ###
    # -------------------------------------------------------------------------
    
    
    # send reactive valus in
    # total_gene_ids = allgene.cor.ids$total_gene_ids
    
    # load  data
    load(
      paste0(
        "./db/raw_data/",
        str_replace_all(globReactiveValues$homePageCancerSelected, " +", "_"),
        "/mRNA.rda"
      )
    )
    
    gtf <- gtf_data$gtf
    score_list <- ConsensusModelReactiveValues$score_list
    
    score_list <- lapply(score_list,function(x){
      x <- data.frame(ID=rownames(x),Score=x[,1],row.names = rownames(x))
      return(x)
    })
    
    
    # -------------------------------------------------------------------------
    ### 计算该基因表达与其它基因在每个队列的相关性，取平均值 ###
    # -------------------------------------------------------------------------
    
    # ln = load('CRC-mRNA.rda')
    
    total_expr_list <- total_expr_list[names(score_list)]
    cor_list <- map2(score_list,total_expr_list,function(x,y){
      y <- y[,x$ID]
      l <- cor(t(y),x$Score)%>%as.data.frame()%>%tibble::rownames_to_column('ID')
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
    
    cor_data$mean_cor <- rowMeans(cor_data,na.rm = T)
    cor_data <- cor_data[order(cor_data$mean_cor,decreasing = T),]
    cordata <- data.frame(ID=rownames(cor_data),mean_cor=cor_data$mean_cor)
    
    ID <- bitr(cordata$ID,fromType = 'SYMBOL',toType = 'ENTREZID',OrgDb = org.Hs.eg.db)
    cordata <- merge(ID,cordata,by=1)%>%dplyr::arrange(desc(mean_cor))
    cordata <- cordata[cordata$mean_cor<2,]%>%na.omit()
    
    # save cordata
    # ConsensusModelPageReactiveValue$cordata <- cordata
    
    # -------------------------------------------------------------------------
    ### Over Representation Analysis ###
    # -------------------------------------------------------------------------
    # input from UI
    input.gene.num = as.numeric(input$ConsensusModelPageSubmoduleFunctionalAnalysisOraGeneNumInput)
    input.q.cutoff = as.numeric(input$ConsensusModelPageSubmoduleFunctionalAnalysisOraQvalueInput)
    input.go.plot.width = input$ConsensusModelPageSubmoduleFunctionalAnalysisOraGoPlotWidthInput
    input.kegg.plot.width = input$ConsensusModelPageSubmoduleFunctionalAnalysisOraKeggPlotWidthInput
    # input.go.cat = input$ConsensusModelPageSubmoduleFunctionalAnalysisOraGoCategoryInput
    #print(input.go.cat)
    
    select_pcor_ids <- cordata$ENTREZID[1:input.gene.num] ##用户可以自定义数量
    select_ncor_ids <- cordata$ENTREZID[nrow(cordata):(nrow(cordata) - (input.gene.num - 1))]
    
    # print(select_pcor_ids)
    # print(select_ncor_ids)
    
    # GO analysis-------------------------------------------------------------------------
    load("./db/raw_data/common/GOref.rda")
    
    GO_pos <-
      lzq_enrichGO(
        gene = select_pcor_ids,
        qvalueCutoff = input.q.cutoff,
        GO2GENE = GO2GENE,
        GOdetial = GOdetial
      )
    GO_neg <-
      lzq_enrichGO(
        gene = select_ncor_ids,
        qvalueCutoff = input.q.cutoff,
        GO2GENE = GO2GENE,
        GOdetial = GOdetial
      )
    
    # data table
    tmp <- lzq_GO_merge(GO_pos, GO_neg)
    go.table.out <- tmp
    
    # plot now
    p.go <- lzq_GO_plot(tmp, width = input.go.plot.width) ## width可以自己设置
    output$ConsensusModelPageSubmoduleFunctionalAnalysisOraGOPlotHtml <- renderUI({
      p.go.height = paste0((nrow(go.table.out) * 12 + 100),"px")
      # p.go.width = paste0(ceiling((nrow(go.table.out) * 12 + 100) * 1.36),"px")
      p.go.width = "800px"
      
      div(
        plotOutput(
          outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraGOPlot",
          height = p.go.height,
          width = p.go.width
        ),
        style = "height:400px;overflow-y:scroll;overflow-x:auto;"
      )
    })
    
    output$ConsensusModelPageSubmoduleFunctionalAnalysisOraGOPlot <- renderPlot(
      return(p.go)
    )
    
    # download file
    output$ConsensusModelPageSubmoduleFunctionalAnalysisOraGOPlotDownloadPdfBtn <- downloadHandler(
      filename = function(){
        paste("BEST_ConsensusModel_FunctionalAnalysis_ORA_GO_",uniqID(),".pdf",sep="")
      },
      
      content = function(fname){
        save_plot(
          fname,
          p.go,
          base_height = 7,
          base_width = 9.5
        )
      },
      
      contentType = ".pdf"
    )
    
    output$ConsensusModelPageSubmoduleFunctionalAnalysisOraGOPlotDownloadPngBtn <- downloadHandler(
      filename = function(){
        paste("BEST_ConsensusModel_FunctionalAnalysis_ORA_GO_",uniqID(),".png",sep="")
      },
      
      content = function(fname){
        save_plot(
          fname,
          p.go,
          bg = "white",
          base_height = 7,
          base_width = 9.5
        )
      },
      
      contentType = ".pdf"
    )
    
    output$ConsensusModelPageSubmoduleFunctionalAnalysisOraGOPlotDownloadTableBtn <- downloadHandler(
      filename = function(){
        paste("BEST_ConsensusModel_FunctionalAnalysis_ORA_GO_",uniqID(),".csv",sep="")
      },
      
      content = function(fname){
        write.csv(
          go.table.out,
          fname,
          row.names = FALSE,
          quote = FALSE
        )
      },
      
      contentType = ".csv"
    )
    
    # KEGG analysis-------------------------------------------------------------------------
    KEGG_pos <-
      enrichKEGG(
        gene = select_pcor_ids,
        organism = 'hsa',
        qvalueCutoff = input.q.cutoff,
        minGSSize = 1,
        maxGSSize = 50000,
        use_internal_data = TRUE
      )
    KEGG_neg <-
      enrichKEGG(
        gene = select_ncor_ids,
        organism = 'hsa',
        qvalueCutoff = input.q.cutoff,
        minGSSize = 1,
        maxGSSize = 50000,
        use_internal_data = TRUE
      )
    
    # table
    tmp <- lzq_KEGG_merge(KEGG_pos,KEGG_neg)
    kegg.table <- tmp
    
    # plot
    p.kegg <- lzq_KEGG_plot(tmp,width = input.kegg.plot.width) ## width可以自己设置
    
    output$ConsensusModelPageSubmoduleFunctionalAnalysisOraKEGGPlotHtml <- renderUI({
      p.kegg.height = paste0((nrow(kegg.table) * 12 + 100),"px")
      #p.kegg.width = paste0(ceiling((nrow(kegg.table) * 12 + 100) * 1.36),"px")
      p.kegg.width = "800px"
      
      div(
        plotOutput(
          outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisOraKEGGPlot",
          height = p.kegg.height,
          width = p.kegg.width
        ),
        style = "height:400px;overflow-y:scroll;overflow-x:auto;"
      )
    })
    output$ConsensusModelPageSubmoduleFunctionalAnalysisOraKEGGPlot <- renderPlot({
      return(p.kegg)
    })
    
    # download file
    output$ConsensusModelPageSubmoduleFunctionalAnalysisOraKEGGPlotDownloadPdfBtn <- downloadHandler(
      filename = function(){
        paste("BEST_ConsensusModel_FunctionalAnalysis_ORA_KEGG_",uniqID(),".pdf",sep="")
      },
      
      content = function(fname){
        save_plot(
          fname,
          p.kegg,
          base_height = 7,
          base_width = 13
        )
      },
      
      contentType = ".pdf"
    )
    
    output$ConsensusModelPageSubmoduleFunctionalAnalysisOraKEGGPlotDownloadPngBtn <- downloadHandler(
      filename = function(){
        paste("BEST_ConsensusModel_FunctionalAnalysis_ORA_KEGG_",uniqID(),".png",sep="")
      },
      
      content = function(fname){
        save_plot(
          fname,
          p.kegg,
          bg = "white",
          base_height = 7,
          base_width = 13
        )
      },
      
      contentType = ".png"
    )
    
    output$ConsensusModelPageSubmoduleFunctionalAnalysisOraKEGGPlotDownloadTableBtn <- downloadHandler(
      filename = function(){
        paste("BEST_ConsensusModel_FunctionalAnalysis_ORA_KEGG_",uniqID(),".csv",sep="")
      },
      
      content = function(fname){
        write.csv(
          kegg.table,
          fname,
          quote = FALSE,
          row.names = FALSE
        )
      },
      
      contentType = ".csv"
    )
    
    ## show the div
    shinyjs::show(
      id = "ConsensusModelPageSubmoduleFunctionalAnalysisORATabsetPanelDiv",
      anim = TRUE,
      animType = "fade"
    ) 
  }) #within isolate
  
})


#### start gsea analysis ####
observeEvent(input$ConsensusModelPageSubmoduleFunctionalAnalysisGseaSubmitBtn,{
  # return nothing if not clicked
  if(input$ConsensusModelPageSubmoduleFunctionalAnalysisGseaSubmitBtn == 0){
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
  
  input$ConsensusModelPageSubmoduleFunctionalAnalysisGseaSubmitBtn
  # start analysis here
  isolate({
    
    library(clusterProfiler)
    library(org.Hs.eg.db)
    library(ggridges)
    library(BiocParallel)
    
    # help information before start 
    sendSweetAlert(
      session = session,
      title = "Attention",
      text = "The calculation will cost several minutes. Please be patient...",
      type = "info",
      btn_labels = "Ok",
      closeOnClickOutside = TRUE
    )
    
    
    # load gsea data
    load("./db/raw_data/common/GSEA_genesets.rda")
    
    # send reactive values in 
    # total_gene_ids = allgene.cor.ids$total_gene_ids
    
    # load  data
    load(
      paste0(
        "./db/raw_data/",
        str_replace_all(globReactiveValues$homePageCancerSelected, " +", "_"),
        "/mRNA.rda"
      )
    )
    
    gtf <- gtf_data$gtf
    score_list <- ConsensusModelReactiveValues$score_list
    
    score_list <- lapply(score_list,function(x){
      x <- data.frame(ID=rownames(x),Score=x[,1],row.names = rownames(x))
      return(x)
    })
    
    # -------------------------------------------------------------------------
    ### 计算该基因表达与其它基因在每个队列的相关性，取平均值 ###
    # -------------------------------------------------------------------------
    
    # ln = load('CRC-mRNA.rda')
    
    total_expr_list <- total_expr_list[names(score_list)]
    cor_list <- map2(score_list,total_expr_list,function(x,y){
      y <- y[,x$ID]
      l <- cor(t(y),x$Score)%>%as.data.frame()%>%tibble::rownames_to_column('ID')
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
    
    cor_data$mean_cor <- rowMeans(cor_data,na.rm = T)
    cor_data <- cor_data[order(cor_data$mean_cor,decreasing = T),]
    cordata <- data.frame(ID=rownames(cor_data),mean_cor=cor_data$mean_cor)
    
    ID <- bitr(cordata$ID,fromType = 'SYMBOL',toType = 'ENTREZID',OrgDb = org.Hs.eg.db)
    cordata <- merge(ID,cordata,by=1)%>%dplyr::arrange(desc(mean_cor))
    cordata <- cordata[cordata$mean_cor<2,]%>%na.omit()
    
    kegg_sets <- kegg_sets[kegg_sets$gene%in%cordata$ENTREZID,]
    kegg_sets <- kegg_sets[kegg_sets$term%in%names(table(kegg_sets$term)>1),]
    go_sets <- go_sets[go_sets$gene%in%cordata$ENTREZID,]
    go_sets <- go_sets[go_sets$term%in%names(table(go_sets$term)>1),]
    hall_sets <- hall_sets[hall_sets$gene%in%cordata$ENTREZID,]
    hall_sets <- hall_sets[hall_sets$term%in%names(table(hall_sets$term)>1),]
    
    gene_rank_list <- cordata$mean_cor
    names(gene_rank_list) <- cordata$ENTREZID
    
    
    ##### GSEA-GO #####
    GSEA_GO <- GSEA(gene_rank_list,
                    pvalueCutoff = 1,
                    TERM2GENE = go_sets,
                    nPermSimple=500,
                    BPPARAM=SerialParam())
    GSEA_GO@result <- lzq_GSEA_merge(GSEA_GO)[[1]]
    cate <- lzq_GSEA_merge(GSEA_GO)[[2]] + lzq_GSEA_merge(GSEA_GO)[[3]]
    
    
    # plot gsea go
    plot.gsea.go <- ridgeplot(
      GSEA_GO,
      showCategory = cate,
      fill = "p.adjust",
      core_enrichment = TRUE,
      label_format = 80
    ) +
      geom_vline(
        xintercept = 0,
        linetype = 2,
        color = 'grey30',
        alpha = 0.7
      ) +
      theme_classic(base_rect_size = 2) +
      labs(x = 'Enrichment Score', y = NULL, title = 'GSEA-GO Analysis') +
      scale_x_continuous(expand = c(0.01, 0.01)) +
      scale_fill_gradient(low = alpha('#21b6af', 0.7),
                          high = alpha('#eeba4d', 0.7)) +
      theme(
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 12, colour = 'black'),
        axis.title.x = element_text(
          size = 13,
          colour = 'darkred',
          face = 'bold'
        ),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(
          hjust = 0.5,
          size = 14,
          colour = 'darkred',
          face = 'bold'
        ),
        legend.title = element_text(size = 13, colour = 'black'),
        legend.text = element_text(size = 12, colour = 'black')
      )
    
    # plot output
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotHtml <- renderUI({
      div(
        plotOutput(
          outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlot",
          width = "800px",
          height = "600px"
        ),
        style = "overflow-x:auto;overflow-y:scroll;height:400px;"
      )
    })
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlot <- renderPlot({
      return(plot.gsea.go)
    })
    
    # download plot
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotDownloadPdfBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_ConsensusModel_FunctionalAnalysis_GSEA_GO_",
            "_ridgePlot_",
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
          plot.gsea.go,
          base_height = 8,
          base_width = 10
        )
      },
      
      contentType = ".pdf"
    )
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotDownloadPngBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_ConsensusModel_FunctionalAnalysis_GSEA_GO_",
            "_ridgePlot_",
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
          plot.gsea.go,
          base_height = 8,
          base_width = 10
        )
      },
      
      contentType = ".png"
    )
    
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotDownloadTableBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_ConsensusModel_FunctionalAnalysis_GSEA_GO_",
            "_ridgePlot_",
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
          GSEA_GO@result,
          fname,
          quote = FALSE,
          row.names = FALSE
        )
      },
      
      contentType = ".csv"
    )
    
    ###### gsea go subplot ######
    # update selection
    updateSelectInput(
      session = session,
      inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotTermSelection",
      choices = GSEA_GO@result$ID
    )
    
    # plot gsea subplot
    observeEvent(input$ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotStartBtn,{
      term.selected = input$ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotTermSelection
      
      # sub plot
      plot.gsea.go.subplot <- lzq_gseaplot(
        GSEA_GO,
        term.selected,
        "#62929e"
      )
      
      # output 
      output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplot <- renderPlot({
        return(plot.gsea.go.subplot)
      })
      
      # download plot
      output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotDownloadPdfBtn <- downloadHandler(
        filename = function(){
          str_replace_all(
            paste(
              "BEST_ConsensusModel_FunctionalAnalysis_GSEA_GO_",
              "_",
              term.selected,
              "_GseaPlot_",
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
            plot.gsea.go.subplot,
            base_height = 5.5,
            base_width = 5
          )
        },
        
        contentType = ".pdf"
      )
      
      output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotDownloadPngBtn <- downloadHandler(
        filename = function(){
          str_replace_all(
            paste(
              "BEST_ConsensusModel_FunctionalAnalysis_GSEA_GO_",
              "_",
              term.selected,
              "_GseaPlot_",
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
            plot.gsea.go.subplot,
            base_height = 5.5,
            base_width = 5
          )
        },
        
        contentType = ".png"
      )
      
      # show div
      shinyjs::show(
        id = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaGoPlotSubplotDiv",
        anim = T,
        animType = "fade"
      )
    })
    
    ###### GSEA-KEGG #####
    GSEA_KEGG <-
      GSEA(gene_rank_list,
           pvalueCutoff = 1,
           TERM2GENE = kegg_sets,
           nPermSimple=500,
           BPPARAM=SerialParam())
    GSEA_KEGG@result <-
      GSEA_KEGG@result[order(GSEA_KEGG@result$NES, decreasing = T), ]
    cate <- lzq_GSEA_merge(GSEA_KEGG)[[2]] + lzq_GSEA_merge(GSEA_KEGG)[[3]]
    
    # plot now
    plot.gsea.kegg <- ridgeplot(
      GSEA_KEGG,
      showCategory = cate,
      fill = "p.adjust",
      core_enrichment = TRUE,
      label_format = 80
    ) +
      geom_vline(
        xintercept = 0,
        linetype = 2,
        color = 'grey30',
        alpha = 0.7
      ) +
      theme_classic(base_rect_size = 2) +
      labs(x = 'Enrichment Score', y = NULL, title = 'GSEA-KEGG Analysis') +
      scale_x_continuous(expand = c(0.01, 0.01)) +
      scale_fill_gradient(low = alpha('#21b6af', 0.7),
                          high = alpha('#eeba4d', 0.7)) +
      theme(
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 12, colour = 'black'),
        axis.title.x = element_text(
          size = 13,
          colour = 'darkred',
          face = 'bold'
        ),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(
          hjust = 0.5,
          size = 14,
          colour = 'darkred',
          face = 'bold'
        ),
        legend.title = element_text(size = 13, colour = 'black'),
        legend.text = element_text(size = 12, colour = 'black')
      )
    
    # plot output
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotHtml <- renderUI({
      div(
        plotOutput(
          outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlot",
          width = "800px",
          height = "600px"
        ),
        style = "overflow-x:auto;overflow-y:scroll;height:400px;"
      )
    })
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlot <- renderPlot({
      return(plot.gsea.kegg)
    })
    
    # download plot
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotDownloadPdfBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_ConsensusModel_FunctionalAnalysis_GSEA_KEGG_",
            "_ridgePlot_",
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
          plot.gsea.kegg,
          base_height = 8,
          base_width = 10
        )
      },
      
      contentType = ".pdf"
    )
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotDownloadPngBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_ConsensusModel_FunctionalAnalysis_GSEA_KEGG_",
            "_ridgePlot_",
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
          plot.gsea.kegg,
          base_height = 8,
          base_width = 10
        )
      },
      
      contentType = ".png"
    )
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotDownloadTableBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_ConsensusModel_FunctionalAnalysis_GSEA_KEGG_",
            "_ridgePlot_",
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
          GSEA_KEGG@result,
          fname,
          row.names = FALSE,
          quote = FALSE
        )
      },
      
      contentType = ".csv"
    )
    
    ###### gsea kegg subplot ######
    # update selection
    updateSelectInput(
      session = session,
      inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotTermSelection",
      choices = GSEA_KEGG@result$ID
    )
    
    # plot gsea subplot
    observeEvent(input$ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotStartBtn,{
      term.selected = input$ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotTermSelection
      
      # sub plot
      plot.gsea.kegg.subplot <- lzq_gseaplot(
        GSEA_KEGG,
        term.selected,
        "#62929e"
      )
      
      # output 
      output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplot <- renderPlot({
        return(plot.gsea.kegg.subplot)
      })
      
      # download plot
      output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotDownloadPdfBtn <- downloadHandler(
        filename = function(){
          str_replace_all(
            paste(
              "BEST_ConsensusModel_FunctionalAnalysis_GSEA_KEGG_",
              "_",
              term.selected,
              "_GseaPlot_",
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
            plot.gsea.kegg.subplot,
            base_height = 5.5,
            base_width = 5
          )
        },
        
        contentType = ".pdf"
      )
      
      output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotDownloadPngBtn <- downloadHandler(
        filename = function(){
          str_replace_all(
            paste(
              "BEST_ConsensusModel_FunctionalAnalysis_GSEA_KEGG_",
              "_",
              term.selected,
              "_GseaPlot_",
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
            plot.gsea.kegg.subplot,
            base_height = 5.5,
            base_width = 5
          )
        },
        
        contentType = ".png"
      )
      
      # show div
      shinyjs::show(
        id = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaKeggPlotSubplotDiv",
        anim = T,
        animType = "fade"
      )
    })
    
    ##### GSEA-Hallmark #####
    GSEA_hall <-
      GSEA(gene_rank_list,
           pvalueCutoff = 1,
           TERM2GENE = hall_sets,
           nPermSimple=500,
           BPPARAM=SerialParam())
    GSEA_hall@result <-
      GSEA_hall@result[order(GSEA_hall@result$NES, decreasing = T), ]
    cate <- lzq_GSEA_merge(GSEA_hall)[[2]] + lzq_GSEA_merge(GSEA_hall)[[3]]
    
    # plot now
    plot.gsea.hallmark <- ridgeplot(
      GSEA_hall,
      showCategory = cate,
      fill = "p.adjust",
      core_enrichment = TRUE,
      label_format = 80
    ) +
      geom_vline(
        xintercept = 0,
        linetype = 2,
        color = 'grey30',
        alpha = 0.7
      ) +
      theme_classic(base_rect_size = 2) +
      labs(x = 'Enrichment Score', y = NULL, title = 'GSEA-Hallmark Analysis') +
      scale_x_continuous(expand = c(0.01, 0.01)) +
      scale_fill_gradient(low = alpha('#21b6af', 0.7),
                          high = alpha('#eeba4d', 0.7)) +
      theme(
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 12, colour = 'black'),
        axis.title.x = element_text(
          size = 13,
          colour = 'darkred',
          face = 'bold'
        ),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(
          hjust = 0.5,
          size = 14,
          colour = 'darkred',
          face = 'bold'
        ),
        legend.title = element_text(size = 13, colour = 'black'),
        legend.text = element_text(size = 12, colour = 'black')
      )
    
    # plot output
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotHtml <- renderUI({
      div(
        plotOutput(
          outputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlot",
          width = "800px",
          height = "600px"
        ),
        style = "overflow-x:auto;overflow-y:scroll;height:400px;"
      )
    })
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlot <- renderPlot({
      return(plot.gsea.hallmark)
    })
    
    # download plot
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotDownloadPdfBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_ConsensusModel_FunctionalAnalysis_GSEA_Hallmark_",
            "_ridgePlot_",
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
          plot.gsea.hallmark,
          base_height = 8,
          base_width = 10
        )
      },
      
      contentType = ".pdf"
    )
    
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotDownloadPngBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_ConsensusModel_FunctionalAnalysis_GSEA_Hallmark_",
            "_ridgePlot_",
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
          plot.gsea.hallmark,
          base_height = 8,
          base_width = 10
        )
      },
      
      contentType = ".png"
    )
    
    output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotDownloadTableBtn <- downloadHandler(
      filename = function(){
        str_replace_all(
          paste(
            "BEST_ConsensusModel_FunctionalAnalysis_GSEA_Hallmark_",
            "_ridgePlot_",
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
          GSEA_hall@result,
          fname,
          quote = FALSE,
          row.names = FALSE
        )
      },
      
      contentType = ".csv"
    )
    
    ###### gsea hallmark subplot ######
    # update selection
    updateSelectInput(
      session = session,
      inputId = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotTermSelection",
      choices = GSEA_hall@result$ID
    )
    
    # plot gsea subplot
    observeEvent(input$ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotStartBtn,{
      term.selected = input$ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotTermSelection
      
      # sub plot
      plot.gsea.hallmark.subplot <- lzq_gseaplot(
        GSEA_hall,
        term.selected,
        "#62929e"
      )
      
      # output 
      output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplot <- renderPlot({
        return(plot.gsea.hallmark.subplot)
      })
      
      # download plot
      output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotDownloadPdfBtn <- downloadHandler(
        filename = function(){
          str_replace_all(
            paste(
              "BEST_ConsensusModel_FunctionalAnalysis_GSEA_Hallmark_",
              "_",
              term.selected,
              "_GseaPlot_",
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
            plot.gsea.hallmark.subplot,
            base_height = 5.5,
            base_width = 5
          )
        },
        
        contentType = ".pdf"
      )
      
      output$ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotDownloadPngBtn <- downloadHandler(
        filename = function(){
          str_replace_all(
            paste(
              "BEST_ConsensusModel_FunctionalAnalysis_GSEA_Hallmark_",
              "_",
              term.selected,
              "_GseaPlot_",
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
            plot.gsea.hallmark.subplot,
            base_height = 5.5,
            base_width = 5
          )
        },
        
        contentType = ".png"
      )
      
      # show div
      shinyjs::show(
        id = "ConsensusModelPageSubmoduleFunctionalAnalysisGseaHallmarkPlotSubplotDiv",
        anim = T,
        animType = "fade"
      )
    })
    
    
    ## show div
    shinyjs::show(
      id = "ConsensusModelPageSubmoduleFunctionalAnalysisGSEATabsetPanelDiv",
      anim = TRUE,
      animType = "fade"
    )
  }) # within isolate
})

