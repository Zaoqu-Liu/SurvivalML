# This script contains frequenctly used functions.
# Summary of functions are listed below:


#### global values ####
cm.jobs <- list() # consensus model gene score计算后台任务列表


#### predefined functions ####
# sendEmail, 批量发邮件
# demoPlotly, 生成测试用PlotlyOutput
# demoplot, 生成测试用PlotOutput
# uniqIDWithDate, 生成带有日期的唯一的ID
# uniqID, 生成唯一ID

sendEmail <- function(reciver){
  # R 包
  pacman::p_load(mailR)
  
  # 收件人，加上开发者邮箱，让开发者获知程序正常运行中
  receiver <- c("huzai920621@126.com",reciver)
  
  # 发件人邮箱
  sender <- "shinyapp_best@163.com"
  
  # 邮件主题
  emailSubject <- paste("User feedback [", date(), "]")
  
  # 邮箱内容
  emailBody <- "There are some new user feedbacks, please check."
  
  # 邮件附件
  emailFile <- "./db/contact_user_message.xlsx"
  
  # 发送邮件
  send.mail(from = sender,
            to = receiver,
            subject = emailSubject,
            body = emailBody,
            smtp = list(host.name = "smtp.163.com", # smtp服务器主机名
                        port = 465, # 默认端口
                        user.name = sender, #用户名
                        passwd = "URBCOSCIYSBYMPFS", #密码(授权码)
                        ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,
            timeout = 60000,
            attach.files = emailFile,
            encoding = "utf-8" #编码
  )
}

# 生成plotly测试图片
demoPlotly <- function(){
  plotly::ggplotly(
    ggplot(economics[1:50,],aes(psavert, uempmed))+
      geom_line()+
      theme_bw()
  )
}

# 生成plot测试图片
demoPlot <- function(){
  ggplot(economics[1:50,],aes(psavert, uempmed))+
    geom_line()+
    theme_bw()
}

# 生成带有时间的唯一ID
uniqIDWithDate <- function(prefix = "", suffix = "", idLength = 10){
  
  # get time stamp
  date.now = str_replace_all(Sys.time(),"\\s+","_")
  # remove : with letters, or it will cause error when used in file system
  date.now = str_replace(date.now,":","H")
  date.now = str_replace(date.now,":","M")
  date.now = paste(date.now,"S",sep="")
  
  
  # random string
  rand.string = paste(sample(c(letters,LETTERS,0:9),idLength),collapse = "")
  
  # generate id
  id = paste(prefix,date.now,"_",rand.string,suffix,sep="")
  
  # return
  return(id)
  
}

# 生成唯一ID
uniqID <- function(idLength = 10){
  
  # random string
  rand.string = paste(sample(c(letters,LETTERS,0:9),idLength),collapse = "")
  
  # return
  return(rand.string)
  
}

#### preload data ####



#### predefined functions by LZQ####
#### lzq_enrichGO ####
lzq_enrichGO <-
  function(gene,
           qvalueCutoff,
           GO2GENE = GO2GENE,
           GOdetial = GOdetial) {
    x <- enricher(
      gene,
      minGSSize = 1,
      maxGSSize = 50000,
      # pvalueCutoff = 1,
      pAdjustMethod = 'BH',
      qvalueCutoff = qvalueCutoff,
      TERM2GENE = GO2GENE
    )
    x@result <- x@result %>%
      dplyr::filter(qvalue < qvalueCutoff, pvalue < 0.05)
    x@result <- merge(GOdetial, x@result[, -2], by = 1) %>%
      dplyr::rename('Description' = 'Term',
                    'ID' = 'go_id',
                    'ONTOLOGY' = 'Ontology') %>%
      arrange(p.adjust)
    return(x)
  }

#### lzq_alter_landscape ####
lzq_alter_landscape <- function(rank, Input, alterdata, cnadata) {
  rank$Group <-
    ifelse(rank[, 2] > median(rank[, 2]),
           paste0('High ', Input),
           paste0('Low ', Input))
  
  tmp <- alterdata[, rank$ID]
  tmp[tmp == ''] <- NA
  
  Top = HeatmapAnnotation(
    Group = anno_block(
      gp = gpar(fill = c('#B0997F', '#ffc857')),
      height = unit(5.5, 'mm'),
      labels = c(paste0('Low ', Input), paste0('High ', Input)),
      labels_gp = gpar(
        cex = 0.85,
        col = "white",
        fontface = 'bold'
      )
    ),
    border = T,
    show_annotation_name = F
  )
  
  low <- tmp[, rank$ID[rank$Group == paste0('Low ', Input)]]
  low <- apply(low, 1, function(x) {
    sum(!is.na(x))
  }) / ncol(tmp)
  high <- tmp[, rank$ID[rank$Group == paste0('High ', Input)]]
  high <- apply(high, 1, function(x) {
    sum(!is.na(x))
  }) / ncol(tmp)
  pct1 <- data.frame(row.names = rownames(tmp),
                     Low = low,
                     High = high)
  
  ll  <- c()
  for (i in rownames(pct1)) {
    dd <- data.frame(x = c(pct1[i, 1] * ncol(tmp), (1 - pct1[i, 1]) * ncol(tmp)),
                     y = c(pct1[i, 2] * ncol(tmp), (1 - pct1[i, 2]) * ncol(tmp)))
    p <- fisher.test(dd)$p.value
    ll <-
      c(ll, ifelse(p < 0.0001, '****', ifelse(
        p < 0.001, '***', ifelse(p < 0.01, '**', ifelse(p < 0.05, '*', ''))
      )))
  }
  
  low <- tmp[, rank$ID[rank$Group == paste0('Low ', Input)]]
  low <-
    apply(low, 1, function(x) {
      sum(!is.na(x))
    }) / apply(tmp, 1, function(x) {
      sum(!is.na(x))
    })
  high <- tmp[, rank$ID[rank$Group == paste0('High ', Input)]]
  high <-
    apply(high, 1, function(x) {
      sum(!is.na(x))
    }) / apply(tmp, 1, function(x) {
      sum(!is.na(x))
    })
  pct2 <- data.frame(row.names = rownames(tmp),
                     Low = low,
                     High = high)
  
  right_anno <- anno_barplot(
    as.matrix(pct2),
    which = "row",
    border = F,
    show_annotation_name = F,
    gp = gpar(
      fill = c('#B0997F', '#ffc857'),
      border = NA,
      lty = "blank"
    ),
    bar_width = 0.6,
    width = unit(1.8, "cm"),
    height = unit(1, "cm")
  )
  right <- rowAnnotation(
    Percent = right_anno,
    annotation_name_side = "top",
    annotation_name_rot = 0,
    annotation_name_gp = gpar(fontsize = 11),
    ann = anno_text(ll)
  )
  
  return(
    Heatmap(
      as.matrix(tmp),
      na_col = 'WhiteSmoke',
      border = T,
      name = ' ',
      col = c(
        'Mut' = '#67AB9F',
        'Gain' = alpha('#ff477e', 0.8),
        'Loss' = alpha('#0096c7', 0.8)
      ),
      top_annotation = Top,
      right_annotation = right,
      column_split = rev(as.numeric(factor(rank$Group))),
      row_split = factor(cnadata, levels = c('Mutation', 'Gain', 'Loss')),
      cluster_rows = F,
      row_title = NULL,
      show_column_names = F,
      cluster_columns = F,
      column_title = NULL,
      show_row_names = T,
      row_names_side = 'left',
      row_names_gp = gpar(fontface = 'italic', fontsize = 10),
      # width = ncol(tmp) * unit(0.5, "mm"),
      # height = nrow(tmp) * unit(4, "mm"),
      show_heatmap_legend = T
    )
  )
}

# lzq_alter_landscape <- function(rank, Input, alterdata, cnadata) {
#   rank$Group <-
#     ifelse(rank[, 2] > median(rank[, 2]),
#            paste0('High ', Input),
#            paste0('Low ', Input))
#   
#   tmp <- alterdata[, rank$ID]
#   tmp[tmp == ''] <- NA
#   
#   Top = HeatmapAnnotation(
#     Group = anno_block(
#       gp = gpar(fill = c('#B0997F', '#ffc857')),
#       height = unit(5.5, 'mm'),
#       labels = c(paste0('Low ', Input), paste0('High ', Input)),
#       labels_gp = gpar(
#         cex = 0.85,
#         col = "white",
#         fontface = 'bold'
#       )
#     ),
#     border = T,
#     show_annotation_name = F
#   )
#   
#   low <- tmp[, rank$ID[rank$Group == paste0('Low ', Input)]]
#   low <- apply(low, 1, function(x) {
#     sum(!is.na(x))
#   }) / ncol(tmp)
#   high <- tmp[, rank$ID[rank$Group == paste0('High ', Input)]]
#   high <- apply(high, 1, function(x) {
#     sum(!is.na(x))
#   }) / ncol(tmp)
#   pct1 <- data.frame(row.names = rownames(tmp),
#                      Low = low,
#                      High = high)
#   
#   ll  <- c()
#   for (i in rownames(pct1)) {
#     dd <- data.frame(x = c(pct1[i, 1] * ncol(tmp), (1 - pct1[i, 1]) * ncol(tmp)),
#                      y = c(pct1[i, 2] * ncol(tmp), (1 - pct1[i, 2]) * ncol(tmp)))
#     p <- fisher.test(dd)$p.value
#     ll <-
#       c(ll, ifelse(p < 0.0001, '****', ifelse(
#         p < 0.001, '***', ifelse(p < 0.01, '**', ifelse(p < 0.05, '*', ''))
#       )))
#   }
#   
#   low <- tmp[, rank$ID[rank$Group == paste0('Low ', Input)]]
#   low <-
#     apply(low, 1, function(x) {
#       sum(!is.na(x))
#     }) / apply(tmp, 1, function(x) {
#       sum(!is.na(x))
#     })
#   high <- tmp[, rank$ID[rank$Group == paste0('High ', Input)]]
#   high <-
#     apply(high, 1, function(x) {
#       sum(!is.na(x))
#     }) / apply(tmp, 1, function(x) {
#       sum(!is.na(x))
#     })
#   pct2 <- data.frame(row.names = rownames(tmp),
#                      Low = low,
#                      High = high)
#   
#   right_anno <- anno_barplot(
#     as.matrix(pct2),
#     which = "row",
#     border = F,
#     show_annotation_name = F,
#     gp = gpar(
#       fill = c('#B0997F', '#ffc857'),
#       border = NA,
#       lty = "blank"
#     ),
#     bar_width = 0.6,
#     width = unit(1.8, "cm"),
#     height = unit(1, "cm")
#   )
#   right <- rowAnnotation(
#     Percent = right_anno,
#     annotation_name_side = "top",
#     annotation_name_rot = 0,
#     annotation_name_gp = gpar(fontsize = 11),
#     ann = anno_text(ll)
#   )
#   
#   return(
#     Heatmap(
#       as.matrix(tmp),
#       na_col = 'WhiteSmoke',
#       border = T,
#       name = ' ',
#       col = c(
#         'Mut' = '#67AB9F',
#         'Gain' = alpha('#ff477e', 0.8),
#         'Loss' = alpha('#0096c7', 0.8)
#       ),
#       top_annotation = Top,
#       right_annotation = right,
#       column_split = rank$Group,
#       row_split = factor(cnadata, levels = c('Mutation', 'Gain', 'Loss')),
#       cluster_rows = F,
#       row_title = NULL,
#       show_column_names = F,
#       cluster_columns = F,
#       column_title = NULL,
#       show_row_names = T,
#       row_names_side = 'left',
#       row_names_gp = gpar(fontface = 'italic', fontsize = 10),
#       # width = ncol(tmp) * unit(0.8, "mm"),
#       # height = nrow(tmp) * unit(4, "mm"),
#       show_heatmap_legend = T
#     )
#   )
# }



#### lzq_calculate_score ####
lzq_calculate_score <-
  function(datalist = select_cohorts,
           genelist = Input,
           method = 'ssgsea') {
    ll <- list(genelist)
    
    if (method == 'ssgsea') {
      scorelist <- lapply(datalist, function(x) {
        data.frame(score = t(gsva(as.matrix(x), ll, method == 'ssgsea')))
      })
    }
    if (method == 'gsva') {
      scorelist <- lapply(datalist, function(x) {
        data.frame(score = t(
          gsva(
            as.matrix(x),
            ll,
            method = 'gsva',
            kcdf = "Gaussian",
            abs.ranking = FALSE,
            min.sz = 1,
            max.sz = Inf,
            mx.diff = TRUE,
            verbose = TRUE
          )
        ))
      })
    }
    if (method == 'zscore') {
      scorelist <- lapply(datalist, function(x) {
        data.frame(score = t(gsva(as.matrix(x), ll, method = 'zscore')))
      })
    }
    if (method == 'plage') {
      scorelist <- lapply(datalist, function(x) {
        data.frame(score = t(gsva(as.matrix(x), ll, method = 'plage')))
      })
    }
    if (method == 'mean') {
      scorelist <- lapply(datalist, function(x) {
        data.frame(score = scale(colMeans(x[rownames(x) %in% genelist, ])))
      })
    }
    return(scorelist)
  }

#### lzq_COC_normalize ####
lzq_COC_normalize <- function(data1, data2, ABS_Cor_cutoff) {
  overlap_gene_ids <- intersect(rownames(data1), rownames(data2))
  data1 <- data1[overlap_gene_ids, ]
  data2 <- data2[overlap_gene_ids, ]
  data1 <-
    data1[apply(data1, 1, function(x) {
      sum(x == 0) < 0.8 * ncol(data1)
    }), ]
  data2 <-
    data2[apply(data2, 1, function(x) {
      sum(x == 0) < 0.8 * ncol(data2)
    }), ]
  overlap_gene_ids <- intersect(rownames(data1), rownames(data2))
  data1 <- data1[overlap_gene_ids, ]
  data2 <- data2[overlap_gene_ids, ]
  
  cor1 <- cor(t(data1), method = 'spearman') %>% as.data.frame()
  cor2 <- cor(t(data2), method = 'spearman') %>% as.data.frame()
  cor3 <-
    sapply(overlap_gene_ids, function(x) {
      cor(cor1[, x], cor2[, x], method = 'spearman') %>% as.numeric()
    })
  cor3 <- sort(cor3, decreasing = T)
  message(paste0('Cor range is ', paste(round(range(
    cor3
  ), 2), collapse = '~')))
  
  ID <- names(cor3)[cor3 > ABS_Cor_cutoff]
  if (length(ID) < 50) {
    ID <- names(cor3)[1:50]
  }
  data1 <- data1[ID, ]
  data2 <- data2[ID, ]
  return(list(data1 = data1, data2 = data2))
}

#### lzq_consensus_model ####
lzq_consensus_model <- function(
    select_survars,
    alldata,
    traindata,
    seed=1234,
    StepCox_direction='backward', ##包括三个选项：forward backward both
    RSF_nodesize=5,
    RSF_nsplit=10,
    RSF_splitrule="logrank", ##包括三个选项：logrank bs.gradient logrankscore
    Lasso_lamda_rule='lambda.min', #包括两个选项 lambda.min lambda.1se
    Ridge_lamda_rule='lambda.min', #包括两个选项 lambda.min lambda.1se
    Enet_alpha=0.5, ##可选择0.1-0.9
    Enet_lamda_rule='lambda.min', #包括两个选项 lambda.min lambda.1se
    GBM_nodesize=5,
    SVM_type= 'vanbelle1', ##包括四个选项：regression vanbelle1 vanbelle2 hybrid'
    SVM_diffmeth= 'makediff3', ## 包括三个选项： makediff1 makediff2 and makediff3
    SVM_optmeth= 'quadprog',   ##包括两个选项：quadprog or ipop
    SVM_kernel='add_kernel', ##包括四个选项：lin_kernel add_kernel rbf_kernel poly_kernel
    plsRcox_lambda_rule='lambda.min', #包括两个选项 lambda.min lambda.1se
    Coxboost_type='verweij', #包括两个选项 verweij naive
    SuperPC_ncomponents=1 ## Number of principal components to compute. Should be 1,2 or 3.
){
  library(randomForestSRC)
  library(glmnet)
  library(gbm)
  library(survivalsvm)
  library(plsRcox)
  library(superpc)
  library(CoxBoost)
  
  if(select_survars=='OS'){
    gsurv <-  as.formula(Surv(OS.time,OS)~.)
  }
  if(select_survars=='RFS'){
    gsurv <-  as.formula(Surv(RFS.time,RFS)~.)
  }
  if(select_survars=='DFS'){
    gsurv <-  as.formula(Surv(DFS.time,DFS)~.)
  }
  if(select_survars=='PFS'){
    gsurv <-  as.formula(Surv(PFS.time,PFS)~.)
  }
  if(select_survars=='DSS'){
    gsurv <-  as.formula(Surv(DSS.time,DSS)~.)
  }
  
  results <- list()
  
  fit <- tryCatch(step(coxph(gsurv,traindata),direction = StepCox_direction),error=function(e)NA)
  rs <- tryCatch(lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(scale(predict(fit,type = 'risk',newdata = x))))}),error=function(e)NA)
  results[['StepCox']] <- rs
  results <- results[!is.na(results)]
  
  fit <- rfsrc(formula = gsurv,
               data = traindata,
               ntree = 1000,
               nodesize = RSF_nodesize,
               nsplit = RSF_nsplit,
               splitrule = RSF_splitrule,
               importance = T,
               seed = seed)
  best <- which.min(fit$err.rate)
  fit <- rfsrc(formula = gsurv,
               data = traindata,
               ntree = best,
               nodesize = RSF_nodesize,
               nsplit = RSF_nsplit,
               splitrule = RSF_splitrule,
               importance = T,
               seed = seed)
  rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(scale(predict(fit,newdata = x)$predicted)))})
  results[['RSF']] <- rs
  
  
  set.seed(seed)
  fit = cv.glmnet(as.matrix(traindata[,-c(1,2)]), as.matrix(Surv(traindata[,1],traindata[,2])),family = "cox",alpha=1,nfolds = 10)
  lambda <- ifelse(Lasso_lamda_rule=='lambda.min',fit$lambda.min,fit$lambda.1se)
  rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(scale(predict(fit,type='link',newx=as.matrix(x[,-c(1,2)]),s=lambda))))})
  results[['Lasso']] <- rs
  
  set.seed(seed)
  fit = cv.glmnet(as.matrix(traindata[,-c(1,2)]), as.matrix(Surv(traindata[,1],traindata[,2])),family = "cox",alpha=0,nfolds = 10)
  lambda <- ifelse(Ridge_lamda_rule=='lambda.min',fit$lambda.min,fit$lambda.1se)
  rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(scale(predict(fit,type='link',newx=as.matrix(x[,-c(1,2)]),s=lambda))))})
  results[['Ridge']] <- rs
  
  set.seed(seed)
  fit = cv.glmnet(as.matrix(traindata[,-c(1,2)]), as.matrix(Surv(traindata[,1],traindata[,2])),family = "cox",alpha=Enet_alpha,nfolds = 10)
  lambda <- ifelse(Enet_lamda_rule=='lambda.min',fit$lambda.min,fit$lambda.1se)
  rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(scale(predict(fit,type='link',newx=as.matrix(x[,-c(1,2)]),s=lambda))))})
  results[['Enet']] <- rs
  
  set.seed(seed)
  fit <- gbm(formula = gsurv,
             data = traindata,
             distribution = 'coxph',
             n.trees = 1000,
             interaction.depth = 3,
             n.minobsinnode = GBM_nodesize,
             shrinkage = 0.001,
             cv.folds = 10,n.cores = 6)
  rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(scale(predict(fit,x,n.trees = which.min(fit$cv.error),type = 'link'))))})
  results[['GBM']] <- rs
  
  fit = survivalsvm(formula = gsurv, data = traindata, gamma.mu = 0.1,
                    type = SVM_type, diff.meth = SVM_diffmeth, 
                    opt.meth = SVM_optmeth, kernel = SVM_kernel,
                    sgf.sv = 5,sigf = 7, maxiter = 20, margin = 0.05, 
                    bound = 10, eig.tol = 1e-06,
                    conv.tol = 1e-07, posd.tol = 1e-08)
  rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(predict(fit,x)$predicted))})
  results[['SVM']] <- rs
  
  set.seed(seed)
  nfold <- ifelse(nrow(alldata[[1]])<=60,3,ifelse(nrow(alldata[[1]])<=100,5,10))
  cv.plsRcox.res=tryCatch(cv.plsRcox(list(x=traindata[,-c(1,2)],time=traindata[,1],status=traindata[,2]),
                                     nt=10,verbDFSe = FALSE,nfold = nfold),
                          error=function(e){
                            tryCatch(cv.plsRcox(list(x=traindata[,-c(1,2)],time=traindata[,1],status=traindata[,2]),
                                                nt=1,verbDFSe = FALSE,nfold = 5),
                                     error=function(e){tryCatch(cv.plsRcox(list(x=traindata[,-c(1,2)],time=traindata[,1],status=traindata[,2]),
                                                                           nt=1,verbDFSe = FALSE,nfold = 3),
                                                                error=function(e){cv.plsRcox(list(x=traindata[,-c(1,2)],time=traindata[,1],status=traindata[,2]),
                                                                                             nt=1,verbDFSe = FALSE,nfold = 2)})
                                     })})
  lambda <- ifelse(plsRcox_lambda_rule=='lambda.min',cv.plsRcox.res[[5]],cv.plsRcox.res[[6]])
  fit <- plsRcox(traindata[,-c(1,2)],time=traindata[,1],event=traindata[,2],nt=lambda)
  rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(scale(predict(fit,type="lp",newdata=x[,-c(1,2)]))))})
  results[['plsRcox']] <- rs
  
  set.seed(seed)
  pen <- optimCoxBoostPenalty(traindata[,1],traindata[,2],as.matrix(traindata[,-c(1,2)]),
                              trace=TRUE,start.penalty=1000,parallel = T)
  cv.res <- cv.CoxBoost(traindata[,1],traindata[,2],as.matrix(traindata[,-c(1,2)]),
                        maxstepno=1000,K=10,type=Coxboost_type,penalty=pen$penalty)
  fit <- CoxBoost(traindata[,1],traindata[,2],as.matrix(traindata[,-c(1,2)]),
                  stepno=cv.res$optimal.step,penalty=pen$penalty)
  rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(predict(fit,newdata=x[,-c(1,2)], newtime=x[,1], newstatus=x[,2], type="lp")))})
  results[['Coxboost']] <- rs
  
  data <- list(x=t(traindata[,-c(1,2)]),y=traindata[,1],censoring.status=traindata[,2],featurenames=colnames(traindata)[-c(1,2)])
  set.seed(seed)
  fit <- superpc.train(data = data,type = 'survival',s0.perc = 0.5) 
  set.seed(seed)
  cv.fit <- superpc.cv(fit,data,n.threshold = 20,
                       n.fold = 10,
                       n.components=SuperPC_ncomponents,
                       min.features=0,
                       max.features=nrow(data$x),
                       compute.fullcv= TRUE,
                       compute.preval=TRUE)
  rs <- tryCatch(lapply(alldata,function(w){
    test <- list(x=t(w[,-c(1,2)]),y=w[,1],censoring.status=w[,2],featurenames=colnames(w)[-c(1,2)])
    ff <- superpc.predict(fit,data,test,threshold = cv.fit$thresholds[which.max(cv.fit[["scor"]][1,])],n.components = SuperPC_ncomponents)
    rr <- as.numeric(ff$v.pred)
    rr2 <- cbind(w[,1:2],score=rr)
    return(rr2)}),error=function(e){NA})
  results[['SuperPC']] <- rs
  results <- results[!is.na(results)]
  
  return(results)
}



#### lzq_coxph ####
lzq_coxph <-
  function (formula,
            data,
            weights,
            subset,
            na.action,
            init,
            control,
            ties = c("efron", "breslow", "exact"),
            singular.ok = TRUE,
            robust,
            model = FALSE,
            x = FALSE,
            y = TRUE,
            tt,
            method = ties,
            id,
            cluster,
            istate,
            statedata,
            nocenter = c(-1, 0, 1),
            ...)
  {
    ties <- match.arg(ties)
    Call <- match.call()
    extraArgs <- list(...)
    if (length(extraArgs)) {
      controlargs <- names(formals(coxph.control))
      indx <- pmatch(names(extraArgs), controlargs, nomatch = 0L)
      if (any(indx == 0L))
        stop(gettextf("Argument %s not matched", names(extraArgs)[indx ==
                                                                    0L]), domain = NA)
    }
    if (missing(control))
      control <- coxph.control(...)
    if (missing(formula))
      stop("a formula argument is required")
    ss <- "cluster"
    if (is.list(formula))
      Terms <- if (missing(data))
        terms(formula[[1]], specials = ss)
    else
      terms(formula[[1]], specials = ss, data = data)
    else
      Terms <- if (missing(data))
        terms(formula, specials = ss)
    else
      terms(formula, specials = ss, data = data)
    tcl <- attr(Terms, "specials")$cluster
    if (length(tcl) > 1)
      stop("a formula cannot have multiple cluster terms")
    if (length(tcl) > 0) {
      factors <- attr(Terms, "factors")
      if (any(factors[tcl,] > 1))
        stop("cluster() cannot be in an interaction")
      if (attr(Terms, "response") == 0)
        stop("formula must have a Surv response")
      if (is.null(Call$cluster))
        Call$cluster <- attr(Terms, "variables")[[1 + tcl]][[2]]
      else
        warning("cluster appears both in a formula and as an argument, formula term ignored")
      Terms <- drop.special(Terms, tcl)
      formula <- Call$formula <- formula(Terms)
    }
    indx <- match(
      c(
        "formula",
        "data",
        "weights",
        "subset",
        "na.action",
        "cluster",
        "id",
        "istate"
      ),
      names(Call),
      nomatch = 0
    )
    if (indx[1] == 0)
      stop("A formula argument is required")
    tform <- Call[c(1, indx)]
    tform[[1L]] <- quote(stats::model.frame)
    if (is.list(formula)) {
      multiform <- TRUE
      dformula <- formula[[1]]
      if (missing(statedata))
        covlist <- parsecovar1(formula[-1])
      else {
        if (!inherits(statedata, "data.frame"))
          stop("statedata must be a data frame")
        if (is.null(statedata$state))
          stop("statedata data frame must contain a 'state' variable")
        covlist <- parsecovar1(formula[-1], names(statedata))
      }
      tlab <-
        unlist(lapply(covlist$rhs, function(x)
          attr(terms.formula(x$formula),
               "term.labels")))
      tlab <- c(attr(terms.formula(dformula), "term.labels"),
                tlab)
      newform <- reformulate(tlab, dformula[[2]])
      environment(newform) <- environment(dformula)
      formula <- newform
      tform$na.action <- na.pass
    }
    else {
      multiform <- FALSE
      covlist <- NULL
      dformula <- formula
    }
    special <- c("strata", "tt", "frailty", "ridge", "pspline")
    tform$formula <- if (missing(data))
      terms(formula, special)
    else
      terms(formula, special, data = data)
    if (!is.null(attr(tform$formula, "specials")$tt)) {
      coxenv <- new.env(parent = environment(formula))
      assign("tt", function(x)
        x, envir = coxenv)
      environment(tform$formula) <- coxenv
    }
    mf <- eval(tform, parent.frame())
    Terms <- terms(mf)
    n <- nrow(mf)
    Y <- model.response(mf)
    isSurv2 <- inherits(Y, "Surv2")
    if (isSurv2) {
      if (length(attr(mf, "na.action"))) {
        tform$na.action <- na.pass
        mf <- eval.parent(tform)
      }
      if (!is.null(attr(Terms, "specials")$cluster))
        stop("cluster() cannot appear in the model statement")
      new <- surv2data(mf)
      mf <- new$mf
      istate <- new$istate
      id <- new$id
      Y <- new$y
      n <- nrow(mf)
    }
    else {
      if (!is.Surv(Y))
        stop("Response must be a survival object")
      id <- model.extract(mf, "id")
      istate <- model.extract(mf, "istate")
    }
    if (n == 0)
      stop("No (non-missing) observations")
    type <- attr(Y, "type")
    multi <- FALSE
    if (type == "mright" || type == "mcounting")
      multi <- TRUE
    else if (type != "right" && type != "counting")
      stop(paste("Cox model doesn't support \"", type, "\" survival data",
                 sep = ""))
    data.n <- nrow(Y)
    if (!multi && multiform)
      stop("formula is a list but the response is not multi-state")
    if (multi && length(attr(Terms, "specials")$frailty) > 0)
      stop("multi-state models do not currently support frailty terms")
    if (multi && length(attr(Terms, "specials")$pspline) > 0)
      stop("multi-state models do not currently support pspline terms")
    if (multi && length(attr(Terms, "specials")$ridge) > 0)
      stop("multi-state models do not currently support ridge penalties")
    if (control$timefix)
      Y <- aeqSurv(Y)
    if (length(attr(Terms, "variables")) > 2) {
      ytemp <- terms.inner(formula[1:2])
      suppressWarnings(z <- as.numeric(ytemp))
      ytemp <- ytemp[is.na(z)]
      xtemp <- terms.inner(formula[-2])
      if (any(!is.na(match(xtemp, ytemp))))
        warning("a variable appears on both the left and right sides of the formula")
    }
    strats <- attr(Terms, "specials")$strata
    hasinteractions <- FALSE
    dropterms <- NULL
    if (length(strats)) {
      stemp <- untangle.specials(Terms, "strata", 1)
      if (length(stemp$vars) == 1)
        strata.keep <- mf[[stemp$vars]]
      else
        strata.keep <- strata(mf[, stemp$vars], shortlabel = TRUE)
      istrat <- as.integer(strata.keep)
      for (i in stemp$vars) {
        if (any(attr(Terms, "order")[attr(Terms, "factors")[i,] > 0] > 1))
          hasinteractions <- TRUE
      }
      if (!hasinteractions)
        dropterms <- stemp$terms
    }
    else
      istrat <- NULL
    if (hasinteractions && multi)
      stop("multi-state coxph does not support strata*covariate interactions")
    timetrans <- attr(Terms, "specials")$tt
    if (missing(tt))
      tt <- NULL
    if (length(timetrans)) {
      if (multi || isSurv2)
        stop("the tt() transform is not implemented for multi-state or Surv2 models")
      timetrans <- untangle.specials(Terms, "tt")
      ntrans <- length(timetrans$terms)
      if (is.null(tt)) {
        tt <- function(x, time, riskset, weights) {
          obrien <- function(x) {
            r <- rank(x)
            (r - 0.5) / (0.5 + length(r) - r)
          }
          unlist(tapply(x, riskset, obrien))
        }
      }
      if (is.function(tt))
        tt <- list(tt)
      if (is.list(tt)) {
        if (any(!sapply(tt, is.function)))
          stop("The tt argument must contain function or list of functions")
        if (length(tt) != ntrans) {
          if (length(tt) == 1) {
            temp <- vector("list", ntrans)
            for (i in 1:ntrans)
              temp[[i]] <- tt[[1]]
            tt <- temp
          }
          else
            stop("Wrong length for tt argument")
        }
      }
      else
        stop("The tt argument must contain a function or list of functions")
      if (ncol(Y) == 2) {
        if (length(strats) == 0) {
          sorted <- order(-Y[, 1], Y[, 2])
          newstrat <- rep.int(0L, nrow(Y))
          newstrat[1] <- 1L
        }
        else {
          sorted <- order(istrat,-Y[, 1], Y[, 2])
          newstrat <- as.integer(c(1, 1 * (diff(istrat[sorted]) !=
                                             0)))
        }
        if (storage.mode(Y) != "double")
          storage.mode(Y) <- "double"
        counts <- .Call(Ccoxcount1, Y[sorted,], as.integer(newstrat))
        tindex <- sorted[counts$index]
      }
      else {
        if (length(strats) == 0) {
          sort.end <- order(-Y[, 2], Y[, 3])
          sort.start <- order(-Y[, 1])
          newstrat <- c(1L, rep(0, nrow(Y) - 1))
        }
        else {
          sort.end <- order(istrat,-Y[, 2], Y[, 3])
          sort.start <- order(istrat,-Y[, 1])
          newstrat <- c(1L, as.integer(diff(istrat[sort.end]) !=
                                         0))
        }
        if (storage.mode(Y) != "double")
          storage.mode(Y) <- "double"
        counts <- .Call(
          Ccoxcount2,
          Y,
          as.integer(sort.start -
                       1L),
          as.integer(sort.end - 1L),
          as.integer(newstrat)
        )
        tindex <- counts$index
      }
      Y <- Surv(rep(counts$time, counts$nrisk), counts$status)
      type <- "right"
      mf <- mf[tindex,]
      istrat <- rep(1:length(counts$nrisk), counts$nrisk)
      weights <- model.weights(mf)
      if (!is.null(weights) && any(!is.finite(weights)))
        stop("weights must be finite")
      tcall <- attr(Terms, "variables")[timetrans$terms +
                                          2]
      pvars <- attr(Terms, "predvars")
      pmethod <-
        sub("makepredictcall.", "", as.vector(methods("makepredictcall")))
      for (i in 1:ntrans) {
        newtt <- (tt[[i]])(mf[[timetrans$var[i]]], Y[, 1],
                           istrat, weights)
        mf[[timetrans$var[i]]] <- newtt
        nclass <- class(newtt)
        if (any(nclass %in% pmethod)) {
          dummy <- as.call(list(as.name(class(newtt)[1]),
                                tcall[[i]][[2]]))
          ptemp <- makepredictcall(newtt, dummy)
          pvars[[timetrans$terms[i] + 2]] <- ptemp
        }
      }
      attr(Terms, "predvars") <- pvars
    }
    xlevels <- .getXlevels(Terms, mf)
    cluster <- model.extract(mf, "cluster")
    weights <- model.weights(mf)
    has.cluster <- !(missing(cluster) || length(cluster) ==
                       0)
    has.id <- !(missing(id) || length(id) == 0)
    has.rwt <- (!is.null(weights) && any(weights != floor(weights)))
    has.robust <- (!missing(robust) && !is.null(robust))
    if (has.id)
      id <- as.factor(id)
    if (missing(robust) || is.null(robust)) {
      if (has.cluster ||
          has.rwt || (has.id && (multi || anyDuplicated(id[Y[,
                                                             ncol(Y)] == 1]))))
        robust <- TRUE
      else
        robust <- FALSE
    }
    if (!is.logical(robust))
      stop("robust must be TRUE/FALSE")
    if (has.cluster) {
      if (!robust) {
        warning("cluster specified with robust=FALSE, cluster ignored")
        ncluster <- 0
        clname <- NULL
      }
      else {
        if (is.factor(cluster)) {
          clname <- levels(cluster)
          cluster <- as.integer(cluster)
        }
        else {
          clname <- sort(unique(cluster))
          cluster <- match(cluster, clname)
        }
        ncluster <- length(clname)
      }
    }
    else {
      if (robust && has.id) {
        clname <- levels(id)
        cluster <- as.integer(id)
        ncluster <- length(clname)
      }
      else {
        ncluster <- 0
      }
    }
    if (robust && is.null(cluster)) {
      if (ncol(Y) == 2 || !has.robust)
        cluster <- seq.int(1, nrow(mf))
      else
        stop("one of cluster or id is needed")
    }
    contrast.arg <- NULL
    attr(Terms, "intercept") <- 1
    if (multi) {
      if (length(id) == 0)
        stop("an id statement is required for multi-state models")
      mcheck <- survcheck2(Y, id, istate)
      if (mcheck$flag["overlap"] > 0)
        stop("data set has overlapping intervals for one or more subjects")
      transitions <- mcheck$transitions
      istate <- mcheck$istate
      states <- mcheck$states
      if (missing(statedata))
        covlist2 <- parsecovar2(covlist, NULL, dformula = dformula,
                                Terms, transitions, states)
      else
        covlist2 <- parsecovar2(covlist, statedata, dformula = dformula,
                                Terms, transitions, states)
      tmap <- covlist2$tmap
      if (!is.null(covlist)) {
        good.tran <- bad.tran <- rep(FALSE, nrow(Y))
        termname <- rownames(attr(Terms, "factors"))
        trow <- (!is.na(match(rownames(tmap), termname)))
        termiss <- matrix(0L, nrow(mf), ncol(mf))
        for (i in 1:ncol(mf)) {
          xx <- is.na(mf[[i]])
          if (is.matrix(xx))
            termiss[, i] <- apply(xx, 1, any)
          else
            termiss[, i] <- xx
        }
        for (i in levels(istate)) {
          rindex <- which(istate == i)
          j <- which(covlist2$mapid[, 1] == match(i, states))
          for (jcol in j) {
            k <- which(trow & tmap[, jcol] > 0)
            bad.tran[rindex] <-
              (bad.tran[rindex] | apply(termiss[rindex,
                                                k, drop = FALSE], 1, any))
            good.tran[rindex] <- (good.tran[rindex] |
                                    apply(!termiss[rindex, k, drop = FALSE],
                                          1, all))
          }
        }
        n.partially.used <- sum(good.tran & bad.tran & !is.na(Y))
        omit <- (!good.tran & bad.tran) | is.na(Y)
        if (all(omit))
          stop("all observations deleted due to missing values")
        temp <- setNames(seq(omit)[omit], attr(mf, "row.names")[omit])
        attr(temp, "class") <- "omit"
        mf <- mf[!omit, , drop = FALSE]
        attr(mf, "na.action") <- temp
        Y <- Y[!omit]
        id <- id[!omit]
        if (length(istate))
          istate <- istate[!omit]
      }
    }
    if (length(dropterms)) {
      Terms2 <- Terms[-dropterms]
      X <- model.matrix(Terms2, mf, constrasts.arg = contrast.arg)
      temp <- attr(X, "assign")
      shift <- sort(dropterms)
      for (i in seq(along.with = shift))
        temp <- temp + 1 *
        (shift[i] <= temp)
      attr(X, "assign") <- temp
    }
    else
      X <- model.matrix(Terms, mf, contrasts.arg = contrast.arg)
    Xatt <- attributes(X)
    if (hasinteractions)
      adrop <- c(0, untangle.specials(Terms, "strata")$terms)
    else
      adrop <- 0
    xdrop <- Xatt$assign %in% adrop
    X <- X[,!xdrop, drop = FALSE]
    attr(X, "assign") <- Xatt$assign[!xdrop]
    attr(X, "contrasts") <- Xatt$contrasts
    offset <- model.offset(mf)
    if (is.null(offset) | all(offset == 0))
      offset <- rep(0, nrow(mf))
    else if (any(!is.finite(offset) | !is.finite(exp(offset))))
      stop("offsets must lead to a finite risk score")
    weights <- model.weights(mf)
    if (!is.null(weights) && any(!is.finite(weights)))
      stop("weights must be finite")
    assign <- attrassign(X, Terms)
    contr.save <- attr(X, "contrasts")
    if (sum(Y[, ncol(Y)]) == 0) {
      ncoef <- ncol(X)
      ctemp <- rep(NA, ncoef)
      names(ctemp) <- colnames(X)
      concordance = c(
        concordant = 0,
        discordant = 0,
        tied.x = 0,
        tied.y = 0,
        tied.xy = 0,
        concordance = NA,
        std = NA,
        timefix = FALSE
      )
      rval <- list(
        coefficients = ctemp,
        var = matrix(0, ncoef,
                     ncoef),
        loglik = c(0, 0),
        score = 0,
        iter = 0,
        linear.predictors = offset,
        residuals = rep(0, data.n),
        means = colMeans(X),
        method = method,
        n = data.n,
        nevent = 0,
        terms = Terms,
        assign = assign,
        concordance = concordance,
        wald.test = 0,
        y = Y,
        call = Call
      )
      class(rval) <- "coxph"
      return(rval)
    }
    if (multi) {
      if (length(strats) > 0) {
        stratum_map <- tmap[c(1L, strats),]
        stratum_map[-1,] <- ifelse(stratum_map[-1,] >
                                     0, 1L, 0L)
        if (nrow(stratum_map) > 2) {
          temp <- stratum_map[-1,]
          if (!all(apply(temp, 2, function(x)
            all(x ==
                0) ||
            all(x == 1)))) {
            strata.keep <- mf[, strats]
            istrat <- sapply(strata.keep, as.numeric)
          }
        }
      }
      else
        stratum_map <- tmap[1, , drop = FALSE]
      cmap <- parsecovar3(tmap, colnames(X), attr(X, "assign"),
                          covlist2$phbaseline)
      xstack <- stacker(
        cmap,
        stratum_map,
        as.integer(istate),
        X,
        Y,
        strata = istrat,
        states = states
      )
      rkeep <- unique(xstack$rindex)
      transitions <-
        survcheck2(Y[rkeep,], id[rkeep], istate[rkeep])$transitions
      X <- xstack$X
      Y <- xstack$Y
      istrat <- xstack$strata
      if (length(offset))
        offset <- offset[xstack$rindex]
      if (length(weights))
        weights <- weights[xstack$rindex]
      if (length(cluster))
        cluster <- cluster[xstack$rindex]
      t2 <- tmap[-c(1, strats), , drop = FALSE]
      r2 <- row(t2)[!duplicated(as.vector(t2)) & t2 != 0]
      c2 <- col(t2)[!duplicated(as.vector(t2)) & t2 != 0]
      a2 <- lapply(seq(along.with = r2), function(i) {
        cmap[assign[[r2[i]]], c2[i]]
      })
      tab <- table(r2)
      count <- tab[r2]
      names(a2) <-
        ifelse(count == 1,
               row.names(t2)[r2],
               paste(row.names(t2)[r2],
                     colnames(cmap)[c2], sep = "_"))
      assign <- a2
    }
    if (!all(is.finite(X)))
      stop("data contains an infinite predictor")
    if (missing(init))
      init <- NULL
    else {
      if (length(init) != ncol(X))
        stop("wrong length for init argument")
      temp <- X %*% init - sum(colMeans(X) * init) + offset
      if (any(exp(temp) > .Machine$double.xmax) || all(exp(temp) ==
                                                       0))
        stop("initial values lead to overflow or underflow of the exp function")
    }
    pterms <- sapply(mf, inherits, "coxph.penalty")
    if (any(pterms)) {
      pattr <- lapply(mf[pterms], attributes)
      pname <- names(pterms)[pterms]
      ord <- attr(Terms, "order")[match(pname, attr(Terms,
                                                    "term.labels"))]
      if (any(ord > 1))
        stop("Penalty terms cannot be in an interaction")
      pcols <- assign[match(pname, names(assign))]
      fit <- coxpenal.fit(
        X,
        Y,
        istrat,
        offset,
        init = init,
        control,
        weights = weights,
        method = method,
        row.names(mf),
        pcols,
        pattr,
        assign,
        nocenter = nocenter
      )
    }
    else {
      rname <- row.names(mf)
      if (multi)
        rname <- rname[xstack$rindex]
      if (method == "breslow" || method == "efron") {
        if (grepl("right", type))
          fit <- coxph.fit(
            X,
            Y,
            istrat,
            offset,
            init,
            control,
            weights = weights,
            method = method,
            rname,
            nocenter = nocenter
          )
        else
          fit <- agreg.fit(
            X,
            Y,
            istrat,
            offset,
            init,
            control,
            weights = weights,
            method = method,
            rname,
            nocenter = nocenter
          )
      }
      else if (method == "exact") {
        if (type == "right")
          fit <- coxexact.fit(
            X,
            Y,
            istrat,
            offset,
            init,
            control,
            weights = weights,
            method = method,
            rname,
            nocenter = nocenter
          )
        else
          fit <- agexact.fit(
            X,
            Y,
            istrat,
            offset,
            init,
            control,
            weights = weights,
            method = method,
            rname,
            nocenter = nocenter
          )
      }
      else
        stop(paste("Unknown method", method))
    }
    if (is.character(fit)) {
      fit <- list(fail = fit)
      class(fit) <- "coxph"
    }
    else {
      if (!is.null(fit$coefficients) && any(is.na(fit$coefficients))) {
        vars <- (1:length(fit$coefficients))[is.na(fit$coefficients)]
        msg <- paste("X matrix deemed to be singular; variable",
                     paste(vars, collapse = " "))
        if (!singular.ok)
          stop(msg)
      }
      fit$n <- data.n
      fit$nevent <- sum(Y[, ncol(Y)])
      fit$terms <- Terms
      fit$assign <- assign
      class(fit) <- fit$class
      fit$class <- NULL
      if (robust &&
          !is.null(fit$coefficients) && !all(is.na(fit$coefficients))) {
        fit$naive.var <- fit$var
        fit2 <- c(fit, list(
          x = X,
          y = Y,
          weights = weights
        ))
        if (length(istrat))
          fit2$strata <- istrat
        if (length(cluster)) {
          temp <- residuals.coxph(fit2,
                                  type = "dfbeta",
                                  collapse = cluster,
                                  weighted = TRUE)
          if (is.null(init))
            fit2$linear.predictors <- 0 * fit$linear.predictors
          else
            fit2$linear.predictors <- c(X %*% init)
          temp0 <- residuals.coxph(fit2,
                                   type = "score",
                                   collapse = cluster,
                                   weighted = TRUE)
        }
        else {
          temp <- residuals.coxph(fit2, type = "dfbeta",
                                  weighted = TRUE)
          fit2$linear.predictors <- 0 * fit$linear.predictors
          temp0 <- residuals.coxph(fit2, type = "score",
                                   weighted = TRUE)
        }
        fit$var <- t(temp) %*% temp
        u <- apply(as.matrix(temp0), 2, sum)
        fit$rscore <- coxph.wtest(t(temp0) %*% temp0, u,
                                  control$toler.chol)$test
      }
      if (length(fit$coefficients) && is.null(fit$wald.test)) {
        nabeta <- !is.na(fit$coefficients)
        if (is.null(init))
          temp <- fit$coefficients[nabeta]
        else
          temp <-
            (fit$coefficients - init[1:length(fit$coefficients)])[nabeta]
        fit$wald.test <- tryCatch(
          coxph.wtest(fit$var[nabeta, nabeta],
                      temp, control$toler.chol)$test,
          error = function(e)
            NA)
      }
      if (length(cluster))
        temp <- concordancefit(
          Y,
          fit$linear.predictors,
          istrat,
          weights,
          cluster = cluster,
          reverse = TRUE,
          timefix = FALSE
        )
      else
        temp <- concordancefit(
          Y,
          fit$linear.predictors,
          istrat,
          weights,
          reverse = TRUE,
          timefix = FALSE
        )
      if (is.matrix(temp$count))
        fit$concordance <-
        c(
          colSums(temp$count),
          concordance = temp$concordance,
          std = sqrt(temp$var)
        )
      else
        fit$concordance <- c(temp$count,
                             concordance = temp$concordance,
                             std = sqrt(temp$var))
      na.action <- attr(mf, "na.action")
      if (length(na.action))
        fit$na.action <- na.action
      if (model) {
        if (length(timetrans)) {
          stop("'model=TRUE' not supported for models with tt terms")
        }
        fit$model <- mf
      }
      if (x) {
        fit$x <- X
        if (length(timetrans))
          fit$strata <- istrat
        else if (length(strats))
          fit$strata <- strata.keep
      }
      if (y)
        fit$y <- Y
      fit$timefix <- control$timefix
    }
    if (!is.null(weights) && any(weights != 1))
      fit$weights <- weights
    if (multi) {
      fit$transitions <- transitions
      fit$states <- states
      fit$cmap <- cmap
      fit$stratum_map <- stratum_map
      fit$resid <- rowsum(fit$resid, xstack$rindex)
      single <- apply(cmap, 1, function(x)
        all(x %in% c(0,
                     max(x))))
      cindx <- col(cmap)[match(1:length(fit$coefficients),
                               cmap)]
      rindx <- row(cmap)[match(1:length(fit$coefficients),
                               cmap)]
      suffix <-
        ifelse(single[rindx], "", paste0("_", colnames(cmap)[cindx]))
      names(fit$coefficients) <- paste0(names(fit$coefficients),
                                        suffix)
      if (x)
        fit$strata <- istrat
      class(fit) <- c("coxphms", class(fit))
    }
    names(fit$means) <- names(fit$coefficients)
    fit$formula <- formula(Terms)
    if (length(xlevels) > 0)
      fit$xlevels <- xlevels
    fit$contrasts <- contr.save
    if (any(offset != 0))
      fit$offset <- offset
    fit$call <- Call
    fit
  }
function (formula,
          data,
          weights,
          subset,
          na.action,
          init,
          control,
          ties = c("efron", "breslow", "exact"),
          singular.ok = TRUE,
          robust,
          model = FALSE,
          x = FALSE,
          y = TRUE,
          tt,
          method = ties,
          id,
          cluster,
          istate,
          statedata,
          nocenter = c(-1, 0, 1),
          ...)
{
  ties <- match.arg(ties)
  Call <- match.call()
  extraArgs <- list(...)
  if (length(extraArgs)) {
    controlargs <- names(formals(coxph.control))
    indx <- pmatch(names(extraArgs), controlargs, nomatch = 0L)
    if (any(indx == 0L))
      stop(gettextf("Argument %s not matched", names(extraArgs)[indx ==
                                                                  0L]), domain = NA)
  }
  if (missing(control))
    control <- coxph.control(...)
  if (missing(formula))
    stop("a formula argument is required")
  ss <- "cluster"
  if (is.list(formula))
    Terms <- if (missing(data))
      terms(formula[[1]], specials = ss)
  else
    terms(formula[[1]], specials = ss, data = data)
  else
    Terms <- if (missing(data))
      terms(formula, specials = ss)
  else
    terms(formula, specials = ss, data = data)
  tcl <- attr(Terms, "specials")$cluster
  if (length(tcl) > 1)
    stop("a formula cannot have multiple cluster terms")
  if (length(tcl) > 0) {
    factors <- attr(Terms, "factors")
    if (any(factors[tcl,] > 1))
      stop("cluster() cannot be in an interaction")
    if (attr(Terms, "response") == 0)
      stop("formula must have a Surv response")
    if (is.null(Call$cluster))
      Call$cluster <- attr(Terms, "variables")[[1 + tcl]][[2]]
    else
      warning("cluster appears both in a formula and as an argument, formula term ignored")
    Terms <- drop.special(Terms, tcl)
    formula <- Call$formula <- formula(Terms)
  }
  indx <- match(
    c(
      "formula",
      "data",
      "weights",
      "subset",
      "na.action",
      "cluster",
      "id",
      "istate"
    ),
    names(Call),
    nomatch = 0
  )
  if (indx[1] == 0)
    stop("A formula argument is required")
  tform <- Call[c(1, indx)]
  tform[[1L]] <- quote(stats::model.frame)
  if (is.list(formula)) {
    multiform <- TRUE
    dformula <- formula[[1]]
    if (missing(statedata))
      covlist <- parsecovar1(formula[-1])
    else {
      if (!inherits(statedata, "data.frame"))
        stop("statedata must be a data frame")
      if (is.null(statedata$state))
        stop("statedata data frame must contain a 'state' variable")
      covlist <- parsecovar1(formula[-1], names(statedata))
    }
    tlab <-
      unlist(lapply(covlist$rhs, function(x)
        attr(terms.formula(x$formula),
             "term.labels")))
    tlab <- c(attr(terms.formula(dformula), "term.labels"),
              tlab)
    newform <- reformulate(tlab, dformula[[2]])
    environment(newform) <- environment(dformula)
    formula <- newform
    tform$na.action <- na.pass
  }
  else {
    multiform <- FALSE
    covlist <- NULL
    dformula <- formula
  }
  special <- c("strata", "tt", "frailty", "ridge", "pspline")
  tform$formula <- if (missing(data))
    terms(formula, special)
  else
    terms(formula, special, data = data)
  if (!is.null(attr(tform$formula, "specials")$tt)) {
    coxenv <- new.env(parent = environment(formula))
    assign("tt", function(x)
      x, envir = coxenv)
    environment(tform$formula) <- coxenv
  }
  mf <- eval(tform, parent.frame())
  Terms <- terms(mf)
  n <- nrow(mf)
  Y <- model.response(mf)
  isSurv2 <- inherits(Y, "Surv2")
  if (isSurv2) {
    if (length(attr(mf, "na.action"))) {
      tform$na.action <- na.pass
      mf <- eval.parent(tform)
    }
    if (!is.null(attr(Terms, "specials")$cluster))
      stop("cluster() cannot appear in the model statement")
    new <- surv2data(mf)
    mf <- new$mf
    istate <- new$istate
    id <- new$id
    Y <- new$y
    n <- nrow(mf)
  }
  else {
    if (!is.Surv(Y))
      stop("Response must be a survival object")
    id <- model.extract(mf, "id")
    istate <- model.extract(mf, "istate")
  }
  if (n == 0)
    stop("No (non-missing) observations")
  type <- attr(Y, "type")
  multi <- FALSE
  if (type == "mright" || type == "mcounting")
    multi <- TRUE
  else if (type != "right" && type != "counting")
    stop(paste("Cox model doesn't support \"", type, "\" survival data",
               sep = ""))
  data.n <- nrow(Y)
  if (!multi && multiform)
    stop("formula is a list but the response is not multi-state")
  if (multi && length(attr(Terms, "specials")$frailty) > 0)
    stop("multi-state models do not currently support frailty terms")
  if (multi && length(attr(Terms, "specials")$pspline) > 0)
    stop("multi-state models do not currently support pspline terms")
  if (multi && length(attr(Terms, "specials")$ridge) > 0)
    stop("multi-state models do not currently support ridge penalties")
  if (control$timefix)
    Y <- aeqSurv(Y)
  if (length(attr(Terms, "variables")) > 2) {
    ytemp <- terms.inner(formula[1:2])
    suppressWarnings(z <- as.numeric(ytemp))
    ytemp <- ytemp[is.na(z)]
    xtemp <- terms.inner(formula[-2])
    if (any(!is.na(match(xtemp, ytemp))))
      warning("a variable appears on both the left and right sides of the formula")
  }
  strats <- attr(Terms, "specials")$strata
  hasinteractions <- FALSE
  dropterms <- NULL
  if (length(strats)) {
    stemp <- untangle.specials(Terms, "strata", 1)
    if (length(stemp$vars) == 1)
      strata.keep <- mf[[stemp$vars]]
    else
      strata.keep <- strata(mf[, stemp$vars], shortlabel = TRUE)
    istrat <- as.integer(strata.keep)
    for (i in stemp$vars) {
      if (any(attr(Terms, "order")[attr(Terms, "factors")[i,] > 0] > 1))
        hasinteractions <- TRUE
    }
    if (!hasinteractions)
      dropterms <- stemp$terms
  }
  else
    istrat <- NULL
  if (hasinteractions && multi)
    stop("multi-state coxph does not support strata*covariate interactions")
  timetrans <- attr(Terms, "specials")$tt
  if (missing(tt))
    tt <- NULL
  if (length(timetrans)) {
    if (multi || isSurv2)
      stop("the tt() transform is not implemented for multi-state or Surv2 models")
    timetrans <- untangle.specials(Terms, "tt")
    ntrans <- length(timetrans$terms)
    if (is.null(tt)) {
      tt <- function(x, time, riskset, weights) {
        obrien <- function(x) {
          r <- rank(x)
          (r - 0.5) / (0.5 + length(r) - r)
        }
        unlist(tapply(x, riskset, obrien))
      }
    }
    if (is.function(tt))
      tt <- list(tt)
    if (is.list(tt)) {
      if (any(!sapply(tt, is.function)))
        stop("The tt argument must contain function or list of functions")
      if (length(tt) != ntrans) {
        if (length(tt) == 1) {
          temp <- vector("list", ntrans)
          for (i in 1:ntrans)
            temp[[i]] <- tt[[1]]
          tt <- temp
        }
        else
          stop("Wrong length for tt argument")
      }
    }
    else
      stop("The tt argument must contain a function or list of functions")
    if (ncol(Y) == 2) {
      if (length(strats) == 0) {
        sorted <- order(-Y[, 1], Y[, 2])
        newstrat <- rep.int(0L, nrow(Y))
        newstrat[1] <- 1L
      }
      else {
        sorted <- order(istrat,-Y[, 1], Y[, 2])
        newstrat <- as.integer(c(1, 1 * (diff(istrat[sorted]) !=
                                           0)))
      }
      if (storage.mode(Y) != "double")
        storage.mode(Y) <- "double"
      counts <- .Call(Ccoxcount1, Y[sorted,], as.integer(newstrat))
      tindex <- sorted[counts$index]
    }
    else {
      if (length(strats) == 0) {
        sort.end <- order(-Y[, 2], Y[, 3])
        sort.start <- order(-Y[, 1])
        newstrat <- c(1L, rep(0, nrow(Y) - 1))
      }
      else {
        sort.end <- order(istrat,-Y[, 2], Y[, 3])
        sort.start <- order(istrat,-Y[, 1])
        newstrat <- c(1L, as.integer(diff(istrat[sort.end]) !=
                                       0))
      }
      if (storage.mode(Y) != "double")
        storage.mode(Y) <- "double"
      counts <- .Call(
        Ccoxcount2,
        Y,
        as.integer(sort.start -
                     1L),
        as.integer(sort.end - 1L),
        as.integer(newstrat)
      )
      tindex <- counts$index
    }
    Y <- Surv(rep(counts$time, counts$nrisk), counts$status)
    type <- "right"
    mf <- mf[tindex,]
    istrat <- rep(1:length(counts$nrisk), counts$nrisk)
    weights <- model.weights(mf)
    if (!is.null(weights) && any(!is.finite(weights)))
      stop("weights must be finite")
    tcall <- attr(Terms, "variables")[timetrans$terms +
                                        2]
    pvars <- attr(Terms, "predvars")
    pmethod <-
      sub("makepredictcall.", "", as.vector(methods("makepredictcall")))
    for (i in 1:ntrans) {
      newtt <- (tt[[i]])(mf[[timetrans$var[i]]], Y[, 1],
                         istrat, weights)
      mf[[timetrans$var[i]]] <- newtt
      nclass <- class(newtt)
      if (any(nclass %in% pmethod)) {
        dummy <- as.call(list(as.name(class(newtt)[1]),
                              tcall[[i]][[2]]))
        ptemp <- makepredictcall(newtt, dummy)
        pvars[[timetrans$terms[i] + 2]] <- ptemp
      }
    }
    attr(Terms, "predvars") <- pvars
  }
  xlevels <- .getXlevels(Terms, mf)
  cluster <- model.extract(mf, "cluster")
  weights <- model.weights(mf)
  has.cluster <- !(missing(cluster) || length(cluster) ==
                     0)
  has.id <- !(missing(id) || length(id) == 0)
  has.rwt <- (!is.null(weights) && any(weights != floor(weights)))
  has.robust <- (!missing(robust) && !is.null(robust))
  if (has.id)
    id <- as.factor(id)
  if (missing(robust) || is.null(robust)) {
    if (has.cluster ||
        has.rwt || (has.id && (multi || anyDuplicated(id[Y[,
                                                           ncol(Y)] == 1]))))
      robust <- TRUE
    else
      robust <- FALSE
  }
  if (!is.logical(robust))
    stop("robust must be TRUE/FALSE")
  if (has.cluster) {
    if (!robust) {
      warning("cluster specified with robust=FALSE, cluster ignored")
      ncluster <- 0
      clname <- NULL
    }
    else {
      if (is.factor(cluster)) {
        clname <- levels(cluster)
        cluster <- as.integer(cluster)
      }
      else {
        clname <- sort(unique(cluster))
        cluster <- match(cluster, clname)
      }
      ncluster <- length(clname)
    }
  }
  else {
    if (robust && has.id) {
      clname <- levels(id)
      cluster <- as.integer(id)
      ncluster <- length(clname)
    }
    else {
      ncluster <- 0
    }
  }
  if (robust && is.null(cluster)) {
    if (ncol(Y) == 2 || !has.robust)
      cluster <- seq.int(1, nrow(mf))
    else
      stop("one of cluster or id is needed")
  }
  contrast.arg <- NULL
  attr(Terms, "intercept") <- 1
  if (multi) {
    if (length(id) == 0)
      stop("an id statement is required for multi-state models")
    mcheck <- survcheck2(Y, id, istate)
    if (mcheck$flag["overlap"] > 0)
      stop("data set has overlapping intervals for one or more subjects")
    transitions <- mcheck$transitions
    istate <- mcheck$istate
    states <- mcheck$states
    if (missing(statedata))
      covlist2 <- parsecovar2(covlist, NULL, dformula = dformula,
                              Terms, transitions, states)
    else
      covlist2 <- parsecovar2(covlist, statedata, dformula = dformula,
                              Terms, transitions, states)
    tmap <- covlist2$tmap
    if (!is.null(covlist)) {
      good.tran <- bad.tran <- rep(FALSE, nrow(Y))
      termname <- rownames(attr(Terms, "factors"))
      trow <- (!is.na(match(rownames(tmap), termname)))
      termiss <- matrix(0L, nrow(mf), ncol(mf))
      for (i in 1:ncol(mf)) {
        xx <- is.na(mf[[i]])
        if (is.matrix(xx))
          termiss[, i] <- apply(xx, 1, any)
        else
          termiss[, i] <- xx
      }
      for (i in levels(istate)) {
        rindex <- which(istate == i)
        j <- which(covlist2$mapid[, 1] == match(i, states))
        for (jcol in j) {
          k <- which(trow & tmap[, jcol] > 0)
          bad.tran[rindex] <-
            (bad.tran[rindex] | apply(termiss[rindex,
                                              k, drop = FALSE], 1, any))
          good.tran[rindex] <- (good.tran[rindex] |
                                  apply(!termiss[rindex, k, drop = FALSE],
                                        1, all))
        }
      }
      n.partially.used <- sum(good.tran & bad.tran & !is.na(Y))
      omit <- (!good.tran & bad.tran) | is.na(Y)
      if (all(omit))
        stop("all observations deleted due to missing values")
      temp <- setNames(seq(omit)[omit], attr(mf, "row.names")[omit])
      attr(temp, "class") <- "omit"
      mf <- mf[!omit, , drop = FALSE]
      attr(mf, "na.action") <- temp
      Y <- Y[!omit]
      id <- id[!omit]
      if (length(istate))
        istate <- istate[!omit]
    }
  }
  if (length(dropterms)) {
    Terms2 <- Terms[-dropterms]
    X <- model.matrix(Terms2, mf, constrasts.arg = contrast.arg)
    temp <- attr(X, "assign")
    shift <- sort(dropterms)
    for (i in seq(along.with = shift))
      temp <- temp + 1 *
      (shift[i] <= temp)
    attr(X, "assign") <- temp
  }
  else
    X <- model.matrix(Terms, mf, contrasts.arg = contrast.arg)
  Xatt <- attributes(X)
  if (hasinteractions)
    adrop <- c(0, untangle.specials(Terms, "strata")$terms)
  else
    adrop <- 0
  xdrop <- Xatt$assign %in% adrop
  X <- X[,!xdrop, drop = FALSE]
  attr(X, "assign") <- Xatt$assign[!xdrop]
  attr(X, "contrasts") <- Xatt$contrasts
  offset <- model.offset(mf)
  if (is.null(offset) | all(offset == 0))
    offset <- rep(0, nrow(mf))
  else if (any(!is.finite(offset) | !is.finite(exp(offset))))
    stop("offsets must lead to a finite risk score")
  weights <- model.weights(mf)
  if (!is.null(weights) && any(!is.finite(weights)))
    stop("weights must be finite")
  assign <- attrassign(X, Terms)
  contr.save <- attr(X, "contrasts")
  if (sum(Y[, ncol(Y)]) == 0) {
    ncoef <- ncol(X)
    ctemp <- rep(NA, ncoef)
    names(ctemp) <- colnames(X)
    concordance = c(
      concordant = 0,
      discordant = 0,
      tied.x = 0,
      tied.y = 0,
      tied.xy = 0,
      concordance = NA,
      std = NA,
      timefix = FALSE
    )
    rval <- list(
      coefficients = ctemp,
      var = matrix(0, ncoef,
                   ncoef),
      loglik = c(0, 0),
      score = 0,
      iter = 0,
      linear.predictors = offset,
      residuals = rep(0, data.n),
      means = colMeans(X),
      method = method,
      n = data.n,
      nevent = 0,
      terms = Terms,
      assign = assign,
      concordance = concordance,
      wald.test = 0,
      y = Y,
      call = Call
    )
    class(rval) <- "coxph"
    return(rval)
  }
  if (multi) {
    if (length(strats) > 0) {
      stratum_map <- tmap[c(1L, strats),]
      stratum_map[-1,] <- ifelse(stratum_map[-1,] >
                                   0, 1L, 0L)
      if (nrow(stratum_map) > 2) {
        temp <- stratum_map[-1,]
        if (!all(apply(temp, 2, function(x)
          all(x ==
              0) ||
          all(x == 1)))) {
          strata.keep <- mf[, strats]
          istrat <- sapply(strata.keep, as.numeric)
        }
      }
    }
    else
      stratum_map <- tmap[1, , drop = FALSE]
    cmap <- parsecovar3(tmap, colnames(X), attr(X, "assign"),
                        covlist2$phbaseline)
    xstack <- stacker(
      cmap,
      stratum_map,
      as.integer(istate),
      X,
      Y,
      strata = istrat,
      states = states
    )
    rkeep <- unique(xstack$rindex)
    transitions <-
      survcheck2(Y[rkeep,], id[rkeep], istate[rkeep])$transitions
    X <- xstack$X
    Y <- xstack$Y
    istrat <- xstack$strata
    if (length(offset))
      offset <- offset[xstack$rindex]
    if (length(weights))
      weights <- weights[xstack$rindex]
    if (length(cluster))
      cluster <- cluster[xstack$rindex]
    t2 <- tmap[-c(1, strats), , drop = FALSE]
    r2 <- row(t2)[!duplicated(as.vector(t2)) & t2 != 0]
    c2 <- col(t2)[!duplicated(as.vector(t2)) & t2 != 0]
    a2 <- lapply(seq(along.with = r2), function(i) {
      cmap[assign[[r2[i]]], c2[i]]
    })
    tab <- table(r2)
    count <- tab[r2]
    names(a2) <-
      ifelse(count == 1,
             row.names(t2)[r2],
             paste(row.names(t2)[r2],
                   colnames(cmap)[c2], sep = "_"))
    assign <- a2
  }
  if (!all(is.finite(X)))
    stop("data contains an infinite predictor")
  if (missing(init))
    init <- NULL
  else {
    if (length(init) != ncol(X))
      stop("wrong length for init argument")
    temp <- X %*% init - sum(colMeans(X) * init) + offset
    if (any(exp(temp) > .Machine$double.xmax) || all(exp(temp) ==
                                                     0))
      stop("initial values lead to overflow or underflow of the exp function")
  }
  pterms <- sapply(mf, inherits, "coxph.penalty")
  if (any(pterms)) {
    pattr <- lapply(mf[pterms], attributes)
    pname <- names(pterms)[pterms]
    ord <- attr(Terms, "order")[match(pname, attr(Terms,
                                                  "term.labels"))]
    if (any(ord > 1))
      stop("Penalty terms cannot be in an interaction")
    pcols <- assign[match(pname, names(assign))]
    fit <- coxpenal.fit(
      X,
      Y,
      istrat,
      offset,
      init = init,
      control,
      weights = weights,
      method = method,
      row.names(mf),
      pcols,
      pattr,
      assign,
      nocenter = nocenter
    )
  }
  else {
    rname <- row.names(mf)
    if (multi)
      rname <- rname[xstack$rindex]
    if (method == "breslow" || method == "efron") {
      if (grepl("right", type))
        fit <- coxph.fit(
          X,
          Y,
          istrat,
          offset,
          init,
          control,
          weights = weights,
          method = method,
          rname,
          nocenter = nocenter
        )
      else
        fit <- agreg.fit(
          X,
          Y,
          istrat,
          offset,
          init,
          control,
          weights = weights,
          method = method,
          rname,
          nocenter = nocenter
        )
    }
    else if (method == "exact") {
      if (type == "right")
        fit <- coxexact.fit(
          X,
          Y,
          istrat,
          offset,
          init,
          control,
          weights = weights,
          method = method,
          rname,
          nocenter = nocenter
        )
      else
        fit <- agexact.fit(
          X,
          Y,
          istrat,
          offset,
          init,
          control,
          weights = weights,
          method = method,
          rname,
          nocenter = nocenter
        )
    }
    else
      stop(paste("Unknown method", method))
  }
  if (is.character(fit)) {
    fit <- list(fail = fit)
    class(fit) <- "coxph"
  }
  else {
    if (!is.null(fit$coefficients) && any(is.na(fit$coefficients))) {
      vars <- (1:length(fit$coefficients))[is.na(fit$coefficients)]
      msg <- paste("X matrix deemed to be singular; variable",
                   paste(vars, collapse = " "))
      if (!singular.ok)
        stop(msg)
    }
    fit$n <- data.n
    fit$nevent <- sum(Y[, ncol(Y)])
    fit$terms <- Terms
    fit$assign <- assign
    class(fit) <- fit$class
    fit$class <- NULL
    if (robust &&
        !is.null(fit$coefficients) && !all(is.na(fit$coefficients))) {
      fit$naive.var <- fit$var
      fit2 <- c(fit, list(
        x = X,
        y = Y,
        weights = weights
      ))
      if (length(istrat))
        fit2$strata <- istrat
      if (length(cluster)) {
        temp <- residuals.coxph(fit2,
                                type = "dfbeta",
                                collapse = cluster,
                                weighted = TRUE)
        if (is.null(init))
          fit2$linear.predictors <- 0 * fit$linear.predictors
        else
          fit2$linear.predictors <- c(X %*% init)
        temp0 <- residuals.coxph(fit2,
                                 type = "score",
                                 collapse = cluster,
                                 weighted = TRUE)
      }
      else {
        temp <- residuals.coxph(fit2, type = "dfbeta",
                                weighted = TRUE)
        fit2$linear.predictors <- 0 * fit$linear.predictors
        temp0 <- residuals.coxph(fit2, type = "score",
                                 weighted = TRUE)
      }
      fit$var <- t(temp) %*% temp
      u <- apply(as.matrix(temp0), 2, sum)
      fit$rscore <- coxph.wtest(t(temp0) %*% temp0, u,
                                control$toler.chol)$test
    }
    if (length(fit$coefficients) && is.null(fit$wald.test)) {
      nabeta <- !is.na(fit$coefficients)
      if (is.null(init))
        temp <- fit$coefficients[nabeta]
      else
        temp <-
          (fit$coefficients - init[1:length(fit$coefficients)])[nabeta]
      fit$wald.test <- coxph.wtest(fit$var[nabeta, nabeta],
                                   temp, control$toler.chol)$test
    }
    if (length(cluster))
      temp <- concordancefit(
        Y,
        fit$linear.predictors,
        istrat,
        weights,
        cluster = cluster,
        reverse = TRUE,
        timefix = FALSE
      )
    else
      temp <- concordancefit(
        Y,
        fit$linear.predictors,
        istrat,
        weights,
        reverse = TRUE,
        timefix = FALSE
      )
    if (is.matrix(temp$count))
      fit$concordance <-
      c(
        colSums(temp$count),
        concordance = temp$concordance,
        std = sqrt(temp$var)
      )
    else
      fit$concordance <- c(temp$count,
                           concordance = temp$concordance,
                           std = sqrt(temp$var))
    na.action <- attr(mf, "na.action")
    if (length(na.action))
      fit$na.action <- na.action
    if (model) {
      if (length(timetrans)) {
        stop("'model=TRUE' not supported for models with tt terms")
      }
      fit$model <- mf
    }
    if (x) {
      fit$x <- X
      if (length(timetrans))
        fit$strata <- istrat
      else if (length(strats))
        fit$strata <- strata.keep
    }
    if (y)
      fit$y <- Y
    fit$timefix <- control$timefix
  }
  if (!is.null(weights) && any(weights != 1))
    fit$weights <- weights
  if (multi) {
    fit$transitions <- transitions
    fit$states <- states
    fit$cmap <- cmap
    fit$stratum_map <- stratum_map
    fit$resid <- rowsum(fit$resid, xstack$rindex)
    single <- apply(cmap, 1, function(x)
      all(x %in% c(0,
                   max(x))))
    cindx <- col(cmap)[match(1:length(fit$coefficients),
                             cmap)]
    rindx <- row(cmap)[match(1:length(fit$coefficients),
                             cmap)]
    suffix <-
      ifelse(single[rindx], "", paste0("_", colnames(cmap)[cindx]))
    names(fit$coefficients) <- paste0(names(fit$coefficients),
                                      suffix)
    if (x)
      fit$strata <- istrat
    class(fit) <- c("coxphms", class(fit))
  }
  names(fit$means) <- names(fit$coefficients)
  fit$formula <- formula(Terms)
  if (length(xlevels) > 0)
    fit$xlevels <- xlevels
  fit$contrasts <- contr.save
  if (any(offset != 0))
    fit$offset <- offset
  fit$call <- Call
  fit
}

terms.inner <- function (x)
{
  if (inherits(x, "formula")) {
    if (length(x) == 3)
      c(terms.inner(x[[2]]), terms.inner(x[[3]]))
    else
      terms.inner(x[[2]])
  }
  else if (inherits(x, "call") && (x[[1]] != as.name("$") &&
                                   x[[1]] != as.name("["))) {
    if (x[[1]] == "+" || x[[1]] == "*" || x[[1]] == "-" ||
        x[[1]] == ":") {
      if (length(x) == 3)
        c(terms.inner(x[[2]]), terms.inner(x[[3]]))
      else
        terms.inner(x[[2]])
    }
    else if (x[[1]] == as.name("Surv"))
      unlist(lapply(x[-1], terms.inner))
    else
      terms.inner(x[[2]])
  }
  else
    (deparse(x))
}


#### lzq_Enrichment ####
lzq_GO_merge <- function(GO_pos,GO_neg){
  d1 <- GO_pos@result%>%filter(ONTOLOGY=='BP')
  d2 <- GO_neg@result%>%filter(ONTOLOGY=='BP')
  
  num1 <- num2 <- 8
  if(nrow(d1)<8){
    num1 <- nrow(d1)
    if(nrow(d2)< 16-num1){
      num2 <- nrow(d2)
    }else{num2 <- 16-num1}
  }else{
    if(nrow(d2)<8){
      num2 <- nrow(d2)
      if(nrow(d1) < 16-num2){
        num1 <- nrow(d1)
      }
    }
  }
  
  if(num1!=0){dd1 <- head(d1,num1);dd1$type <- 'Positive correlation'}else{dd1 <- d1;dd1$type <- NULL}
  if(num2!=0){dd2 <- head(d2,num2);dd2$type <- 'Negative correlation'}else{dd2 <- d2;dd2$type <- NULL}
  
  tmp1 <- rbind(dd1,dd2)%>%dplyr::select(ID,Description,GeneRatio,type,ONTOLOGY)%>%
    tidyr::separate(GeneRatio,into = c('Enriched','Detected'))%>%
    dplyr::rename(Ontology=ONTOLOGY)%>%
    dplyr::mutate(Enriched = as.numeric(Enriched),
                  Detected = as.numeric(Detected),
                  Ontology = ifelse(Ontology=='BP','Biological Process',
                                    ifelse(Ontology=='CC','Cellular Component','Molecular Function')))%>%
    dplyr::arrange(Ontology) %>%
    dplyr::mutate(ID = factor(ID, rev(unique(ID))))
  
  d1 <- GO_pos@result%>%filter(ONTOLOGY=='CC')
  d2 <- GO_neg@result%>%filter(ONTOLOGY=='CC')
  
  num1 <- num2 <- 8
  if(nrow(d1)<8){
    num1 <- nrow(d1)
    if(nrow(d2)< 16-num1){
      num2 <- nrow(d2)
    }else{num2 <- 16-num1}
  }else{
    if(nrow(d2)<8){
      num2 <- nrow(d2)
      if(nrow(d1) < 16-num2){
        num1 <- nrow(d1)
      }
    }
  }
  
  if(num1!=0){dd1 <- head(d1,num1);dd1$type <- 'Positive correlation'}else{dd1 <- d1;dd1$type <- NULL}
  if(num2!=0){dd2 <- head(d2,num2);dd2$type <- 'Negative correlation'}else{dd2 <- d2;dd2$type <- NULL}
  
  tmp2 <- rbind(dd1,dd2)%>%dplyr::select(ID,Description,GeneRatio,type,ONTOLOGY)%>%
    tidyr::separate(GeneRatio,into = c('Enriched','Detected'))%>%
    dplyr::rename(Ontology=ONTOLOGY)%>%
    dplyr::mutate(Enriched = as.numeric(Enriched),
                  Detected = as.numeric(Detected),
                  Ontology = ifelse(Ontology=='BP','Biological Process',
                                    ifelse(Ontology=='CC','Cellular Component','Molecular Function')))%>%
    dplyr::arrange(Ontology) %>%
    dplyr::mutate(ID = factor(ID, rev(unique(ID))))
  
  d1 <- GO_pos@result%>%filter(ONTOLOGY=='MF')
  d2 <- GO_neg@result%>%filter(ONTOLOGY=='MF')
  
  num1 <- num2 <- 8
  if(nrow(d1)<8){
    num1 <- nrow(d1)
    if(nrow(d2)< 16-num1){
      num2 <- nrow(d2)
    }else{num2 <- 16-num1}
  }else{
    if(nrow(d2)<8){
      num2 <- nrow(d2)
      if(nrow(d1) < 16-num2){
        num1 <- nrow(d1)
      }
    }
  }
  if(num1!=0){dd1 <- head(d1,num1);dd1$type <- 'Positive correlation'}else{dd1 <- d1;dd1$type <- NULL}
  if(num2!=0){dd2 <- head(d2,num2);dd2$type <- 'Negative correlation'}else{dd2 <- d2;dd2$type <- NULL}
  
  tmp3 <- rbind(dd1,dd2)%>%dplyr::select(ID,Description,GeneRatio,type,ONTOLOGY)%>%
    tidyr::separate(GeneRatio,into = c('Enriched','Detected'))%>%
    dplyr::rename(Ontology=ONTOLOGY)%>%
    dplyr::mutate(Enriched = as.numeric(Enriched),
                  Detected = as.numeric(Detected),
                  Ontology = ifelse(Ontology=='BP','Biological Process',
                                    ifelse(Ontology=='CC','Cellular Component','Molecular Function')))%>%
    dplyr::arrange(Ontology) %>%
    dplyr::distinct(Description,.keep_all = T)%>%
    dplyr::mutate(ID = factor(ID, rev(unique(ID))))
  tmp <- rbind(tmp1,tmp2,tmp3)%>%dplyr::distinct(ID,.keep_all = T)
  tmp$ID <- factor(tmp$ID,rev(tmp$ID))
  return(tmp)
}

lzq_GO_plot <- function(tmp,width){
  tmp_l1 <- data.frame(nrow(tmp) - (cumsum(table(tmp$Ontology)) - table(tmp$Ontology)/2))
  tmp_l1$start <- tmp_l1$Freq - table(tmp$Ontology)/2
  tmp_l1$end <- tmp_l1$Freq + table(tmp$Ontology)/2
  
  m1 <- ifelse(log(max(tmp$Detected),base = 20)*1000<1000,1000,log(max(tmp$Detected),base = 20)*1000)
  m2 <- ifelse(log(max(tmp$Enriched),base = 20)*1000<1000,1000,log(max(tmp$Enriched),base = 20)*1000)
  
  p1 <- ggplot(tmp) +
    geom_col(mapping = aes(ID, Detected),
             color = "black", fill = "#e6b8a2",
             width = 0.75, 
             show.legend = F) +
    geom_text(mapping = aes(ID, Detected, label = Detected),hjust=-0.3, size = 2.5) +
    scale_y_log10(limits = c(1, m1),expand = c(0,0),breaks=c(1,100),labels=c(1,100)) +
    coord_flip() + 
    theme_classic() +
    labs(x = NULL, y = NULL, title = "Detected Genes") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          plot.margin = unit(c(1,2,1,0), "mm"),
          axis.text.x = element_text(size=7),
          plot.title = element_text(hjust = 0.2, size = 10,face = 'bold',color='black',vjust=-1))
  
  p2 <- ggplot(tmp) +
    geom_col(mapping = aes(ID, Enriched, fill = type),color = "black", width = 0.75, show.legend = F) +
    geom_text(mapping = aes(ID, Enriched, label = Enriched),hjust=-0.3, size = 2.5) +
    scale_y_log10(limits = c(1, m2),expand = expansion(),breaks=c(1,100,1000),labels=c(1,100,1000)) +
    scale_fill_manual(values = alpha(c('#00b4d8','#ff477e'),0.9)) +
    coord_flip() + 
    theme_classic() +
    labs(x = NULL, y = NULL, title = "Enriched Genes",fill=NULL) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.x = element_text(size=7),
          plot.margin = unit(c(1,1,1,1), "mm"),
          plot.title = element_text(hjust = 0.2, size = 10,face = 'bold',vjust=-1))
  
  p0 <- ggplot(tmp) +
    geom_text(mapping = aes(ID, 0, label = ID, color = type),size = 3, show.legend = F, hjust = 0) +
    scale_color_manual(values = alpha(c('#00b4d8','#ff477e'),0.9)) +
    scale_y_continuous(expand = c(0,0),limits = c(0,0.02)) +
    coord_flip() + theme_void() +
    labs(x = NULL, y = NULL, title = "Identifiers") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          plot.margin = unit(c(1,0,1,3), "mm"),
          plot.title = element_text(hjust = 0.01, size = 10,face = 'bold',vjust=1.5))
  
  p3 <- ggplot(tmp) +
    geom_text(mapping = aes(ID, 0, label = Description, color = type), 
              size = 3, show.legend = F, hjust = 0) +
    scale_color_manual(values = alpha(c('#00b4d8','#ff477e'),0.9)) +
    scale_y_continuous(expand = c(0,0),limits = c(0,0.1)) +
    coord_flip() + 
    theme_void() +
    labs(x = NULL, y = NULL, title = "Description") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          plot.margin = unit(c(1,0.2,1,1), "mm"),
          plot.title = element_text(hjust = 0, size = 10,face = 'bold',vjust=1.5))
  
  p4 <- ggplot(tmp_l1) +
    geom_segment(mapping = aes(x = start+0.1, xend = end-0.1, y = -0.1, yend = -0.1), size = 2)+
    geom_text(mapping = aes(Freq, 0, label = Var1), size = 3, show.legend = F, hjust = 0) +
    scale_y_continuous(expand = expansion(), limits = c(-0.1,1)) +
    scale_x_continuous(expand = expansion()) +
    coord_flip() + 
    theme_void() +
    labs(x = NULL, y = NULL, title = "Ontoloty") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          plot.margin = unit(c(1,1,1,0.2), "mm"),
          plot.title = element_text(hjust = 0.1,size = 10,face = 'bold',vjust=1.5))
  
  cowplot::plot_grid(p0,p1,p2,p3,p4,align = "h", nrow = 1, 
                     rel_widths = c(0.1,0.15,0.15,width,0.2))
}

lzq_KEGG_merge <- function(KEGG_pos,KEGG_neg){
  d1 <- KEGG_pos@result
  d2 <- KEGG_neg@result
  
  num1 <- num2 <- 25
  if(nrow(d1)<25){
    num1 <- nrow(d1)
    if(nrow(d2)< 50-num1){
      num2 <- nrow(d2)
    }else{num2 <- 50-num1}
  }else{
    if(nrow(d2)<25){
      num2 <- nrow(d2)
      if(nrow(d1) < 50-num2){
        num1 <- nrow(d1)
      }
    }
  }
  
  if(num1!=0){dd1 <- head(d1,num1);dd1$type <- 'Positive correlation'}else{dd1 <- d1;dd1$type <- NULL}
  if(num2!=0){dd2 <- head(d2,num2);dd2$type <- 'Negative correlation'}else{dd2 <- d2;dd2$type <- NULL}
  
  load('./db/raw_data/common/keggdatabase.rda')
  tmp <- rbind(dd1,dd2)%>%dplyr::select(ID,Description,GeneRatio,type)%>%
    dplyr::mutate(ID = gsub('hsa','ko',ID))%>%
    tidyr::separate(GeneRatio,into = c('Enriched','Detected'))%>%
    dplyr::mutate(Enriched = as.numeric(Enriched),
                  Detected = as.numeric(Detected),
                  level3 = kegg$level3[match(ID, kegg$id)],
                  level2 = kegg$level2[match(ID, kegg$id)],
                  level1 = kegg$level1[match(ID, kegg$id)])%>%
    dplyr::arrange(level1, level2, type, level3) %>%
    dplyr::distinct(Description,.keep_all = T)
  tmp <- tmp[order(tmp$level1,tmp$level2),]
  tmp$level2 <- factor(tmp$level2,levels = unique(tmp$level2))
  tmp$level1 <- factor(tmp$level1,levels = unique(tmp$level1))
  tmp$Description <- factor(tmp$Description,levels = rev(tmp$Description))
  tmp$ID <- factor(tmp$ID,levels = rev(tmp$ID))
  return(tmp)
}

lzq_KEGG_plot <- function(tmp,width){
  tmp_l1 <- data.frame(nrow(tmp) - (cumsum(table(tmp$level1)) - table(tmp$level1)/2))
  tmp_l1$start <- tmp_l1$Freq - table(tmp$level1)/2
  tmp_l1$end <- tmp_l1$Freq + table(tmp$level1)/2
  tmp_l2 <- data.frame(nrow(tmp) - (cumsum(table(tmp$level2)) - table(tmp$level2)/2))
  tmp_l2$start <- tmp_l2$Freq - table(tmp$level2)/2
  tmp_l2$end <- tmp_l2$Freq + table(tmp$level2)/2
  
  m1 <- ifelse(log(max(tmp$Detected),base = 20)*1000<1000,1000,log(max(tmp$Detected),base = 20)*1000)
  m2 <- ifelse(log(max(tmp$Enriched),base = 20)*1000<1000,1000,log(max(tmp$Enriched),base = 20)*1000)
  
  p1 <- ggplot(tmp) +
    geom_col(mapping = aes(ID, Detected),
             color = "black", fill = "#e6b8a2",
             width = 0.75, 
             show.legend = F) +
    geom_text(mapping = aes(ID, Detected, label = Detected),hjust=-0.3, size = 2.5) +
    scale_y_log10(limits = c(1, m1),expand = c(0,0),breaks=c(1,100),labels=c(1,100)) +
    coord_flip()+ 
    theme_classic() +
    labs(x = NULL, y = NULL, title = "Detected Genes") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          plot.margin = unit(c(1,2,1,0), "mm"),
          axis.text.x = element_text(size=7),
          plot.title = element_text(hjust = 0.2, size = 10,face = 'bold',color='black',vjust=-1))
  
  p2 <- ggplot(tmp) +
    geom_col(mapping = aes(ID, Enriched, fill = type),color = "black", width = 0.75, show.legend = F) +
    geom_text(mapping = aes(ID, Enriched, label = Enriched),hjust=-0.3, size = 2.5) +
    scale_y_log10(limits = c(1, m2),expand = expansion()) +
    scale_fill_manual(values = alpha(c('#00b4d8','#ff477e'),0.9)) +
    coord_flip() + 
    theme_classic() +
    labs(x = NULL, y = NULL, title = "Enriched Genes",fill=NULL) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.x = element_text(size=7),
          plot.margin = unit(c(1,1,1,1), "mm"),
          plot.title = element_text(hjust = 0.2, size = 10,face = 'bold',vjust=-1))
  
  p0 <- ggplot(tmp) +
    geom_text(mapping = aes(ID, 0, label = ID, color = type),size = 3, show.legend = F, hjust = 0) +
    scale_color_manual(values = alpha(c('#00b4d8','#ff477e'),0.9)) +
    scale_y_continuous(expand = c(0,0),limits = c(0,0.02)) +
    coord_flip() + theme_void() +     
    labs(x = NULL, y = NULL, title = "Terms") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          plot.margin = unit(c(1,0,1,3), "mm"),
          plot.title = element_text(hjust = 0.01, size = 10,face = 'bold',vjust=1.5))
  
  p3 <- ggplot(tmp) +
    geom_text(mapping = aes(ID, 0, label = Description, color = type), 
              size = 3, show.legend = F, hjust = 0) +
    scale_color_manual(values = alpha(c('#00b4d8','#ff477e'),0.9)) +
    scale_y_continuous(expand = c(0,0),limits = c(0,0.1)) +
    coord_flip() + 
    theme_void() +
    labs(x = NULL, y = NULL, title = "Description (Level 3)") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          plot.margin = unit(c(1,0.2,1,0.2), "mm"),
          plot.title = element_text(hjust = 0, size = 10,face = 'bold',vjust=1.5))
  
  p4 <- ggplot(tmp_l2) +
    geom_segment(mapping = aes(x = start+0.1, xend = end-0.1, y = -0.1, yend = -0.1), size = 2)+
    geom_text(mapping = aes(Freq, 0, label = Var1), 
              size = 3, show.legend = F, hjust = 0) +
    scale_y_continuous(expand = c(0,0), limits = c(-0.1,1)) +
    scale_x_continuous(expand = expansion()) +
    coord_flip() + 
    theme_void() +
    labs(x = NULL, y = NULL, title = "Category (Level 2)") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          plot.margin = unit(c(1,0.2,1,0.2), "mm"),
          plot.title = element_text(hjust = 0.2, size = 10,face = 'bold',vjust=1.5))
  
  p5 <- ggplot(tmp_l1) +
    geom_segment(mapping = aes(x = start+0.1, xend = end-0.1, y = -0.1, yend = -0.1), size = 2)+
    geom_text(mapping = aes(Freq, 0, label = Var1), 
              size = 3, show.legend = F, hjust = 0) +
    scale_y_continuous(expand = c(0,0), limits = c(-0.1,1)) +
    scale_x_continuous(expand = expansion()) +
    coord_flip() + 
    theme_void() +
    labs(x = NULL, y = NULL, title = "Category (Level 1)") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          plot.margin = unit(c(1,0.2,1,0), "mm"),
          plot.title = element_text(hjust = 0.2, size = 10,face = 'bold',vjust=1.5))
  cowplot::plot_grid(p0,p1, p2,p3,p4,p5, align = "h", nrow = 1, 
                     rel_widths = c(0.08,0.15,0.15,width, 0.25, 0.25))
}

lzq_GSEA_merge <- function(GSEA_res){
  GSEA_res@result <- GSEA_res@result[order(GSEA_res@result$NES,decreasing = T),]
  num1 <- num2 <- 15
  if(sum(GSEA_res@result$NES>0)<15){
    num1 <- sum(GSEA_res@result$NES>0)
    if(sum(GSEA_res@result$NES<0)< 30-num1){
      num2 <- sum(GSEA_res@result$NES<0)
    }else{num2 <- 30-num1}
  }else{
    if(sum(GSEA_res@result$NES<0)<15){
      num2 <- sum(GSEA_res@result$NES<0)
      if(sum(GSEA_res@result$NES>0) < 30-num2){
        num1 <- sum(GSEA_res@result$NES>0)
      }
    }
  }
  
  if(num1!=0){dd1 <- head(GSEA_res@result[GSEA_res@result$NES>0,],num1)}else{dd1 <- GSEA_res@result[GSEA_res@result$NES>0,]}
  if(num2!=0){dd2 <- tail(GSEA_res@result[GSEA_res@result$NES<0,],num2)}else{dd2 <- GSEA_res@result[GSEA_res@result$NES<0,]}
  
  tmp <- rbind(dd1,dd2)
  return(list(tmp,num1,num2))
}



#### lzq_gseaplot ####
lzq_gseaplot <- function(GSEA_result, Pathway_ID, color) {
  gsdata <- enrichplot:::gsInfo(GSEA_result, Pathway_ID)
  label <-
    paste0(
      'NES = ',
      sprintf("%.3f", GSEA_result@result$NES[GSEA_result@result$ID == Pathway_ID]),
      '; FDR = ',
      format(GSEA_result@result$p.adjust[GSEA_result@result$ID ==
                                           Pathway_ID], scientific = T)
    )
  p1 <- ggplot(gsdata, aes(x)) +
    geom_line(aes(y = runningScore), size = 1, color = color) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0.01)) +
    labs(
      x = NULL,
      y = "Enrichment Score",
      title = Pathway_ID,
      subtitle = label
    ) +
    theme_bw(base_rect_size = 2) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 14,
        colour = 'darkred',
        face = 'bold'
      ),
      plot.subtitle = element_text(
        face = 'italic',
        hjust = 0.5,
        size = 11,
        colour = 'black'
      ),
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10, colour = 'black'),
      axis.title.y = element_text(
        size = 13,
        colour = 'darkred',
        face = 'bold'
      ),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      plot.margin = margin(
        t = 0.2,
        r = .2,
        b = -0.07,
        l = .2,
        unit = "cm"
      )
    )
  p2 <- ggplot(gsdata, aes(x)) +
    geom_linerange(aes(ymin = ymin, ymax = ymax), color = 'grey30') +
    xlab(NULL) + ylab(NULL) +
    theme_bw(base_rect_size = 2) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      plot.margin = margin(t = -0.1, b = 0, unit = "cm"),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line.x = element_blank()
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  p2
  v <- seq(1, sum(gsdata$position), length.out = 9)
  inv <- findInterval(rev(cumsum(gsdata$position)), v)
  if (min(inv) == 0) {
    inv <- inv + 1
  }
  col <- c(rev(brewer.pal(5, "Blues")), brewer.pal(5, "Reds"))
  ymin <- min(p2$data$ymin)
  yy <- max(p2$data$ymax - p2$data$ymin) * 0.3
  xmin <- which(!duplicated(inv))
  xmax <- xmin + as.numeric(table(inv)[as.character(unique(inv))])
  d <- data.frame(
    ymin = ymin,
    ymax = yy,
    xmin = xmin,
    xmax = xmax,
    col = col[unique(inv)]
  )
  p2 <-
    p2 + geom_rect(
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = 0,
        fill = I(col)
      ),
      data = d,
      alpha = 0.9,
      inherit.aes = FALSE
    )
  p2
  
  p3 <- ggplot(gsdata, aes(x)) +
    labs(y = 'Correlation', x = 'Rank in Ordered Dataset') +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_segment(aes(
      x = x,
      xend = x,
      y = geneList,
      yend = 0,
      color = geneList
    )) +
    scale_color_continuous(type = 'viridis') +
    theme_bw(base_rect_size = 2) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 14,
        colour = 'darkred',
        face = 'bold'
      ),
      plot.subtitle = element_text(
        face = 'italic',
        hjust = 0.5,
        size = 11,
        colour = 'black'
      ),
      panel.grid = element_blank(),
      legend.position = 'none',
      axis.text.x = element_text(size = 10, colour = 'black'),
      axis.text.y = element_text(size = 10, colour = 'black'),
      axis.title = element_text(
        size = 13,
        colour = 'darkred',
        face = 'bold'
      ),
      plot.margin = margin(
        t = -.17,
        r = .2,
        b = .2,
        l = .2,
        unit = "cm"
      )
    )
  p3
  
  require(cowplot)
  plotlist <- list(p1, p2, p3)
  n <- length(plotlist)
  plotlist[[n]] <- plotlist[[n]] +
    theme(
      axis.line.x = element_line(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text()
    )
  plot_grid(
    plotlist = plotlist,
    ncol = 1,
    align = "v",
    rel_heights = c(1.5, .2, 1)
  )
}

#### lzq_refit ####
lzq_refit <- function(results, testdata){
  if(results$select_survars=='OS'){
    gsurv <-  as.formula(Surv(OS.time,OS)~.)
  }
  if(results$select_survars=='RFS'){
    gsurv <-  as.formula(Surv(RFS.time,RFS)~.)
  }
  if(results$select_survars=='DFS'){
    gsurv <-  as.formula(Surv(DFS.time,DFS)~.)
  }
  if(results$select_survars=='PFS'){
    gsurv <-  as.formula(Surv(PFS.time,PFS)~.)
  }
  if(results$select_survars=='DSS'){
    gsurv <-  as.formula(Surv(DSS.time,DSS)~.)
  }
  
  library(survival)
  library(randomForestSRC)
  library(glmnet)
  library(gbm)
  library(survivalsvm)
  library(plsRcox)
  library(superpc)
  library(CoxBoost)
  
  if(results$method=='StepCox'){
    rs <- lapply(testdata,function(x){data.frame(ID=rownames(x),score=as.numeric(predict(results$fit,type = 'risk',newdata = x)))})
  }
  
  if(results$method=='RSF'){
    rs <- lapply(testdata,function(x){data.frame(ID=rownames(x),score=as.numeric(predict(results$fit,newdata = x)$predicted))})
  }
  
  if(results$method=='Lasso'){
    rs <- lapply(testdata,function(x){data.frame(ID=rownames(x),score=as.numeric(predict(results$fit,type='link',newx=as.matrix(x),s=results$lambda)))})
  }
  
  if(results$method=='Ridge'){
    rs <- lapply(testdata,function(x){data.frame(ID=rownames(x),score=as.numeric(predict(results$fit,type='link',newx=as.matrix(x),s=results$lambda)))})
  }
  
  if(results$method=='Enet'){
    rs <- lapply(testdata,function(x){data.frame(ID=rownames(x),score=as.numeric(predict(results$fit,type='link',newx=as.matrix(x),s=results$lambda)))})
  }
  
  if(results$method=='GBM'){
    rs <- lapply(testdata,function(x){data.frame(ID=rownames(x),score=as.numeric(predict(results$fit,x,n.trees = results$n_tree,type = 'link')))})
  }
  
  if(results$method=='SVM'){
    rs <- lapply(testdata,function(x){data.frame(ID=rownames(x),score=as.numeric(predict(results$fit,x)$predicted))})
  }
  
  if(results$method=='plsRcox'){
    rs <- lapply(testdata,function(x){data.frame(ID=rownames(x),score=as.numeric(predict(results$fit,type="lp",newdata=x)))})
  }
  
  if(results$method=='Coxboost'){
    rs <- lapply(testdata,function(x){data.frame(ID=rownames(x),score=as.numeric(predict(results$fit,newdata=x, type="lp")))})
  }
  
  if(results$method=='SuperPC'){
    rs <- lapply(testdata,function(w){
      test <- list(x=t(w),featurenames=colnames(w))
      ff <- superpc.predict(results$fit,results$data,test,threshold = results$threshold,n.components = results$SuperPC_ncomponents)
      rr <- as.numeric(ff$v.pred)
      rr2 <- data.frame(ID=rownames(w),score=rr)
      return(rr2)})
  }
  
  return(rs)
}

#### lzq_surmodel ####
lzq_surmodel <- function(
    select_survars,
    alldata,
    method,
    seed=1234,
    StepCox_direction='backward', ##包括三个选项：forward backward both
    RSF_nodesize=5,
    RSF_nsplit=10,
    RSF_splitrule="logrank", ##包括三个选项：logrank bs.gradient logrankscore
    Lasso_lamda_rule='lambda.min', #包括两个选项 lambda.min lambda.1se
    Ridge_lamda_rule='lambda.min', #包括两个选项 lambda.min lambda.1se
    Enet_alpha=0.5, ##可选择0.1-0.9
    Enet_lamda_rule='lambda.min', #包括两个选项 lambda.min lambda.1se
    GBM_nodesize=5,
    SVM_type= 'vanbelle1', ##包括四个选项：regression vanbelle1 vanbelle2 hybrid'
    SVM_diffmeth= 'makediff3', ## 包括三个选项： makediff1 makediff2 and makediff3
    SVM_optmeth= 'quadprog',   ##包括两个选项：quadprog or ipop
    SVM_kernel='add_kernel', ##包括四个选项：lin_kernel add_kernel rbf_kernel poly_kernel
    plsRcox_lambda_rule='lambda.min', #包括两个选项 lambda.min lambda.1se
    Coxboost_type='verweij', #包括两个选项 verweij naive
    SuperPC_ncomponents=1 ## Number of principal components to compute. Should be 1,2 or 3.
    
){
  library(survival)
  library(randomForestSRC)
  library(glmnet)
  library(gbm)
  library(survivalsvm)
  library(plsRcox)
  library(superpc)
  library(CoxBoost)
  
  if(select_survars=='OS'){
    gsurv <-  as.formula(Surv(OS.time,OS)~.)
  }
  if(select_survars=='RFS'){
    gsurv <-  as.formula(Surv(RFS.time,RFS)~.)
  }
  if(select_survars=='DFS'){
    gsurv <-  as.formula(Surv(DFS.time,DFS)~.)
  }
  if(select_survars=='PFS'){
    gsurv <-  as.formula(Surv(PFS.time,PFS)~.)
  }
  if(select_survars=='DSS'){
    gsurv <-  as.formula(Surv(DSS.time,DSS)~.)
  }
  
  if(method=='StepCox'){
    fit <- tryCatch(step(coxph(gsurv,alldata[[1]]),direction = StepCox_direction),error=function(e)NA)
    rs <- tryCatch(lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(predict(fit,type = 'risk',newdata = x)))}),error=function(e)NA)
  }
  
  if(method=='RSF'){
    fit <- rfsrc(formula = gsurv,
                 data = alldata[[1]],
                 ntree = 10000,
                 nodesize = RSF_nodesize,
                 nsplit = RSF_nsplit,
                 splitrule = RSF_splitrule,
                 importance = T,
                 seed = seed)
    best <- which.min(fit$err.rate)
    fit <- rfsrc(formula = gsurv,
                 data = alldata[[1]],
                 ntree = best,
                 nodesize = RSF_nodesize,
                 nsplit = RSF_nsplit,
                 splitrule = RSF_splitrule,
                 importance = T,
                 seed = seed)
    rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(predict(fit,newdata = x)$predicted))})
  }
  
  if(method=='Lasso'){
    set.seed(seed)
    fit = cv.glmnet(as.matrix(alldata[[1]][,-c(1,2)]), as.matrix(Surv(alldata[[1]][,1],alldata[[1]][,2])),family = "cox",alpha=1,nfolds = 10)
    lambda <- ifelse(Lasso_lamda_rule=='lambda.min',fit$lambda.min,fit$lambda.1se)
    rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(predict(fit,type='link',newx=as.matrix(x[,-c(1,2)]),s=lambda)))})
  }
  
  if(method=='Ridge'){
    set.seed(seed)
    fit = cv.glmnet(as.matrix(alldata[[1]][,-c(1,2)]), as.matrix(Surv(alldata[[1]][,1],alldata[[1]][,2])),family = "cox",alpha=0,nfolds = 10)
    lambda <- ifelse(Ridge_lamda_rule=='lambda.min',fit$lambda.min,fit$lambda.1se)
    rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(predict(fit,type='link',newx=as.matrix(x[,-c(1,2)]),s=lambda)))})
  }
  
  if(method=='Enet'){
    set.seed(seed)
    fit = cv.glmnet(as.matrix(alldata[[1]][,-c(1,2)]), as.matrix(Surv(alldata[[1]][,1],alldata[[1]][,2])),family = "cox",alpha=Enet_alpha,nfolds = 10)
    lambda <- ifelse(Enet_lamda_rule=='lambda.min',fit$lambda.min,fit$lambda.1se)
    rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(predict(fit,type='link',newx=as.matrix(x[,-c(1,2)]),s=lambda)))})
  }
  
  if(method=='GBM'){
    set.seed(seed)
    fit <- gbm(formula = gsurv,
               data = alldata[[1]],
               distribution = 'coxph',
               n.trees = 10000,
               interaction.depth = 3,
               n.minobsinnode = GBM_nodesize,
               shrinkage = 0.001,
               cv.folds = 10,n.cores = 6)
    n_tree <- which.min(fit$cv.error)
    rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(predict(fit,x,n.trees = n_tree,type = 'link')))})
  }
  
  if(method=='SVM'){
    fit = survivalsvm(formula = gsurv, data = alldata[[1]], gamma.mu = 0.1,
                      type = SVM_type, diff.meth = SVM_diffmeth, 
                      opt.meth = SVM_optmeth, kernel = SVM_kernel,
                      sgf.sv = 5,sigf = 7, maxiter = 20, margin = 0.05, 
                      bound = 10, eig.tol = 1e-06,
                      conv.tol = 1e-07, posd.tol = 1e-08)
    rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(predict(fit,x)$predicted))})
  }
  
  if(method=='plsRcox'){
    set.seed(seed)
    cv.plsRcox.res=cv.plsRcox(list(x=alldata[[1]][,-c(1,2)],time=alldata[[1]][,1],status=alldata[[1]][,2]),
                              nt=10,verbDFSe = FALSE,nfold = 10)
    lambda <- ifelse(plsRcox_lambda_rule=='lambda.min',cv.plsRcox.res[[5]],cv.plsRcox.res[[6]])
    fit <- plsRcox(alldata[[1]][,-c(1,2)],time=alldata[[1]][,1],event=alldata[[1]][,2],nt=lambda)
    rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(predict(fit,type="lp",newdata=x[,-c(1,2)])))})
  }
  
  if(method=='Coxboost'){
    set.seed(seed)
    pen <- optimCoxBoostPenalty(alldata[[1]][,1],alldata[[1]][,2],as.matrix(alldata[[1]][,-c(1,2)]),
                                trace=TRUE,start.penalty=1000,parallel = T)
    cv.res <- cv.CoxBoost(alldata[[1]][,1],alldata[[1]][,2],as.matrix(alldata[[1]][,-c(1,2)]),
                          maxstepno=1000,K=10,type=Coxboost_type,penalty=pen$penalty)
    fit <- CoxBoost(alldata[[1]][,1],alldata[[1]][,2],as.matrix(alldata[[1]][,-c(1,2)]),
                    stepno=cv.res$optimal.step,penalty=pen$penalty)
    rs <- lapply(alldata,function(x){cbind(x[,1:2],score=as.numeric(predict(fit,newdata=x[,-c(1,2)], newtime=x[,1], newstatus=x[,2], type="lp")))})
  }
  
  if(method=='SuperPC'){
    data <- list(x=t(alldata[[1]][,-c(1,2)]),y=alldata[[1]][,1],censoring.status=alldata[[1]][,2],featurenames=colnames(alldata[[1]])[-c(1,2)])
    set.seed(seed)
    fit <- superpc.train(data = data,type = 'survival',s0.perc = 0.5) 
    set.seed(seed)
    cv.fit <- superpc.cv(fit,data,n.threshold = 20,
                         n.fold = 10,
                         n.components=SuperPC_ncomponents,
                         min.features=0,
                         max.features=nrow(data$x),
                         compute.fullcv= TRUE,
                         compute.preval=TRUE)
    threshold <- cv.fit$thresholds[which.max(cv.fit[["scor"]][1,])]
    rs <- lapply(alldata,function(w){
      test <- list(x=t(w[,-c(1,2)]),y=w[,1],censoring.status=w[,2],featurenames=colnames(w)[-c(1,2)])
      ff <- tryCatch(superpc.predict(fit,data,test,threshold = threshold,n.components = SuperPC_ncomponents),error=function(e){NA})
      rr <- tryCatch(as.numeric(ff$v.pred),error=function(e){rep(0,nrow(w))})
      rr2 <- cbind(w[,1:2],score=rr)
      return(rr2)})
  }
  if(sum(sapply(rs,function(x){length(unique(x$score))==1}))>0|any(is.na(rs))){
    message('Fitting failed due to some error, please replace a method or genelist')
  }
  results <- list(rs=rs,
              select_survars=select_survars,
              method=method,
              fit=fit,
              lambda = tryCatch(lambda,error=function(e)NA),
              n_tree = tryCatch(n_tree,error=function(e)NA),
              threshold = tryCatch(threshold,error=function(e)NA),
              SuperPC_ncomponents=SuperPC_ncomponents,
              data = tryCatch(data,error=function(e)NA)
  )
  return(results)
}


#### lzq_survplot ####
gsurv <- function(select_survars) {
  if (select_survars == 'OS') {
    gsurv <-  as.formula(Surv(OS.time, OS) ~ .)
  }
  if (select_survars == 'RFS') {
    gsurv <-  as.formula(Surv(RFS.time, RFS) ~ .)
  }
  if (select_survars == 'DFS') {
    gsurv <-  as.formula(Surv(DFS.time, DFS) ~ .)
  }
  if (select_survars == 'PFS') {
    gsurv <-  as.formula(Surv(PFS.time, PFS) ~ .)
  }
  if (select_survars == 'DSS') {
    gsurv <-  as.formula(Surv(DSS.time, DSS) ~ .)
  }
  return(gsurv)
}

lzq_coxplot <- function(coxr) {
  coxr$Sur_var <-
    factor(coxr$Sur_var, levels = c('OS', 'DFS', 'RFS', 'DSS', 'PFS'))
  coxr <- coxr[order(coxr$HR, decreasing = T), ]
  coxr <- coxr[order(coxr$Sur_var, decreasing = F), ]
  coxr$ll <-
    ifelse(coxr$P < 0.0001, '****', ifelse(coxr$P < 0.001, '***', ifelse(
      coxr$P < 0.01, '**', ifelse(coxr$P < 0.05, '*', '')
    )))
  coxr$y <-
    factor(paste0(coxr$Sur_var, '_', coxr$Cohort), levels = rev(paste0(coxr$Sur_var, '_', coxr$Cohort)))
  cols2 <- rev(c('#B0997F', '#FF6666', '#ffc857', '#93a8ac', '#119da4'))
  print(
    ggplot(coxr, aes(HR, y)) +
      geom_rect(
        xmin = log10(min(coxr$HRL * 0.7)),
        xmax = log10(max(coxr$HRH * 1.3)),
        ymin = cumsum(rev(table(coxr$Sur_var)))[4] + 0.5,
        ymax = cumsum(rev(table(coxr$Sur_var)))[5] + 0.5,
        fill = alpha(cols2[1], 0.003)
      ) +
      geom_rect(
        xmin = log10(min(coxr$HRL * 0.7)),
        xmax = log10(max(coxr$HRH * 1.3)),
        ymin = cumsum(rev(table(coxr$Sur_var)))[3] + 0.5,
        ymax = cumsum(rev(table(coxr$Sur_var)))[4] + 0.5,
        fill = alpha(cols2[2], 0.006)
      ) +
      geom_rect(
        xmin = log10(min(coxr$HRL * 0.7)),
        xmax = log10(max(coxr$HRH * 1.3)),
        ymin = cumsum(rev(table(coxr$Sur_var)))[2] + 0.5,
        ymax = cumsum(rev(table(coxr$Sur_var)))[3] + 0.5,
        fill = alpha(cols2[3], 0.003)
      ) +
      geom_rect(
        xmin = log10(min(coxr$HRL * 0.7)),
        xmax = log10(max(coxr$HRH * 1.3)),
        ymin = cumsum(rev(table(coxr$Sur_var)))[1] + 0.5,
        ymax = cumsum(rev(table(coxr$Sur_var)))[2] + 0.5,
        fill = alpha(cols2[4], 0.002)
      ) +
      geom_rect(
        xmin = log10(min(coxr$HRL * 0.7)),
        xmax = log10(max(coxr$HRH * 1.3)),
        ymin = 0.5,
        ymax = cumsum(rev(table(coxr$Sur_var)))[1] + 0.5,
        fill = alpha('#583101', 0.006)
      ) +
      geom_vline(
        xintercept = 1,
        linetype = 2,
        color = 'grey50'
      ) +
      geom_errorbar(aes(xmin = HRL, xmax = HRH), width = 0.1, size =
                      0.8) +
      geom_point(shape = 15, size = 3, aes(color = Sur_var)) +
      scale_color_manual(values = cols2) +
      geom_text(aes(label = ll), vjust = 1.6, size = 4) +
      scale_x_log10() +
      labs(x = 'Hazard ratio', title = 'Cox regression anlaysis') +
      theme_bw(base_rect_size = 0) +
      geom_rect(
        xmin = log10(min(coxr$HRL * 0.7)),
        xmax = log10(max(coxr$HRH * 1.3)),
        ymin = cumsum(rev(table(coxr$Sur_var)))[4] + 0.5,
        ymax = cumsum(rev(table(coxr$Sur_var)))[5] + 0.5,
        fill = alpha('#B0997F', 0.01)
      ) +
      theme(
        axis.text.y = element_text(size = 11, colour = 'black'),
        axis.text.x = element_text(size = 8, colour = 'black'),
        axis.title.x = element_text(
          size = 12,
          colour = 'darkred',
          face = 'bold'
        ),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(
          size = 13,
          colour = 'darkred',
          face = 'bold'
        ),
        panel.grid = element_blank(),
        panel.grid.major = element_line(
          color = "#cacfd2",
          linetype = "dashed",
          size = 0.2
        ),
        panel.background = element_rect(fill = '#f3f6f6'),
        plot.title = element_text(
          hjust = 0.5,
          size = 12,
          colour = 'darkred',
          face = 'bold'
        ),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = 'black')
      ) +
      guides(color = guide_legend(nrow = 1))
  )
}
lzq_survplot <- function(Cohort = NULL,
                         sur_type = 'Overall survival',
                         data,
                         time = 'time',
                         event = 'status',
                         var,
                         color = c("#1F78B4", "#E31A1C", "#FDBF6F"),
                         cutoff = NULL,
                         percent = NULL,
                         main = 'CD274',
                         label = c('High', 'Low')) {
  tmp <- data[, c(time, event, var)]
  colnames(tmp) <- c('time', 'event', 'var')
  spf <- function(label = label) {
    customize_labels <- function (p,
                                  font.title = NULL,
                                  font.subtitle = NULL,
                                  font.caption = NULL,
                                  font.x = NULL,
                                  font.y = NULL,
                                  font.xtickslab = NULL,
                                  font.ytickslab = NULL)
    {
      original.p <- p
      if (is.ggplot(original.p))
        list.plots <- list(original.p)
      else if (is.list(original.p))
        list.plots <- original.p
      else
        stop("Can't handle an object of class ", class (original.p))
      .set_font <- function(font) {
        font <- ggpubr:::.parse_font(font)
        ggtext::element_markdown (
          size = font$size,
          face = font$face,
          colour = font$color
        )
      }
      for (i in 1:length(list.plots)) {
        p <- list.plots[[i]]
        if (is.ggplot(p)) {
          if (!is.null(font.title))
            p <- p + theme(plot.title = .set_font(font.title))
          if (!is.null(font.subtitle))
            p <- p + theme(plot.subtitle = .set_font(font.subtitle))
          if (!is.null(font.caption))
            p <- p + theme(plot.caption = .set_font(font.caption))
          if (!is.null(font.x))
            p <- p + theme(axis.title.x = .set_font(font.x))
          if (!is.null(font.y))
            p <- p + theme(axis.title.y = .set_font(font.y))
          if (!is.null(font.xtickslab))
            p <- p + theme(axis.text.x = .set_font(font.xtickslab))
          if (!is.null(font.ytickslab))
            p <- p + theme(axis.text.y = .set_font(font.ytickslab))
          list.plots[[i]] <- p
        }
      }
      if (is.ggplot(original.p))
        list.plots[[1]]
      else
        list.plots
    }
    pp <- ggsurvplot(
      fit,
      tmp,
      pval = TRUE,
      pval.method = T,
      ylab = NULL,
      xlab = 'Time in years',
      size = 1.3,
      conf.int = F,
      legend.title = main,
      legend.labs = label,
      legend = 'none',
      risk.table = TRUE,
      risk.table.pos = 'out',
      tables.col = "strata",
      risk.table.title = "Number at risk",
      risk.table.height = .3,
      risk.table.y.text.col = T,
      risk.table.y.text = T,
      risk.table.y.title = F,
      palette = color,
      font.main = 15,
      ggtheme = theme_bw(base_rect_size = 2)
    )
    pp$plot <- customize_labels(
      pp$plot,
      font.x        = c(14, "bold", "darkred"),
      font.y        = c(14, "bold", "darkred"),
      font.xtickslab = c(12, "plain", "black"),
      font.ytickslab = c(12, "plain", 'black')
    )
    pp$plot <-
      pp$plot + labs(y = sur_type, x = NULL, title = Cohort) +
      theme(
        plot.title = element_text(
          face = "bold",
          colour = "darkred",
          size = 18,
          hjust = 0.5
        ),
        panel.background = element_rect(fill = "#f3f6f6", color = NA),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#cacfd2", linetype = "dashed"),
        axis.line = element_line(color = "#606F7B"),
        legend.position = 'none'
      )
    pp$table <- customize_labels(
      pp$table,
      font.title  = c(14, "bold", "darkgreen"),
      font.x        = c(15, "bold", "darkred"),
      font.y        = c(14, "bold", "darkred"),
      font.xtickslab = c(12, "plain", "black"),
      font.ytickslab = c(12, "bold")
    ) +
      theme(
        panel.background = element_rect(fill = "#f3f6f6", color = NA),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#cacfd2", linetype = "dashed"),
        axis.line = element_line(color = "#606F7B"),
        legend.position = 'none'
      )
    
    # merge table and plot with patchwork
    library(patchwork)
    pp.out = pp$plot + pp$table + patchwork::plot_layout(nrow = 2, heights = c(3, 1))
    return(pp.out)
  }
  
  if (is.null(cutoff)) {
    stop(
      "Please input cutoff method! The method include 'median','mean','quantile','optimal','custom', just pick one"
    )
  }
  if (is.character(cutoff)) {
    if (cutoff == 'median') {
      tmp$Group <- ifelse(tmp$var > median(tmp$var), 'High', 'Low')
      fit <- survfit(Surv(time, event) ~ Group, tmp)
      return(spf(label))
    }
    if (cutoff == 'optimal') {
      cut <- surv_cutpoint(tmp, 'time', 'event', 'var', minprop = 0.15)
      tmp$Group <-
        ifelse(tmp$var > cut$cutpoint$cutpoint, 'High', 'Low')
      fit <- survfit(Surv(time, event) ~ Group, tmp)
      return(spf(label))
    }
    if (cutoff == 'mean') {
      tmp$Group <- ifelse(tmp$var > mean(tmp$var), 'High', 'Low')
      fit <- survfit(Surv(time, event) ~ Group, tmp)
      return(spf(label))
    }
    if (cutoff == 'quantile') {
      v1 <- as.numeric(quantile(tmp$var)[2])
      v2 <- as.numeric(quantile(tmp$var)[4])
      tmp <- subset(tmp, var > v2 | var < v1)
      tmp$Group <- ifelse(tmp$var > v1, 'High', 'Low')
      fit <- survfit(Surv(time, event) ~ Group, tmp)
      return(spf(label))
    }
    if (cutoff == 'custom') {
      if (is.null(percent)) {
        stop('You must specify the percent value for the smaller group!')
      }
      if (is.numeric(percent)) {
        x <- as.numeric(quantile(tmp$var, percent))
        tmp$Group <- ifelse(tmp$var > x, 'Low', 'High')
        fit <- survfit(Surv(time, event) ~ Group, tmp)
        return(spf(label))
      }
    }
  }
  
}

lzq_survplot2 <-
  function(Sur_ids,
           cohort,
           data,
           gene,
           cols,
           cutoff,
           Input,
           percent) {
    if ('OS' %in% Sur_ids) {
      if (cutoff != 'custom') {
        p.out = lzq_survplot(
          Cohort = cohort,
          sur_type = 'Overall survival',
          data = data,
          time = 'OS.time',
          event = 'OS',
          var = gene,
          color = cols,
          cutoff = cutoff,
          main = Input
        )
        return(p.out)
      } else{
        p.out = lzq_survplot(
          Cohort = cohort,
          sur_type = 'Overall survival',
          data = data,
          time = 'OS.time',
          event = 'OS',
          var = gene,
          color = cols,
          cutoff = 'custom',
          percent = percent,
          main = Input
        )
        return(p.out)
      }
    }
    
    if ('RFS' %in% Sur_ids) {
      if (cutoff != 'custom') {
        p.out = lzq_survplot(
          Cohort = cohort,
          sur_type = 'Relapse-free survival',
          data = data,
          time = 'RFS.time',
          event = 'RFS',
          var = gene,
          color = cols,
          cutoff = cutoff,
          main = Input
        )
        return(p.out)
      } else{
        p.out = lzq_survplot(
          Cohort = cohort,
          sur_type = 'Relapse-free survival',
          data = data,
          time = 'RFS.time',
          event = 'RFS',
          var = gene,
          color = cols,
          cutoff = 'custom',
          percent = percent,
          main = Input
        )
        return(p.out)
      }
    }
    
    if ('DFS' %in% Sur_ids) {
      if (cutoff != 'custom') {
        p.out = lzq_survplot(
          Cohort = cohort,
          sur_type = 'Disease-free survival',
          data = data,
          time = 'DFS.time',
          event = 'DFS',
          var = gene,
          color = cols,
          cutoff = cutoff,
          main = Input
        )
        return(p.out)
      } else{
        p.out = lzq_survplot(
          Cohort = cohort,
          sur_type = 'Disease-free survival',
          data = data,
          time = 'DFS.time',
          event = 'DFS',
          var = gene,
          color = cols,
          cutoff = 'custom',
          percent = percent,
          main = Input
        )
        return(p.out)
      }
    }
    
    if ('PFS' %in% Sur_ids) {
      if (cutoff != 'custom') {
        p.out = lzq_survplot(
          Cohort = cohort,
          sur_type = 'Progress-free survival',
          data = data,
          time = 'PFS.time',
          event = 'PFS',
          var = gene,
          color = cols,
          cutoff = cutoff,
          main = Input
        )
        return(p.out)
      } else{
        p.out = lzq_survplot(
          Cohort = cohort,
          sur_type = 'Progress-free survival',
          data = data,
          time = 'PFS.time',
          event = 'PFS',
          var = gene,
          color = cols,
          cutoff = 'custom',
          percent = percent,
          main = Input
        )
        return(p.out)
      }
    }
    
    if ('DSS' %in% Sur_ids) {
      if (cutoff != 'custom') {
        p.out = lzq_survplot(
          Cohort = cohort,
          sur_type = 'Disease-specific survival',
          data = data,
          time = 'DSS.time',
          event = 'DSS',
          var = gene,
          color = cols,
          cutoff = cutoff,
          main = Input
        )
        return(p.out)
      } else{
        p.out = lzq_survplot(
          Cohort = cohort,
          sur_type = 'Disease-specific survival',
          data = data,
          time = 'DSS.time',
          event = 'DSS',
          var = gene,
          color = cols,
          cutoff = 'custom',
          percent = percent,
          main = Input
        )
        return(p.out)
      }
    }
  }