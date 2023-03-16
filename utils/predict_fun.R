#------------------------------------------------------
# © 2020 Francisco M. Garcia-Moreno
#------------------------------------------------------
# source("utils.R")
# source("feature_selection.R")
require(caret)
require(e1071)
# require(randomForest)
# require(mboost)
require(ggplot2)
# require(ROSE)
require(DMwR)
require(ROCR)
require(PRROC)
# require(performanceEstimation)
# require(combinat)
require(neuralnet)
require(naivebayes)
require(RWeka)


#---------------------------------------------
# KERAS
# devtools::install_github("rstudio/keras")
# require(keras)
# install_tensorflow()
#---------------------------------------------


split_data <- function(data, split_pct = 80){
  # training and test data
  shuffle_ds <- sample(dim(data)[1])
  pct <- (dim(data)[1] * split_pct) %/% 100
  data_train <- data[shuffle_ds[1:pct], ]
  data_test <- data[shuffle_ds[(pct+1):dim(data)[1]], ]
  return(list(train=data_train, test=data_test))
}


get_formula <- function(cols, total_features=length(col)){
  return(formula(paste("y~", paste(cols, sep="", collapse = "+"), sep = "")))
}


eval_classificator <- function(formula, dataset, folds=10, reps=5, method="knn", param=3, inputNA = -1,posClass=1, verbose=F){
  set.seed(33)
  parts <- caret::createFolds(dataset[,ncol(dataset)], k=folds)
  auc <- list()
  accuracy <- c()
  tpr_l <- c()
  tnr_l <- c()
  f_l <- c()
  prec_l <-c()
  rec_l <-c()
  
  #multiclass problem: obtener metricas individuales por cada clase
  acc_l_per_class <- list()
  tpr_l_per_class <- list()
  tnr_l_per_class <- list()
  f_l_per_class <- list()
  prec_l_per_class <-list()
  rec_l_per_class <-list()
  
  maxauc <- 0
  nclasses <- length(unique(dataset$y))
  final_accuracy <- c()
  final_f <- c()
  final_tpr <- c()
  final_tnr <- c()
  final_prec <- c()
  final_rec <- c()
  final_auc <- c()
  
  final_accuracy_per_class <- list()
  final_f_per_class <- list()
  final_tpr_per_class <- list()
  final_tnr_per_class <- list()
  final_prec_per_class <- list()
  final_rec_per_class <- list()
  final_auc_per_class <- list()
  
  for(rep in seq(reps)){
    for(j in 1:folds){
      train <- dataset[-parts[[j]],]
      test <- dataset[parts[[j]],]
      # trainMinClasses <- 0 #avoid train partition has minor than 2 classes
      # testMinClasses <- 0 #avoid test partition has minor than 2 classes
      # while(trainMinClasses<2 | testMinClasses<2){
      #   train.index <- caret::createDataPartition(dataset$y, p = .5, list = FALSE)
      #   train <- dataset[ train.index,]
      #   test  <- dataset[-train.index,]
      #   trainMinClasses <- length(unique(train$y))
      #   testMinClasses <- length(unique(test$y))
      # }
      test_labels <- test[, ncol(test)]
      
      if(verbose){
        cat(paste("Fold: ",j,sep=""), "\n")
        cat(paste("Train dim: ",paste(dim(train), collapse = ","),sep=""), "\n")
        cat(paste("Test dim: ",paste(dim(test), collapse = ","),sep=""), "\n")
        cat(paste("Train balance: ", paste(table(train$y), collapse = ","), sep=""), "\n")
        cat(paste("Test balance: ", paste(table(test$y), collapse = ","), sep=""), "\n")
      }
      
      res <- run_cv_prediction(formula, train, test, method, param, inputNA)
      the_prediction <- res$prediction
      model <- res$model
      
      # acc <- (length(prediction[prediction==test_labels])/length(prediction))
      # accuracy[length(accuracy)+1] <- acc
      
      # print(formula)
      # print(test_labels)
      # print(prediction)
      # met<-performanceEstimation::classificationMetrics(test_labels, prediction, posClass=posClass)
      
      if(nclasses == 2){
        # count_ok<-table(prediction[prediction==test_labels])
        # count_wrong <- table(prediction[prediction!=test_labels])
        # tp<-count_ok[names(count_ok)==posClass]
        # fp<-count_wrong[names(count_wrong)==posClass]
        # tn<-count_ok[names(count_ok)!=posClass]
        # fn<-count_wrong[names(count_wrong)!=posClass]
        # tp_fn<-tp + fn
        # tn_fp<-tn + fp
        # 
        # # print(met)
        # tpr<-tp/tp_fn#met["tpr"]
        # tnr<-tn/tn_fp#met["tnr"]
        # f<-(2*tp)/(2*tp+fp+fn)#met["F"]
        # 
        # 
        # tpr_l[length(tpr_l)+1] <- tpr
        # tnr_l[length(tnr_l)+1] <- tnr
        # f_l[length(f_l)+1] <- f
        # 
        
        res_met<- get_metrics(the_prediction, test_labels, posClass = posClass, verbose=verbose) #get_tpr_tnr_auc_in_multiclass(unique(train$y), prediction, test_labels, verbose=verbose)
        tpr_l[length(tpr_l)+1] <- res_met$tpr
        tnr_l[length(tnr_l)+1] <- res_met$tnr
        f_l[length(f_l)+1] <- res_met$f
        prec_l[length(prec_l)+1] <- res_met$prec
        rec_l[length(rec_l)+1] <- res_met$rec
        accuracy[length(accuracy)+1] <- res_met$acc
        
        # p<- PRROC::roc.curve(test_labels, the_prediction)
        # pred1 <- ROCR::prediction(as.integer(the_prediction), test_labels)
        # perf1 <- performance(pred1,"tpr","fpr")
        # m_auc <- performance(pred1,"auc")
        # m_auc <- unlist(slot(m_auc, "y.values"))# now converting S4 class to vector
        # 
        m_auc <- 0
        
        
        # adding min and max ROC AUC to the center of the plot
        minauc<-min(round(m_auc, digits = 2))
        maxauc<-max(round(m_auc, digits = 2))
        if(verbose){
          cat("Accuracy: ", res_met$acc,". AUC roc.curve 1: ",maxauc,". TPR: ",res_met$tpr,". TNR: ",res_met$tnr,". Precision: ",res_met$prec,". Recall: ",res_met$rec,". F: ",res_met$f,"\n")
          cat("________________________________\n\n")
        }
        # auc[[length(auc)+1]] <- list(auc=maxauc, roc=perf1, roc2=NULL)
      }
      else{
        res_met<-get_tpr_tnr_auc_in_multiclass(unique(train$y), the_prediction, test_labels, verbose=verbose)
        
        #computo general
        tpr_l[length(tpr_l)+1] <- res_met$tpr
        tnr_l[length(tnr_l)+1] <- res_met$tnr
        f_l[length(f_l)+1] <- res_met$f
        prec_l[length(prec_l)+1] <- res_met$prec
        rec_l[length(rec_l)+1] <- res_met$rec
        acc <- res_met$acc #(length(the_prediction[the_prediction==test_labels])/length(the_prediction))
        accuracy[length(accuracy)+1] <- acc
        
       
        #computo por cada clase
        acc_l_per_class[[length(acc_l_per_class)+1]] <- res_met$acc_l
        f_l_per_class[[length(f_l_per_class)+1]] <- res_met$f_l
        tpr_l_per_class[[length(tpr_l_per_class)+1]] <- res_met$tpr_l
        tnr_l_per_class[[length(tnr_l_per_class)+1]] <- res_met$tnr_l
        rec_l_per_class[[length(rec_l_per_class)+1]] <- res_met$rec_l
        prec_l_per_class[[length(prec_l_per_class)+1]] <- res_met$prec_l
        
        # print(factor(test_labels,ordered=T))
        # print(factor(the_prediction,ordered=T))
        
        # p<-pROC::multiclass.roc(factor(test_labels,ordered=T), factor(the_prediction,ordered=T))
        # auc[[length(auc)+1]] <- list(auc= p$auc, roc=NULL, roc2=NULL)
        # if(verbose){
        #   cat("Accuracy: ", acc,". AUC roc.curve: ",p$auc,". TPR: ",res_met$tpr,". TNR: ",res_met$tnr,". Precision: ",res_met$prec,". Recall: ",res_met$rec, ". F: ",res_met$f,"\n")
        #   cat("________________________________\n\n")
        # }
      }
      
      
    }
    
    acc<-mean(accuracy)
    # tpr<-0
    # tnr<-0
    # f<-0
    # if(nclasses==2){
      tpr<-mean(tpr_l)
      tnr<-mean(tnr_l)
      f<-mean(f_l)
    # }
    # final_auc[length(final_auc)+1] <- mean(auc)
    final_accuracy[length(final_accuracy)+1] <- acc
    final_f[length(final_f)+1] <- f
    final_tpr[length(final_tpr)+1] <- tpr
    final_tnr[length(final_tnr)+1] <- tnr
    final_prec[length(final_prec)+1] <- mean(prec_l)
    final_rec[length(final_rec)+1] <- mean(rec_l)
    
    if(nclasses>2){
      for(c in seq(nclasses)){
        partial_tpr_per_class=list()
        partial_tnr_per_class=list()
        partial_accuracy_per_class=list()
        partial_f_per_class=list()
        partial_rec=list()
        partial_prec_per_class=list()
        for(i in seq(length(tpr_l_per_class))){ #en cada fold tenemos un array de las metricas de las nclasses [0.9, 0.8, 0.2]
          partial_tpr_per_class[[c]] <- tpr_l_per_class[[i]][c]
          partial_tnr_per_class[[c]] <- tnr_l_per_class[[i]][c]
          partial_accuracy_per_class[[c]] <-acc_l_per_class[[i]][c]
          partial_f_per_class[[c]] <- f_l_per_class[[i]][c]
          partial_rec[[c]] <- rec_l_per_class[[i]][c]
          partial_prec_per_class[[c]] <- prec_l_per_class[[i]][c]
        }
        
        final_tpr_per_class[[c]] <- mean(partial_tpr_per_class[[c]])
        final_tnr_per_class[[c]] <- mean(partial_tnr_per_class[[c]])
        final_accuracy_per_class[[c]] <- mean(partial_accuracy_per_class[[c]])
        final_f_per_class[[c]] <- mean(partial_f_per_class[[c]])
        final_rec[[c]] <- mean(partial_rec[[c]])
        final_prec_per_class[[c]] <- mean(partial_prec_per_class[[c]])
      }
    }
  }
  
  acc<-mean(final_accuracy)
  # tpr<-0
  # tnr<-0
  # f<-0
  # if(nclasses==2){
  tpr<-mean(final_tpr)
  tnr<-mean(final_tnr)
  f<-mean(final_f)
  prec<-mean(final_prec)
  rec<-mean(final_rec)
  auc <- NULL#mean(final_auc)
  
  #computo por clases:
  if(nclasses>2){
    for(c in seq(nclasses)){
      final_tpr_per_class[[c]] <- mean(final_tpr_per_class[[c]])
      final_tnr_per_class[[c]] <- mean(final_tnr_per_class[[c]])
      final_accuracy_per_class[[c]] <- mean(final_accuracy_per_class[[c]])
      final_f_per_class[[c]] <- mean(final_f_per_class[[c]])
      final_rec[[c]] <- mean(final_rec[[c]])
      final_prec_per_class[[c]] <- mean(final_prec_per_class[[c]])
    }
  }
  # }
  return(list(acc=acc,tpr=tpr,tnr=tnr,prod.tpr.tnr=tpr*tnr,prec=prec,rec=rec,f=f, auc=auc
              , acc_by_class=final_accuracy_per_class, f_by_class=final_f_per_class
              , tpr_by_class=final_tpr_per_class, tnr_by_class=final_tnr_per_class
              , rec_by_class=final_rec, prec_by_class=final_prec_per_class))
}

get_metrics <- function(pred, test_labels, posClass=1, verbose=F){
  count_ok<-table(pred[pred==test_labels])
  count_wrong <- table(pred[pred!=test_labels])
  
  tp<-sum(count_ok[names(count_ok)==posClass])
  fp<-sum(count_wrong[names(count_wrong)==posClass])
  tn<-sum(count_ok[names(count_ok)!=posClass])
  fn<-sum(count_wrong[names(count_wrong)!=posClass])
  
  
  
  tpr<-tp / (tp + fn)
  tnr<-tn / (tn + fp)
  
  if(is.nan(tpr))
    tpr <- 0
  if(is.nan(tnr))
    tnr <- 0
  
  prec <- tp / (tp+fp)
  rec <- tp / (tp+fn)
  f<-(2*tp)/(2*tp+fp+fn) #2*((prec*rec) / (prec+rec)) #(2*tp)/(2*tp+fp+fn)
  acc <- (tp+tn) / (tp+tn+fp+fn)
  
  if(is.nan(prec))
    prec <- 0
  if(is.nan(rec))
    rec <- 0
  if(is.nan(f))
    f <- 0
  if(is.nan(acc))
    acc <- 0
  
  if(verbose){
    print(paste("TP: ",tp,". FP: ",fp, ". TN: ",tn, ". FN: ",fn, ". TPR:", tpr, ". TNR: ", tnr, sep=""))
  }
  
  
  return(list(tpr=tpr, tnr=tnr, prec=prec, rec=rec, f=f, acc=acc))
}

get_tpr_tnr_auc_in_multiclass<-function(classes, pred, test_labels, verbose=F){
  auc <- c()
  tpr_l <- c()
  tnr_l <- c()
  f_l <- c()
  prec_l <- c()
  rec_l <- c()
  acc_l <- c()
  # cl <- if(length(classes)==2) 1 else length(classes)
 
  for(n in seq(length(classes))){
    # print("prediction")
    # print(pred)
    # print("test_labels")
    # print(test_labels)
    
    # count_ok<-table(pred[pred==test_labels])
    # count_wrong <- table(pred[pred!=test_labels])
    
    posClass <- n#classes[n]
    
    if(verbose)
      print(paste("Positive classs", posClass))

    
    my_metrics <- get_metrics(pred, test_labels, posClass = posClass, verbose=verbose)
    # 
    # tp<-sum(count_ok[names(count_ok)==posClass])
    # fp<-sum(count_wrong[names(count_wrong)==posClass])
    # tn<-sum(count_ok[names(count_ok)!=posClass])
    # fn<-sum(count_wrong[names(count_wrong)!=posClass])
    # 
    # if(verbose){
    #   print(paste("TP: ",tp,". FP: ",fp, ". TN: ",tn, ". FN: ",fn, sep=""))
    # }
    # 
    # tpr<-tp/tp + fn
    # tnr<-tn/tn + fp
    # prec <- tp/tp+fp
    # rec <- tp/tp+fn
    # f<-2*((prec*rec) / (prec+rec)) #(2*tp)/(2*tp+fp+fn)
    
    tpr_l[length(tpr_l)+1] <- my_metrics$tpr
    tnr_l[length(tnr_l)+1] <- my_metrics$tnr
    f_l[length(f_l)+1] <- my_metrics$f
    prec_l[length(prec_l)+1] <- my_metrics$prec 
    rec_l[length(rec_l)+1] <- my_metrics$rec
    acc_l[length(acc_l)+1] <- my_metrics$acc
    
    # library(pROC)
    # p<-multiclass.roc(factor(test_labels, ordered=T), factor(prediction,ordered=T))
    # auc[length(auc)+1] <- p$auc
  }
  return(list(acc=mean(acc_l), tpr=mean(tpr_l), tnr=mean(tnr_l), prec=mean(prec_l), rec=mean(rec_l), f=mean(f_l)
              ,acc_l=acc_l, tpr_l=tpr_l, tnr_l=tnr_l, prec_l=prec_l, rec_l=rec_l, f_l=f_l))
}

#' Evalúa un clasificador indicado usando un método dado de selección de features
#' @param dataset el conjunto de datos
#' @param fs_method método de seleccion de features
#' @param method el tipo de clasificador
#' @param parametro adicional del clasificador; en knn el k
#' @param folds total de folds para CV
#' @param minfeatTune el mínimo número de features a ajustar
#' @param maxFeatTune máximo número de features a ajustar
eval_featsel_and_classificator <- function(dataset, fs_method="mrmr", method="knn", param=3, folds=10, minfeatTune=1, maxFeatTune=1){
  tuned<-list()
  allTuned<-list()
  max<--Inf
  
  d <- conver_to_numeric_except_y(dataset)
  ranking_vars <- if(fs_method =="mrmr") apply_mrmr(d, 100) else fs_get_weights(dataset, method=fs_method)
  
  for(i in seq(minfeatTune,maxFeatTune)){
    features_sel <- NULL
    if(fs_method=="mrmr"){
      feat<-colnames(dataset)[ranking_vars[1:i]]
      features_sel <- formula(paste("y~",paste(feat,sep="",collapse = "+")))
    }
    else if(fs_method=="none"){
      feat<-colnames(dataset)[1:i]
      features_sel<- formula(paste("y~",paste(feat,sep="",collapse = "+")))
    }
    else{
      features_sel <- as.simple.formula( FSelector::cutoff.k(ranking_vars,i),"y")
    }
    
    
    parts <- caret::createFolds(dataset[,ncol(dataset)], k=folds)
    accuracy <- c()
    
    for(j in 1:folds){
      train <- dataset[-parts[[j]],]
      test <- dataset[parts[[j]],]
      test_labels <- test[, ncol(test)]
      res <- run_cv_prediction(features_sel, train, test, method, param)
      prediction <- res$prediction
      acc <- (length(prediction[prediction==test_labels])/length(prediction))
      # cat("tuning with ",i,"vars with best importance. Fold: ",j,". Accuracy: ", acc,"\n")
      accuracy[length(accuracy)+1] <- acc
    }
    
    acc<-mean(accuracy)
    
    
    cat("tuning with ",i,"vars with best importance. Accuracy: ", acc,"\n")
    
    allTuned[[length(allTuned)+1]] <- list(features=features_sel, total_features=i, accuracy=acc) 
    
    if(acc>max){
      max <- acc
      tuned <- list(features=features_sel, total_features=i, accuracy=acc) 
    }
  }
  
  
  cat(paste("Número de características seleccionadas: ",tuned$total_features, sep=""))
  cat("\nCaracterísticas seleccionadas: ",paste(tuned$features, collapse = "+"), sep=" ")
  
  return(list(features=tuned$features, accuracy=max, all=allTuned))
}

run_cv_prediction<-function(formula, train, test, method="knn", param=3, inputNA=-1){
  prediction<-c()
  need_scale<-T#method=="knn"
  train_labels <- train[, ncol(train)]
  
  method<-unlist(strsplit(method, "-"))
  # print(method)
  
  # print(colnames(train))
  # print(nrow(train))
  # print(length(train_labels))
  # print(param)
  
  if(need_scale){
    model_train<-model.frame(formula,train)
    model_test<-model.frame(formula, test)
    train_labels <- model_train$y
    
    train<-my_own_scale(model_train[-1])
    test<-my_own_scale(model_test[-1])
    
    
    
    #train <- apply(train[,-ncol(train)], 2, function(x) (x - mean(x)) / sd(x) ^ as.logical(sd(x)))
    #test <- apply(test[,-ncol(test)], 2, function(x) (x - mean(x)) / sd(x) ^ as.logical(sd(x)))

    if(total_na(train) > 0){
      print(paste("NA's despues de scale train: ", total_na(train)))
      train_labels <- model_train$y
      # print(train_labels)
      # train <- imputar_nas(train, inputNA)
      train <- replace_missing_values(train)
      
      # print(nrow(train))
      # print(length(train_labels))
    }
    if(total_na(test) > 0){
      print(paste("NA's despues de scale test: ", total_na(test)))
      # test <- imputar_nas(test, inputNA)
      test <- replace_missing_values(test)
    }
    
    #train_labels <- train[, ncol(train)]
    
    m_method <- method
    if(length(method)>1){
      m_method <- method[2]
    }
    
    if(m_method != "knn" || length(method)>1 && method[1] == "ovo"){
      train <- as.data.frame(train)
      train$y <- model_train$y
      # print(colnames(train))
      
      if(m_method=="svm"|| length(method)>1 && method[1] == "ovo"){
        test <- as.data.frame(test)
        test$y<-model_test$y
      }
    }
    
    
    # print(total_na(train))
    # print(total_na(test))
    # print(nrow(train))
    # print(length(train_labels))
  }
  
  
  model <- NULL
  if(method != "knn"){
    model <- create_model(formula, train, method, param)
    prediction <- create_prediction(model, test, method, param)
  }
  else{
    prediction <- create_prediction_knn(train, test, train_labels, param)
  }
  
  
  return(list(prediction=prediction, model=model))
}

create_and_save_model <- function(filename=paste("model/",method,"_",version,".Rds", sep=""), version="1.0.0", formula, data, method, param){
  model <- create_model(formula, data, method, param)
  saveRDS(model, file = filename, compress = TRUE)
}

#' devuelve el modelo predictivo generado
create_model <- function(formula, data, method, param=3){
  set.seed(33)
  model<-NULL
  
  if(method=="svm"){
    data$y<-as.factor(data$y)
    # print(as.character(param[,3]))
    model<-svm(formula, data=data, cost=param[,1], gamma=param[,2], kernel=as.character(param[,3]), scale=T)
    # model<-tune.out$best.model
    #plot(model,train)
    # test$y<-as.factor(test$y)
    # prediction=predict(model,test)
  }
  else if(method=="rf"){
    model<-randomForest(formula, data = data, ntree = param[,1], mtry=param[,2])
  }
  # else if(method=="knn"){
  #   # print(nrow(train))
  #   # print(length(train_labels))
  #   
  #   model <- class::knn(train, test, train_labels, param)
  #   # print("prediction done")
  # }
  else if(method=="JRip"){
    data$y<-as.factor(data$y)
    model <- JRip(formula, data)
    # prediction <- predict(model, test)
  }
  else if(method == "j48"){
    #R:use reduced error pruning; U=use unpruned tree; M:number of instances (def 2); C:pruning threshold (def 0.25)
    model <- RWeka::J48(formula, data = data, control = Weka_control(R=T, U=F, M=param[,1])) 
  }
  else if(method=="glm"){
    model<-glm(formula, data=data, family = param[,1])
    # prediction<-round(predict(model,test))
  }
  else if(method=="rpart"){ #rpart (Decision Tree)
    control <- rpart.control(minsplit = param[,1] #4,
                             , minbucket = param[,2]#round(5 / 3)
                             , maxdepth = param[,3] #3,
                             , cp = param[,4] #0
                             )
    model <- rpart(formula,data=data, method="class", control = control)
    model <- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
    
    # prediction<-round(predict(model,test))
  }
  else if(method=="nb"){ #naïve bayes
    data$y<-as.factor(data$y)
    #e1071::naiveBayes(formula, data = data)
    model <- naivebayes::naive_bayes(formula, data = data, usekernel=param[,1], usepoisson=param[,2])
  }
  else if(method=="ann"){
    model=neuralnet(formula,data=data, hidden=param[,1], act.fct = param[,2],
                 linear.output = FALSE)
  }
  else{ #Gradient Boosting (gbm)
    model <- gbm::gbm(formula,data = data, distribution = "gaussian", n.trees = param[,1], shrinkage = param[,2], interaction.depth = param[,3])
  }
  
  return(model)
}

create_prediction <- function(model, test, method, params){
  set.seed(33)
  # if(length(method)>1){
  #   if(method[1]=="ovo"){
  #     prediction<-predict_1vs1(train, test, method[2], formula, param)
  #   }
  #   else if(method[1]=="ova"){
  #     prediction<-predict_OneVSall(cbind(train, model_train[1]), cbind(test,model_test[1]), method[2], formula)
  #   }
  # }
  # else 
  if(method=="svm"){
    prediction=predict(model,test)
  }
  else if(method=="rf"){
    prediction=round(predict(model,test))
  }
  else if(method=="JRip"){
    prediction <- predict(model, test)
  }
  else if(method == "j48"){
    prediction <- predict(model, new_data=test)
  }
  else if(method=="glm"){
    prediction<-round(predict(model,test))
  }
  else if(method=="rpart"){
    prediction<-round(predict(model,as.data.frame(test)))
  }
  else if(method=="ann"){
    pred=compute(model,test)
    prediction <- ifelse(pred$net.result>0.5, 1, 0)
  }
  else if(method=="gbm"){
    prediction<-round(predict(model, as.data.frame(test), n.trees = params[,1]))
  }
  else if(method == "nb"){
    prediction <- predict(model, test, type = "class")
  }
  return(prediction)
}

create_prediction_knn <- function(train, test, train_labels, param){
  set.seed(33)
  return(class::knn(train, test, train_labels, param))
}

#' Funcion de prediccion
#' @param filename el nombre del fichero del dataset
#' @param method nombre del clasificador que va a usarse
#' @param tune True si se quiere hacer un tunning de parámetros
#' @param plot_roc True para pintar la curva ROC (solo se dibujará si la clasificación es binaria)
predict_fun <- function(d, method="knn", inputNA=-1, tune=T, plot_roc=T, folds=5, reps=5){
  formula <- get_formula(colnames(d)[-ncol(d)]) #todas las features 
  
  # 80% para training y 20% para test (estratificado)
  train.rows<- createDataPartition(y= d$y, p=0.8, list = FALSE)
  train.data<- d[train.rows,] # 70% data goes in here
  test.data <- d[-train.rows,]
  test_labels <- test.data$y
  print(paste("Train: ", table(train.data$y), sep=""))
  print(paste("Test: ", table(test.data$y), sep=""))

  #---------------------
  # tunning
  #---------------------
  bestParam <- 3
  if(tune){
    tuned <- get_tunning_model(formula, train.data, method)
    bestParam <- tuned$bestParam
  }
  
  
  

  #-----------
  #ROC
  #-----------
  # bestParam<-1
  result_eval <- eval_classificator(formula, train.data, folds=folds, reps=reps, method=method, param=bestParam)
  res <- run_cv_prediction(formula, train.data, test.data, method, bestParam)
  prediction <- res$prediction
  p<-roc.curve(test_labels, prediction)
  acc <- (length(prediction[prediction==test_labels])/length(prediction))
  print(acc)
  print(p)
  pred1 <- prediction(as.integer(prediction), test_labels)
  perf1 <- performance(pred1,"tpr","fpr")
  auc <- performance(pred1,"auc")
  # now converting S4 class to vector
  auc <- unlist(slot(auc, "y.values"))
  # adding min and max ROC AUC to the center of the plot
  minauc<-min(round(auc, digits = 2))
  maxauc<-max(round(auc, digits = 2))
  minauct <- paste(c("min(AUC)  = "),minauc,sep="")
  maxauct <- paste(c("AUC = "),maxauc,sep="")
  
  aucs<-result_eval$auc
  
  par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
  plot(aucs[[1]]$roc,col="black",lty=3, lwd=3, main=paste("ROC curve for ",param,"NN SMOTE (5-fold CV)", sep=""))
  abline(0, 1, col= "gray")
  lines(data.frame(aucs[[2]]$roc@x.values, aucs[[2]]$roc@y.values),lty=3, lwd=3, col="red")
  lines(data.frame(aucs[[3]]$roc@x.values, aucs[[3]]$roc@y.values),lty=3, lwd=3, col="green")
  lines(data.frame(aucs[[4]]$roc@x.values, aucs[[4]]$roc@y.values),lty=3, lwd=3, col="blue")
  lines(data.frame(aucs[[5]]$roc@x.values, aucs[[5]]$roc@y.values),lty=3, lwd=3, col="yellow")
  lines(data.frame(perf1@x.values, perf1@y.values),lty=3, lwd=3, col="orange")
  legend(0.6,0.2,aucs[[1]]$auc,border="white",box.col = "white")
  
  
  
  
  msg_roc <- ""
  # #si la clasificación es binaria
  # if(length(unique(test_labels)) == 2){
  #   tab_train <- table(train$y)
  #   
  #   tab_test <- table(test$y)
  #   
  #   min_class_train_i <- which(unique(tab_train) == min(unique(tab_train)))
  #   min_class_train <- as.integer(names(tab_train[min_class_train_i]))
  #   
  #   min_class_test_i <- which(unique(tab_test) == min(unique(tab_test)))
  #   min_class_test <- as.integer(names(tab_test[min_class_test_i]))
  #   
  #   
  #   train_c2 <- train[train$y == min_class_train,]
  #   train_c1 <- train[train$y != min_class_train,]
  #   train_c1 <- train_c1[sample(nrow(train_c1), size=nrow(train_c2)), ]
  #   train <- rbind(train_c1, train_c2)
  #   
  #   test_c2 <- test[test$y == min_class_test,]
  #   test_c1 <- test[test$y != min_class_test,]
  #   test_c1 <- test_c1[sample(nrow(test_c1), size=nrow(test_c2)), ]
  #   test <- rbind(test_c1, test_c2)
  # }
  
  train_acc <- eval_classificator(formula, train, folds=folds, method=method, param=bestParam, inputNA)
  res <- run_cv_prediction(formula, train, test, method, param=bestParam)
  prediction <- res$prediction
  test_labels <- as.factor(test[, ncol(test)])
  test_acc <- (length(prediction[prediction==test_labels])/length(prediction))
  roc.curve(test_labels, prediction)
  
  if(length(unique(test_labels)) == 2 && plot_roc){
    roc_o <- roc(as.integer(test_labels), as.integer(prediction), direction="<")
    plot(roc_o, print.auc=T)
  }
  
  if(method == "knn")
    print(paste("Best k: ", bestParam, "(KNN). Train Accuracy: ", train_acc, ". Test Accuracy: ", test_acc, msg_roc, sep = ""))
  else
    print(paste("Best mtry: ",bestParam," (RF). Train Accuracy: ", train_acc, ". Test Accuracy: ", test_acc, msg_roc, sep = ""))
  return(tuned)
}

get_tunning_model_given_weights <- function(features, data, model="rf", folds=5, reps=5, metric="acc"){
  tuned_model <- get_tunning_model(features, data, model, folds, reps, metric = metric)
  # cat(paste("Accuracy:", tuned_model$result_eval$acc, ". F1-Score:", tuned_model$result_eval$f))
  return(tuned_model)
}

get_tunning_model <- function(formula, data, method="knn", folds=5, reps=5, maxN=as.integer(sqrt(nrow(data))), inputNA=-1,posClass=1,metric="acc", verbose=F){
  tuned<-NULL
  nclasses<-unique(data$y)
  
  grid_param <- expand.grid(param1=seq(1, maxN, 2)) #knn
  
  if(method == "rf"){
    # r<-tuneRF(data[,-ncol(data)], data$y, mtry=55, ntreeTry=35, stepFactor=2, improve=0.005, trace=F, plot=F, doBest=FALSE)
    # grid_param <- expand.grid(param1=c(100, 1000, 100), param2=r[,1]) #param1: ntrees; param2: mtry (buscados previamente con tuneRF)
    grid_param <- expand.grid(param1=c(100, 200, 500, 1000), param2=c(10, 25, 50))
  }
  else if(method=="gbm"){
    grid_param <- expand.grid(param1=c(100, 200, 500, 1000), param2=c(0.1, 0.01, 0.001), param3=seq(5))
  }
  else if(method == "svm")
    grid_param <- expand.grid(param1=10^(-1:2), param2=c(.5,1,2), param3=c("radial", "polynomial","linear","sigmoid")) #param1 = cost; param2=gamma
  else if(method == "rpart") #param1=c("anova", "poisson", "class", "exp")
    grid_param <- expand.grid(param1=c(1, 3, 5, 10), param2=c(1, 3, 5), param3=seq(1, 5), param4=seq(0,1,0.5))
  else if(method == "j48")
    grid_param <- expand.grid(param1=c(5, 10, 20, 40, 50), param2=c(0.2, 0.25, 0.3)) #param1:instances; param2: pruning confidence
  else if(method == "glm")
    grid_param <- expand.grid((param1=c("gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian")))
  else if(method=="ann")
    grid_param <- expand.grid(param1=seq(1, maxN, 2), param2=c("logistic")) #knn
  else if(method == "nb") #naïve bayes
    grid_param <- expand.grid(param1=c(TRUE, FALSE), param2=c(TRUE, FALSE)) #param1: useKernel; param2: usepoisson
    
  # print(grid_param)
  max<--Inf
  bestParam <- grid_param[1,]
  for(i in seq(nrow(grid_param))){
    params<-grid_param[i,]
    result_eval <- eval_classificator(formula, data, folds=folds, reps=reps, method=method, param=params, inputNA=inputNA, verbose=verbose)
    
    mMetric<- if(metric=="acc") result_eval$acc else if(metric=="f1") result_eval$f else if(metric=="prod.tpr.tnr") result_eval$prod.tpr.tnr else if(metric=="prec") result_eval$prec else if(metric=="rec") result_eval$rec else if(metric=="auc") result_eval$auc else if(metric=="tpr") result_eval$tpr else if(metric=="tnr") result_eval$tnr else result_eval$acc
    # print(mMetric)
    # print(result_eval$acc)
    # print(result_eval$f)
    
    # # if(nclasses==2){
    #   aucs_mean <- mean_aucs(result_eval$auc)$a
      if(!is.na(mMetric) && mMetric > max){
        max <- mMetric
        bestParam <- params
        result_ev <- result_eval
      }
    #   tuned[length(tuned)+1] <- aucs_mean
    #   if(verbose){
    #     cat(paste("Params: ",paste(params,collapse = ", "), ". Accuracy: ", acc, ". AUC: ", aucs_mean,". TPR: ",result_eval$tpr, ". TNR: ", result_eval$tnr, ". Precision: ", result_eval$prec, ". Recall: ", result_eval$rec, ". F: ",result_eval$f), "\n")
    #   }
    # }
    # else{
    #   if(acc > max){
    #     max <- acc
    #     bestParam <- params
    #     result_ev <- result_eval
    #   }
    #   tuned[length(tuned)+1] <- acc
    #   cat(paste("Params: ",paste(params,collapse = ", "), ". Accuracy: ", acc), "\n")
    # }
  }

  return(list(tuned=tuned, bestParam=bestParam, result_eval=result_ev))
}


#' Dibuja la curva ROC
#' @param test_labels los labels verdaderos
#' @param prediction las predicciones realizadas
plot_roc<-function(test_labels, prediction){
  roc_o <- roc(test_labels, as.integer(prediction))
  print(paste("AUC: ", auc(roc_o), sep=""))
  d <- ifelse(test_labels == prediction, 1, 0)
  plot(roc_o, print.auc=TRUE)
}

rank_comparison_auc <- function(labels, scores, plot_image=TRUE, ...){
  #l1 <- labels[1]
  labels <- as.integer(labels)#ifelse(labels == l1, 1, 0)
  scores <- as.integer(scores)#ifelse(scores == l1, 1, 0)
  score_order <- order(scores, decreasing=TRUE)
  labels <- labels[score_order]#as.logical(labels[score_order])
  scores <- scores[score_order]
  pos_scores <- scores[labels]
  neg_scores <- scores[!labels]
  n_pos <- length(pos_scores)
  n_neg <- length(neg_scores)
  M <- outer(n_pos:1, 1:n_neg,
             function(i, j) (1 + sign(pos_scores[i] - neg_scores[j]))/2)

  AUC <- auc(roc(labels, scores))#mean (M)
  if (plot_image){
    image(t(M[nrow(M):1,]), ...)
    library(pROC)
    with( roc(labels, scores),
          lines((1 + 1/n_neg)*((1 - specificities) - 0.5/n_neg), 
                (1 + 1/n_pos)*sensitivities - 0.5/n_pos, 
                col="blue", lwd=2, type='b'))
    text(0.5, 0.5, sprintf("AUC = %0.4f", AUC))
  }
  
  return(AUC)
}


get_data_set <- function(file_name,exp_type="autismo", predict_class="status", balance=T, perc.over = 800, perc.under=200, inputNA=-1,plotBars=F,xticsSize=12, rotate=90, header=T,sep = ","){
  my_data <- load_dataset(file_name,header = header,sep=sep)
  my_data$Paciente <- NULL
  
  if(predict_class == "status"){
    my_data$y<-NULL
    my_data$y<-my_data$Status
    my_data$Status<-NULL
  }
  
  if(total_na(my_data) > 0){
    my_data <- if(inputNA != -1) imputar_nas(my_data, inputNA) else na.omit(my_data)
  }
  
  if(plotBars)
    run_bar_plot_with_table(paste(exp_type,"_notbalanced.png",sep=""), my_data, col_y_name=predict_class,xticsSize=xticsSize,rotate=rotate)
  
  print(paste("Dimensión: ",paste(dim(my_data), collapse = ",")))
  
  formula<-get_formula(colnames(my_data)[-ncol(my_data)])
  
  if(balance){
    my_data<-get_balanced_data(my_data,formula,perc.over,perc.under)
    
    if(total_na(my_data) > 0){
      my_data <- if(inputNA != -1) imputar_nas(my_data, inputNA) else na.omit(my_data)
    }
    
    
    if(plotBars)
      run_bar_plot_with_table(paste(exp_type,"_balanced.png",sep=""), my_data, col_y_name=predict_class,xticsSize=xticsSize,rotate=rotate)
  }
  
  return(my_data) #todas las features 
}

#' Obtiene el ROC
#' ROSE::ovun.sample. Los parámetros o son literales o son 
#' variable globales (locales no funciona)
#' @param d_rose_ov_sample esta variable tal cual debe ser global
#' y tener una lista con el dataset, la formula y el tipo de sampling ("over", "under", o "both"):
#' d_rose_ov_sample$formula; d_rose_ov_sample$formula, d_rose_ov_sample$sampling
#' para ello usar:
#' d_rose_ov_sample <- get_data_set("nombre del fichero del dataset")
#' get_roc_result(d_rose_ov_sample)
#' @param tune True si se quiere hacer un tunning
get_roc_result <- function(data,formula, method="knn", bestParam=1, tune=F, smote=F, perc.over=100, perc.under=200){
  inputNA <- -1
  classes<-unique(data$y)
  posClass<-classes[1]
  
  if(smote){
    data$y <- as.factor(data$y)
    data_balanced <- SMOTE(formula, data, perc.over = perc.over, perc.under=perc.under)
    data_balanced$y <- as.numeric(data_balanced$y)
  } 
  else{
    data$y <- as.factor(data$y)
    data_balanced <- data
  }
  # else{
  #   data_balanced <- ROSE::ovun.sample(d_rose_ov_sample$formula, data = d_rose_ov_sample$data, method = d_rose_ov_sample$sampling,
  #                                    N=if(!is.null(d_rose_ov_sample$N)) d_rose_ov_sample$N else if(d_rose_ov_sample$sampling=="under") min(unique(table(d_rose_ov_sample$data$y)))*2 else max(unique(table(d_rose_ov_sample$data$y)))*2,
  #                                    seed = 1)$data
  # }
  
  cat(paste("Balanced: ",paste(table(data_balanced$y),collapse = ","),sep=""),"\n")

  tuned<-NULL
  if(tune){
    cat("Tunning...\n")
    tuned <- get_tunning_model(formula, data_balanced, method,posClass=posClass)
    bestParam <- tuned$bestParam
    cat(paste("Best param: ",bestParam,sep=""))
    result_eval<-tuned$result_eval
  }
  else{
    result_eval <- eval_classificator(formula, data_balanced, folds=5, reps=5, method=method, param=bestParam, inputNA,posClass=posClass)
  }
  
  cat(paste("Balanced: ",paste(table(data_balanced$y),collapse = ","),sep=""),"\n")

  
  return(list(eval_res=result_eval, tuned=tuned))
}


plot_res_roc <- function(file_name, result, method="knn", bestParam=1,smote=T,folds=5, cex=.9, w=900, h=450, lwd=2,mar_leg=c(0.45,0.35,0.55,0.2)){
  png(paste(file_name,".png",sep=""), width = w, height = h)
  
  aucs<-result$auc
  
  #mar: bottom, left, top, and right.
  par(mfrow=c(1,2), mar=c(5,5,2,2),xaxs = "i",yaxs = "i", cex=cex)
  
  title<-if(method=="knn") paste(bestParam,"NN",sep="") else if(method=="rf") paste("ntree=",bestParam,sep="") else if (method=="svm") paste("C=",bestParam$param1,". g=",bestParam$param2,sep="")   else ""
  title <- if(smote) paste(title, " + SMOTE",sep = "")
  title <- paste(title, " (", folds,"-fold CV)",sep = "")
  
  plot(aucs[[1]]$roc,col="black",lty=3, lwd=lwd, main=title)
  abline(0, 1, col= "gray")
  auc_leg <- c()
  for(i in seq(length(aucs))){
    auc_leg[length(auc_leg)+1] <-paste("Fold ",i,". AUC: ",aucs[[i]]$auc, sep="")
    
    if(i!=1)
      lines(data.frame(aucs[[i]]$roc@x.values, aucs[[i]]$roc@y.values),lty=3, lwd=lwd, col=i)
  }
  
  legend(mar_leg[1],mar_leg[2],auc_leg,col=seq(5),lty=3,lwd=lwd,border="white",box.col = "white")
  
  aucs_mean <- mean_aucs(aucs)
  x<-aucs_mean$x
  y<-aucs_mean$y
  a<-aucs_mean$a
  plot_data <- data.frame(x, y)
  
  
  plot(plot_data,col="black",type="l",lty=3, lwd=lwd, main="Mean", ylab="True positive rate", xlab="False positive rate")
  # lines(plot_data,lty=3, lwd=0)
  abline(0, 1, col= "gray")
  legend(mar_leg[3],mar_leg[4],paste("AUC: ",a, sep=""),lty=3,lwd=lwd,border="white",box.col = "white")
  dev.off()
}

mean_aucs <- function(aucs){
  x=NULL
  y=NULL
  # print(aucs)
  if(!is.null(aucs[[1]]$roc)){
    x<-rep(0, length(unlist(aucs[[1]]$roc@x.values)))
    y<-rep(0, length(unlist(aucs[[1]]$roc@y.values)))
  }
  a<-rep(0, length(unlist(aucs[[1]]$auc)))
  for(i in seq(length(aucs))){
    if(!is.null(aucs[[1]]$roc)){
      x<-x+unlist(aucs[[i]]$roc@x.values)
      y<-y+unlist(aucs[[i]]$roc@y.values)
    }
    a<-a+unlist(aucs[[i]]$auc)
  }
  x<-x/5
  y<-y/5
  a<-a/5
  return(list(x=x, y=y, auc=a))
}

# generateBinaryModelrpart <- function(data,i,j,method="rpart",formula=formula(y~.)){
#   dataset_i <- data[data$y == i,]
#   dataset_j <- data[data$y == j,]
#   dataset_i$y <- 1
#   dataset_j$y <- 0
#   dataset <- rbind(dataset_i, dataset_j)
#   
#   if(method=="knn")
#     model <- train_model_form(formula, dataset, "knn")
#   else
#     model <- rpart(formula, dataset)
#   
#   return (model)
# }

get_knn_model<-function(data,formula,method, param){
  data$y<-as.factor(data$y)
  ctrl <- trainControl(method="repeatedcv", number=5, repeats=5) #,classProbs=TRUE,summaryFunction = twoClassSummary)
  trainModel <- caret::train(
    formula
    ,data
    , method = method
    , trControl = ctrl
    , preProcess = c("center","scale")
    , tuneLength = 15
    , tuneGrid = data.frame(.k=param))
  # trainModel$levels<-as.integer(trainModel$levels)
  print(trainModel)
}

predict_1vs1 <- function(data, test,method="rpart",formula=formula(y~.),param=1){
  nclasses <- length(unique(data$y))
  comb <- combinat::combn(nclasses, 2) #creamos las posibles combinaciones
  print(paste("clases: ",nclasses,sep = ""))
  print(comb)
  predicts <- list()
  votes<-c()
  
  for(i in seq(ncol(comb))){
    c1<-comb[,i][1]
    # c2<-comb[,i][2]
    binary_data <- convertToBinaryClass(data,c1)#createBinaryModel(data,c1,method,formula,param)
    # print(test)
    # print(test$y)
    
    res <- run_cv_prediction(formula, binary_data, test, method, param)
    prediction <- res$prediction
    predicts[[i]] <- prediction#as.integer(predict(models[i],test))
    # print(prediction)
  }

  
  # model_0vs1 <- createBinaryModel(data,0,1,method,formula,param)
  # model_0vs2 <- generateBinaryModelrpart(data,0,2,method,formula,param)
  # model_0vs3 <- generateBinaryModelrpart(data,0,3,method,formula,param)
  # model_1vs2 <- generateBinaryModelrpart(data,1,2,method,formula,param)
  # model_1vs3 <- generateBinaryModelrpart(data,1,3,method,formula,param)
  # model_2vs3 <- generateBinaryModelrpart(data,2,3,method,formula,param)
  # 
  # predict_0vs1 <- as.integer(predict(model_0vs1,test))
  # predict_0vs2 <- as.integer(predict(model_0vs2,test))
  # predict_0vs3 <- as.integer(predict(model_0vs3,test))
  # predict_1vs2 <- as.integer(predict(model_1vs2,test))
  # predict_1vs3 <- as.integer(predict(model_1vs3,test))
  # predict_2vs3 <- as.integer(predict(model_2vs3,test))

  indices<-ceiling(which(comb==i)/2) #obtenemos los indices emparejados con las demás clases
  votes[1] <- sum(unlist(predicts[indices]))
  for(i in seq(2,nclasses)){
    indices<-ceiling(which(comb==i)/2) #obtenemos los indices emparejados con las demás clases
    n_indices <- length(indices)
    
    v<-0
    for(j in seq(n_indices-1)){
      v <- v+1-unlist(predicts[indices[j]])
    }
    v<-v+unlist(predicts[indices[n_indices]])
    
    votes[i] <- v
    # votes_0 <- predict_0vs1 + predict_0vs2 + predict_0vs3
    # votes_1 <- (1-predict_0vs1) + predict_1vs2 + predict_1vs3
    # votes_2 <- (1-predict_0vs2) + (1-predict_1vs2) + predict_2vs3
    # votes_3 <- (1-predict_0vs3) + (1-predict_1vs3) + (1-predict_2vs3)
  }
  
  num_instances_test <- dim(test)[1]
  predictions <- vector(length = num_instances_test)
  
  for(i in 1:num_instances_test){
    the_votes<-c()
    for(i in seq(2,nclasses))
      the_votes<-c(the_votes,votes[i])
      
    # the_votes <- c(votes_0[i],votes_1[i], votes_2[i], votes_3[i])
   
    max_index <- which.max(the_votes)
    predicted_class <- max_index - 1
    #cat(i,max_index)
    predictions[i] <- predicted_class
  }
  
  print("1vs1")
  print(predictions)
  
  return (predictions)
}

convertToBinaryClass<-function(dataset,class){
  y<-dataset$y
  y[dataset$y != class] <- 0 
  y[dataset$y == class] <- 1
  dataset$y <- y
  return(dataset)
}

createBinaryModel <- function(data,i,method="rpart",formula=formula("y~."), param=1){
  dataset <- data
  y<-dataset[,"y"]
  y[data$y != i] <- 0 
  y[data$y == i] <- 1
  dataset$y <- y
  
  if(method=="knn"){
  #   model<-caret::knn3(formula, dataset,k=param)
  # }
    model<-get_knn_model(dataset,formula,"knn",param)
  }
  else if(method == "rpart")
    model <- rpart(formula, dataset)
  else if(method == "rf")
    model <- randomForest(formula, data = data, ntree = param)
  else if(method=="svm")
    model <-svm(formula, data=data, cost=param[,1], gamma=param[,2], kernel=as.character(param[,3]), scale=T)
  
  return (model)
}

predict_OneVSall <- function(data,test,method="rpart",formula=formula("y~.")){
  
  model0VSall <- createBinaryModel(data,0,method,formula,"up")
  model1VSall <- createBinaryModel(data,1,method,formula, "up")
  model2VSall <- createBinaryModel(data,2,method,formula, "up")
  model3VSall <- createBinaryModel(data,3,method,formula, "up")
  
  predictions_0 <- predict(model0VSall, test)
  predictions_1 <- predict(model1VSall, test)
  predictions_2 <- predict(model2VSall, test)
  predictions_3 <- predict(model3VSall, test)
  
  num_instances_test <- dim(test)[1]
  predictions <- vector(length = num_instances_test)
  
  
  for(i in 1:num_instances_test){
    votes <- c(predictions_0[i],predictions_1[i], predictions_2[i], predictions_3[i])
    max_index <- which.max(votes)
    predicted_class <- max_index - 1
    predictions[i] <- predicted_class
  }
  
  return (predictions)
}

predict_with_tunning_and_plotting <- function(data, method="knn", fig_save_path="roc_aut_pheno_status", param_name="k", tune = T, bestParam=1, smote=T, perc.over=800,perc.under=200){
  nclasses<-unique(data$y)
  formula<-get_formula(colnames(data)[-ncol(data)])
  r <- get_roc_result(data,formula, method, tune = tune, smote=smote, perc.over=perc.over, perc.under=perc.under, bestParam=bestParam)
  
  cat(paste("Proporción original: ", paste(table(data$y),collapse = ","),sep = ""),"\n")
  
  if(nclasses==2)
    plot_res_roc(fig_save_path, r$eval_res, bestParam=if(is.null(r$tuned)) bestParam else r$tuned$bestParam, method=method)
  
  tunning_step <- 2
  
  # print(r$eval_res$auc)
  #print result 
  # if(nclasses==2){
    cat("AUC: ",mean_aucs(r$eval_res$auc)$a,". ACC: ",r$eval_res$acc, ". TPR: ", r$eval_res$tpr, ". TNR: ", r$eval_res$tnr,". F: ",r$eval_res$f,"\n")
    bestParam <- if(is.null(r$tuned)) bestParam else r$tuned$bestParam
    cat(method,"&",perc.over/100,"\\% &",perc.under/100,"\\% &",if(length(bestParam) == 1) bestParam else paste(bestParam,collapse = ","),"&",round(mean_aucs(r$eval_res$auc)$a,4),"&",round(r$eval_res$acc,4), "&", round(r$eval_res$tpr,4), "&", round(r$eval_res$tnr,4),"&",round(r$eval_res$f,4),"&",round(r$eval_res$tpr*r$eval_res$tnr,4),"\\\\")
  # }
  # else{
  #   cat("ACC: ",r$eval_res$acc,"\n")
  # }
  
  #plot tunning
  if(!is.null(r$tuned)){
    x<-seq(1,length(r$tuned$tuned)*2,2)
    breaks<-seq(1,length(r$tuned$tuned)*tunning_step,5)
    if(method=="rf"){
      x<-c(c(10,50,100),seq(200, 500, 100))
      breaks<-x
    }
    else if(method=="svm"){
      x<-10^(-1:2)
      breaks<-x
    }
    
    ggplot(data=data.frame(x=x, y=r$tuned$tuned), aes(x,y)) +geom_line() + scale_x_continuous(breaks = breaks) +xlab(param_name)+ylab("AUC")
    ggsave(paste(fig_save_path,"_tunning.png",sep=""), width = 4.17, height = 4.17) #4.17in = 400px
    
    if(method=="svm"){
      x<-c(.5,1,2)
      breaks<-x
      
      ggplot(data=data.frame(x=x, y=r$tuned$tuned), aes(x,y)) +geom_line() +xlab("gamma")+ylab("AUC")
      ggsave(paste(fig_save_path,"_tunning_gamma.png",sep=""), width = 4.17, height = 4.17) #4.17in = 400px
      
    }
  }
}


tune_model_and_save_results <- function(data, file_name_save="tunning", method="knn",folds=5, nMax=sqrt(nrow(data)),
                                        my_metrics=c('ROC', 'Accuracy','Sensitivity', 'Specificity', "Spec", "Sens", "Acc")){#c('Accuracy', 'Kappa', 'AccuracyLower', 'AccuracyUpper', 'AccuracyPValue', 
                                                     #'Sensitivity', 'Specificity', 'Pos_Pred_Value', 
                                                     #'Neg_Pred_Value', 'Detection_Rate', 'AUC', 'logLoss')){
  nclasses <- length(unique(data$y))
  
  # if(class(data$y)=="integer" || class(data$y)=="numeric"){
  #   cur_classes <- unique(data$y)
  #   new_classes<-paste("X", unique(data$y), sep="")
  #   
  #   for(i in seq(nclasses)){
  #     data$y[data$y==cur_classes[i]] <- new_classes[i]
  #   }
  #  
  # }
  # 
  # data$y<-as.factor(data$y)
  
  print(paste("n rows: ", nrow(data),sep = ""))
  if(total_na(data) > 0){
    data <- na.omit(data)
    print(paste("n rows after delete NA's: ",nrow(data), sep=""))
  }
 
  
  # print(data$y)
  
  grid_param <- expand.grid(.k=seq(1,nMax,2))#sqrt(nrow(data)), 2)) #knn
  
  if(method == "rf"){
    grid_param <- expand.grid(.mtry=seq(as.integer(ncol(data))))#, .ntree=c(c(10,50,100),seq(200, 500, 100)))
  
    # customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
    # customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
    # customRF$grid <- function(x, y, len = NULL, search = "grid") {}
    # customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    #   randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
    # }
    # customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    #   predict(modelFit, newdata)
    # customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    #   predict(modelFit, newdata, type = "prob")
    # customRF$sort <- function(x) x[order(x[,1]),]
    # customRF$levels <- function(x) x$classes  
  }  
  else if(method == "svm")
    grid_param <- expand.grid(.c=2^(-1:2), .gamma=2^c(2:4), .kernel=c("radial", "polynomial","linear","sigmoid")) #param1 = cost; param2=gamma
  
  formula<-get_formula(colnames(data)[-ncol(data)])
  
  # print(grid_param)
  #Setup parallel cluster
  #If running on the command line of linux, use method='fork'
  library(doParallel)
  cl <- makeCluster(detectCores(), type='PSOCK')
  registerDoParallel(cl)
  
  if(method=="svm"){
    # model<-tune.svm(formula, data=data, cost=10^(-1:2), gamma=c=2^c(-8,-4,0,4)
    model<-tune(svm, formula, data = data, 
         ranges = list(cost=2^(-1:2), gamma=2^c(2:4), kernel=c("radial", "polynomial","linear","sigmoid")),
         tunecontrol = tune.control(sampling="cross", cross=folds))
    print(model)
  }
  else if(method=="ann"){
    model=neuralnet(formula,data=data, hidden=3, act.fct = "logistic",
                 linear.output = FALSE)
  }
  else{
    print(method)
    print(grid_param)
    print(nclasses)
    print(unique(data$y))
    print(formula)
    #Fit model
    
    
    
    library(caret)
    set.seed(33)
    model <- train(
      formula, 
      data=data, 
      method=method, 
      preProcess = c("center","scale"),
      tuneGrid=grid_param,
      metric='Accuracy',
      trControl=trainControl(
        method='cv', 
        number=folds, 
        classProbs=T,
        allowParallel = T,
        summaryFunction=if(nclasses == 2) twoClassSummary else multiClassSummary))
  }
  
  print("tuned")
  
  print(model)
  print(model$bestTune)
  print(model$metric)
  print(model$perfNames)

  
  #Stop parallel cluster
  stopCluster(cl)
  
  # dev.off()
  print(file_name_save)
  pdf(paste(file_name_save,".pdf",sep = ""))
  for(stat in model$perfNames[model$perfNames %in% my_metrics]) {
    
    print(plot(model, metric=stat))
  }
  dev.off()
  return(model)
}
