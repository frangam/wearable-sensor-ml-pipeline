#------------------------------------------------------
# © 2020 Francisco M. Garcia-Moreno
#------------------------------------------------------

require(ggplot2)
require(caret)
require(rpart)
require(FSelector)
require(pROC)
require(mlbench)
require(Boruta)
require(NoiseFiltersR)
require(RWeka)
require(dummies)
require(mice)
require(ROSE)
require(mRMRe)
require(unbalanced)
require(e1071) #svm
require(randomForest)
require(reshape)
require(Hmisc) #NAs
require(outliers)
require(mvoutlier)
require(corrplot)
require(gbm)
require(ROSE)
require(ebmc)
# source("r_scripts/utils/predict_fun.R")

#require(doMC) # configure multicore
#registerDoMC(cores=4)
#set.thread.count(4)

# library(doParallel)
# registerDoParallel(4) # Registrer a parallel backend for train
# getDoParWorkers() # check that there are 4 workers
# 

require(parallel)
require(parallelMap)
# parallelMap::parallelStartSocket(cpus = detectCores())

#' Devuelve el numero total de NA's de un data.frame
total_na<-function(data){
  sum(sapply(data, function(x) sum(is.na(x))))
}

#' Devuelve los índices de los registros del data.frame que 
#' tienen NA's
index_na<-function(data){
  which(!complete.cases(data))
}

replace_missing_values <- function(data, check_y_col=T, delete_constant_vars=T, sensors=F, verbose=F){
  if(total_na(data) > 0){
    set.seed(33)
    na_cols_names <- c()
    
    if(delete_constant_vars){
      data <- delete_constant_vars(data)
    }
    
    # print(head(data))
    
    df.clean <- as.data.frame(data)
    #Indices de valores NA
    if(sensors){
      na_cols_names <- colnames(df.clean)[ apply(df.clean, 2, anyNA) ]
    }
    else{
      na_index <- apply(is.na(df.clean), 2, which)
    }    
    # print(head(na_index))
  
    # na_index$y
    # data[15,"y"]
    if(check_y_col){
      df.clean <- data[-na_index$y,] #QUITAMOS LAS FILAS QUE TIENEN NA EN LA VARIABLE DEPENDIENTE "Y"
      if(sensors){
        na_cols_names <- colnames(df.clean)[ apply(df.clean, 2, anyNA) ]
      }else{
        na_index <- apply(is.na(df.clean), 2, which)
      }
    }
    # na_index$y
    
    if(verbose){
      print(names(na_index))
      print(length(na_index))
    }
    
    #Columnas que tienen valores perdidos
    if(!sensors){
      for(i in seq(length(na_index))){
        indexes <- na_index[[i]]
        # print(na_index)
        if(length(indexes) > 0){
          # print(i)
          na_cols_names[length(na_cols_names)+1] <- names(na_index)[i]
        }
      }
    }
    if(verbose){
      print(na_cols_names)
    }
    
    na_cols_formula <- formula(paste("~", paste(na_cols_names, collapse="+")))
    
    #Obtener valores a imputar (5 imputaciones)
    if(verbose){
      print(class(df.clean))
      print(head(df.clean))
      print(na_cols_formula)
    }
    
    impute_arg <- Hmisc::aregImpute(na_cols_formula, data = df.clean, n.impute = 5, nk = 0, burnin=3, type='pmm', pmmtype=1)
    
    
    print("imputed nas")
    
    #obtener una unica imputacion de todas las que hayamos ejecutando antes con Hmisc
    imp_values <- list()
    for (i in seq(length(impute_arg$imputed))){
      v <- impute_arg$imputed[i]
      # print(v)
      
      element_name <- names(v)
      element <- v[[1]]
      row_indexes <- row.names(element)
      
      # print(element_name)
      # print(row_indexes)
      # print(element)
      
      m <- c()
      
      for(row in 1:nrow(element)) {
        #si la variable es de tipo repuestas categoricas (1, 2, 3, 4...)
        #nos quedamos con la respuesta mayoritaria imputada
        if(sum(table(element[row, ])) != 5){
          imp_value <- as.numeric(names(sort(table(element[row, ]),decreasing=TRUE)[1]))
        }
        else{ #sino, con el valor de la media, porque es una pregunta con valor numerico
          imp_value <- as.integer(mean(element[row, ]))
        }
        
        m[row] <- imp_value
        
        # print(imp_value)
      }
      names(m) <- row_indexes
      imp_values[[length(imp_values)+1]] <- m
    }
    if(verbose){
      print(imp_values)
    }
    
    names(imp_values) <- na_cols_names
    
    #Realizamos la imputacion sobre el dataset df.clean
    for (c in na_cols_names){
      # print(which(is.na(df.clean[,c])))
      # print(df[,c][is.na(df[,c])])
      # print(imp_values[[c]])
      df.clean[,c][is.na(df.clean[,c])] <- imp_values[[c]]
    }
  }
  else{
    df.clean <- data
    print("no nas")
  }
  
  return(df.clean)
}

#' Elimina el porcentaje indicado de instancias aleatoriamente
#' que tengan algún NA en sus columnas
rem_pct_na<-function(data, pct=100){
  na.ind<-index_na(data)
  samp.clean.ind<-sample(na.ind, as.integer((pct*length(na.ind))/100))
  length(samp.clean.ind)
  clean.data<-data[-samp.clean.ind,]
  return(clean.data)
}

#' Devuelve sólo los datos numéricos de un data.frame
data_numeric<-function(x){
  return(x[,sapply(x, is.numeric)])
}

#' Devuelve sólo los datos discretos (factores)
data_discrete<-function(data){
  return(data[,sapply(data, function(x) is.factor(x))])
}

# pasar a numericas las varaibles categoricas, segun numero de niveles
cat_to_num <- function(datos,numberLevels=2){
  attach(datos)
  for(c in 1:dim(datos)[2]){
    if(is.factor(datos[,c])){
      if(length((levels(datos[,c])))<=numberLevels){
        var <- colnames(datos)[c]
        datos[,var]<-as.numeric((datos[,var]))
      }
    }
  }
  return(datos)
}

#' devuelve el dataset con dummies
data_with_dummies<-function(data, includeY=T){
  data.new<-data
  if(includeY){
    data.new$y <- as.factor(data.new$y)
    data.dummy <- dummy.data.frame(data.new[,-ncol(data.new)], sep=".")
  }
  else{
    data.dummy <- dummy.data.frame(data.new, sep=".")
  }
  
  if(includeY){
    y <- data$y
    data.dummy <- cbind(data.dummy, y)
  }
  return(data.dummy)
}

create_weights_files<-function(data, method, model, dataPrepoType="na.imp", study_name=""){
  weights<-fs_get_weights(data, method, model) 
  write.csv(weights, paste("weights/weights.",study_name,".",dataPrepoType,".",method,".",model,".csv",sep=""))
  return(weights)
}

get_weights_from_file<-function(data, method, model, dataPrepoType="na.imp", study_name=""){
  filepath <- paste("weights/weights.",study_name,".",dataPrepoType,".",method,".",model,".csv",sep="")
  
  if(file.exists(filepath))
    weights<-read.csv(filepath, row.names = 1, stringsAsFactors=F)
  else{
    weights<-create_weights_files(data, method, model, dataPrepoType, study_name)
  }
  # print(weights)
  return(weights)
}

#' Entrena un modelo y lo valida con 10-fold cross-validation
#' @param x los datos para entrenar el modelo (sin la y, variable dependiente)
#' @param y los labels correspondientes a x
#' @param k el valor de los k vecinos más cercanos a considerar (defecto = 1, 1-NN)
#' @return el modelo kNN
train_model<-function(x,y,model="knn",k=1){
  fact_y<-model=="knn"
  
  if(fact_y)
    y<-as.factor(y)
  
  x<-x[,sapply(x, is.numeric)] #solo datos numéricos
  
  ctrl <- trainControl(method="repeatedcv", number=5, repeats=3, sampling = "up") #,classProbs=TRUE,summaryFunction = twoClassSummary)
  knnModel <- caret::train(
    x=x
    , y=y
    , method = model
    , trControl = ctrl
    , preProcess = c("center","scale")
    , tuneLength = ifelse(model=="knn", 1, 15)
    , tuneGrid = if(model=="knn") data.frame(.k=k) else NULL)
  return(knnModel)
}

#' Entrena un modelo con el paquete caret
train_model_form<-function(formula, data, model="knn", sampling="up"){
  fact_y<-model=="knn" || model=="JRip" || model=="rpart"
  data<-if(model=="knn") data_numeric(data) else data
  
  #luego convertimos a factor la y
  if(fact_y)
    data$y<-as.factor(data$y)
  
  if(!is.null(sampling) && sampling=="smote") {
    sampling <- list(name = "SMOTE with more neighbors",
                  func = function (x, y) {
                    library(DMwR)
                    dat <- if (is.data.frame(x)) x else as.data.frame(x)
                    dat$.y <- as.factor(y)
                    dat <- SMOTE(.y ~ ., data = dat, k = sqrt(nrow(dat)))
                    list(x = dat[, colnames(dat) != ".y"], 
                         y = dat$.y)
                  },
                  first = TRUE)
  }
  
  ctrl <- trainControl(method="repeatedcv", number=10, repeats=5, sampling = sampling) #,classProbs=TRUE,summaryFunction = twoClassSummary)
  trainModel <- caret::train(
    formula
    ,data
    , method = model
    , trControl = ctrl
    , preProcess = if(model=="knn") c("center","scale") else NULL
    , tuneLength = 15
    , tuneGrid = if(model=="knn") data.frame(.k=1) else NULL)
  return(trainModel)
}




#' Ajusta la selección de mejores características según el accuracy que obtenga
#' @param data el conjunto de datos
#' @param method el método de selección de características
fs_tune_model<-function(data, method="chi", model="knn"){
  #set.seed(33)
  features_sel <- fs_best_features(data,method, model)
  #set.seed(33)
  resModel<-train_model_form(features_sel$features, data, model)
  return(resModel)
}



#' metric: "acc" for accuracy (the most cases) or "f1" for f1-score (imbalance datasets)
search_features_selection <- function(data, method="rf", model="rf", minfeatTune=1, maxFeatTune=ncol(data)-1, metric="acc", search_method="forward", folds=5, reps=5, verboseOnlyFeatSel=T, verbose=F){
  allTuned<-list()
  max<--Inf
  first_time <- T
  tuned <- NULL
  features_sel <- NULL
  
  labels <- data$y
  data <- as.data.frame(my_own_scale(data[,-ncol(data)]))
  data$y <- labels
  
  prev.features<-NULL
  if(method=="mrmr"){
    r <- apply_mrmr(data, maxFeatures = maxFeatTune)
    prev.features<-unlist(r@filters)
  }
  
  # weights <- if (method!="mrmr" && method!="none") get_weights_from_file(data, method, model, dataPrepoType, study_name) else 0#fs_get_weights(data, method, model) 

  for(i in if(search_method == "forward") seq(minfeatTune, maxFeatTune) else seq(maxFeatTune, minfeatTune)){
    if(first_time || search_method!="forward"){
      if(first_time)
        weights <- fs_get_weights(data, method, model)
      else if (search_method!="forward")
        weights <- fs_get_weights(data[, all.vars(features_sel)], method, model)
      
      first_time <- F
    }
    
    features_sel <- get_features_selected(data, method, model, prev.features, weights, i)
    
    if(verboseOnlyFeatSel){
      cat(paste("Total features tuning =",i))
      # print(weights)
    }
    tuned_model <- get_tunning_model(features_sel, data, model, folds=folds, reps=reps, metric = metric, verbose=verbose)
    score <- if(metric=="acc") tuned_model$result_eval$acc else if(metric=="f1") tuned_model$result_eval$f else if(metric=="prod.tnr.tpr") tuned_model$result_eval$prod.tnr.tpr else if(metric=="prec") tuned_model$result_eval$prec else if(metric=="rec") tuned_model$result_eval$rec else if(metric=="auc") tuned_model$result_eval$auc else if(metric=="tpr") tuned_model$result_eval$tpr else if(metric=="tnr") tuned_model$result_eval$tnr else tuned_model$result_eval$acc
    model_spec <- list(features=features_sel, total_features=i, bestParams=tuned_model$bestParam, accuracy=tuned_model$result_eval$acc, tpr=tuned_model$result_eval$tpr, tnr=tuned_model$result_eval$tnr, prod.tnr.tpr=tuned_model$result_eval$prod.tpr.tnr, prec=tuned_model$result_eval$prec, rec=tuned_model$result_eval$rec, f1score=tuned_model$result_eval$f
                       , acc_by_class=tuned_model$result_eval$acc_by_class, f_by_class=tuned_model$result_eval$f_by_class, tpr_by_class=tuned_model$result_eval$tpr_by_class, tnr_by_class=tuned_model$result_eval$tnr_by_class, rec_by_class=tuned_model$result_eval$rec_by_class, prec_by_class=tuned_model$result_eval$prec_by_class) 
    allTuned[[length(allTuned)+1]]<-model_spec
    
    #seleccionamos el que tenga mayor score; o si tiene el mismo score y menor numero de features
    if(score>max || (score==max && model_spec$total_features < tuned$total_features)){
      max<-score
      tuned<-model_spec
    }
    
    if(verboseOnlyFeatSel)
      cat(". Feature Selection (",search_method,"). Accuracy: ", model_spec$accuracy, "; F1:", model_spec$f1score, ";TPR:", model_spec$tpr, ";TNR:",model_spec$tnr, ";TPR * TNR:", model_spec$prod.tnr.tpr, "TPR_L:", paste(model_spec$tpr_by_class), "TNR_L:", paste(model_spec$tnr_by_class),"\n")
  }
  
  if(verboseOnlyFeatSel){
    cat(paste("Número de características seleccionadas: ",tuned$total_features,sep=""))
    cat("\nCaracterísticas seleccionadas: ",paste(tuned$features,collapse = "+"),sep=" ")
    cat(paste("\nAccuracy:", tuned$accuracy), "F1-Score:", tuned$f1score, "TPR:", tuned$tpr, "TNR:", tuned$tnr, "TPR * TNR:", tuned$tpr * tuned$tnr, "TPR_L:", paste(model_spec$tpr_by_class), "TNR_L:", paste(model_spec$tnr_by_class))
  }
  
  return(list(features=tuned$features, total_features=tuned$total_features, accuracy=tuned$accuracy, f1score=tuned$f1score, tpr=tuned$tpr, tnr=tuned$tnr, prod.tnr.tpr=tuned$prod.tnr.tpr, prec=tuned$prec, rec=tuned$rec, auc=tuned$auc, acc_by_class=tuned$acc_by_class, f1_by_class=tuned$f_by_class, tpr_by_class=tuned$tpr_by_class, tnr_by_class=tuned$tnr_by_class, prec_by_class=tuned$prec_by_class, rec_by_class=tuned$rec_by_class, bestParams=tuned$bestParams, all=if(search_method=="rfe") rev(allTuned) else allTuned))
}

get_features_selected_given_weights <- function(data, method="rf", model="rf", total_features=ncol(data)-1){
  prev.features<-NULL
  if(method=="mrmr"){
    r <- apply_mrmr(data, maxFeatures = maxFeatTune)
    prev.features<-unlist(r@filters)
  }
  
  weights <- fs_get_weights(data, method, model)
  features_sel <- get_features_selected(data, method, model, prev.features, weights, total_features)
  return(features_sel)
}

get_features_selected <- function(data, method="rf", model="rf", prev_features=NULL, weights=NULL, i=1){
  features_sel <- NULL
  if(method=="mrmr"){
    feat<-colnames(data)[prev_features[1:i]]
    features_sel <- formula(paste("y~",paste(feat,sep="",collapse = "+")))
  }
  else if(method=="none"){
    feat<-colnames(data)[1:i]
    features_sel<- formula(paste("y~",paste(feat,sep="",collapse = "+")))
  }
  else if(method=="boruta"){
    len_boruta_feat <- length(weights)
    # print(weights)
    rest_cols <- colnames(data)[!(colnames(data) %in% weights[1:i])]
    if (i>len_boruta_feat){
      cols <- c(unlist(weights), rest_cols[1:(i-len_boruta_feat)])
    }
    else{
      cols <- weights[1:i]
    }
    # print(cols)
    features_sel <- formula(paste("y~",paste(cols, sep="",collapse = "+")))
    # print(features_sel)
  }
  else{
    features_sel <- FSelector::as.simple.formula( FSelector::cutoff.k(weights,i),"y")
  }
  return(features_sel)
}

#' #' Devuelve la fórmula de selección de características
#' get_formula<-function(data, method, model, varsSelec=ncol(data), dataPrepoType="na.imp"){
#'   weights<-get_weights_from_file(method, model, dataPrepoType)
#'   subset <- FSelector::cutoff.k(weights, varsSelec)
#'   features_sel <- FSelector::as.simple.formula(subset ,"y")
#'   return(features_sel)
#' }

#' #' Dibuja una gráfica del número de features seleccionadas frente a la precisión
#' #' obtenida.
#' #' @param features una lista que a su vez tiene cada elemento que es una lista
#' #' cada elemento: $features, $accuracy
#' plot_features_vs_acc2<-function(features, title="Figura", minF=1, maxF=0, interv=3,tech="Random Forest", lineBest=F){
#'   r<-getDFFromAllFeatures(features, lineBest)
#'   df<-r$df
#'   maxF<-ifelse(maxF<=0, max(df$features), maxF)
#'   minF<-ifelse(maxF<=0, min(df$features), minF)
#'   
#'   
#'   p<-ggplot(df, aes(features, accuracy)) +
#'     geom_line(color="blue") +
#'     scale_x_continuous(breaks = seq(minF, maxF,interv)) +
#'     ggtitle(title)+theme(plot.title = element_text(hjust = 0.5))
#'   
#'   p<-if(lineBest) p+geom_vline(aes(xintercept=r$numBestFeat),color="green", linetype=5) else p
#'   return(p)
#' }

# plot_features_vs_acc<-function(features, title="Figura", minF=1, maxF=0, interv=3,tech="Random Forest", lineBest=F,myXSel=0){
#   r<-getDFFromAllFeatures(features[[1]], lineBest)
#   df<-r$df
#   
#   
#   
#   df.melt<-NULL
#   bestAcc<--Inf
#   bestAccNFeatures<-0
#   for(i in seq(length(features))){
#     r<-getDFFromAllFeatures(features[[i]], lineBest)
#     dfaux<-r$df
#     colnames(dfaux)[1]<-paste("f",i,sep="")
# 
#     if(r$bestAcc > bestAcc){
#       bestAcc<-r$bestAcc
#       bestAccNFeatures<-r$numBestFeat
#     }
#       
#     temp_df <- data.frame(x=dfaux[,1], y=dfaux[,2], col=rep(i:i, each=nrow(dfaux)))
#     df.melt <- rbind(df.melt,temp_df)
#   }
#   
#   maxF<-ifelse(maxF<=0, max(df.melt$x), maxF)
#   minF<-ifelse(maxF<=0, min(df.melt$x), minF)
#   
#   # for(i in 1:(ncol(df)-1)){
#   #   print(colnames(df)[i])
#   #   print(colnames(df)[i+1])
#   #   temp_df <- data.frame(x=df[,i+1], y=df[,i], col=rep(i:i, each=nrow(df)))
#   #   df.melt <- rbind(df.melt,temp_df)
#   # } 
#   
#   p<-ggplot(df.melt,aes(x=x,y=y,group=col,colour=factor(col))) + geom_line() +
#     scale_color_discrete(name="Feature selection", labels=tech) +
#     scale_x_continuous(breaks = seq(minF, maxF,interv)) + labs(x="Features", y="Accuracy") 
#   
#   # p<-ggplot(df.melt, aes(x=features, y=value, color=variable)) + geom_line() + 
#   #   scale_x_continuous(breaks = seq(minF, maxF,interv)) + labs(x="Features", y="Accuracy") +
#   #   scale_color_discrete(name="Feature selection", labels=c(tech,"None")) +
#   #   
#   #   ggtitle(title)+theme(plot.title = element_text(hjust = 0.5))
#   # 
#   # 
#   threshold<-NULL
#   if(lineBest && myXSel>0) {
#     threshold <- data.frame(x = c(bestAccNFeatures, myXSel), Selection = c("Best", "Manual"))
#   }
#   else if(lineBest && myXSel==0){
#     threshold<-data.frame(x = c(bestAccNFeatures), Selection = c("Best"))
#   }
# 
#   p<-p+geom_vline(aes(xintercept = x, linetype = Selection), data = threshold, color=if(myXSel==0) "green" else c("green","orange"))
# 
#   return(p)
# }

#' Dibuja una gráfica del número de features seleccionadas frente a la precisión
#' obtenida.
#' @param features una lista que a su vez tiene cada elemento que es una lista
#' @myXSel 0: toma el valor máximo de precisión logrado y obtiene el numero de features que lo consigue
#'  >0 si queremos seleccionar nosotros un numero de features concreto 
#'  (Sirve solo para dibujar una línea vertical)
#' cada elemento: $features, $accuracy
plot_features_vs_acc<-function(features,featNormal, title="Figura", minF=1, maxF=0, interv=3,tech="Random Forest", lineBest=F,myXSel=0){
  r<-getDFFromAllFeatures(features)
  df<-r$df
  dfnormal<-getDFFromAllFeatures(featNormal)$df
  maxF<-ifelse(maxF<=0, max(df$features), maxF)
  minF<-ifelse(maxF<=0, min(df$features), minF)
  accuracy2<-dfnormal$accuracy
  df<-cbind(df,accuracy2)
  
  df.melt<-melt(df, id="features")
  
  p<-ggplot(df.melt, aes(x=features, y=value, color=variable)) + geom_line() + 
    scale_x_continuous(breaks = seq(minF, maxF,interv)) + labs(x="Features", y="Accuracy") +
    scale_color_discrete(name="Feature selection", labels=c(tech,"None")) +
    
    ggtitle(title)+theme(plot.title = element_text(hjust = 0.5))
  
  
  threshold<-NULL
  if(lineBest && myXSel>0) {
    threshold <- data.frame(x = c(r$numBestFeat, myXSel), Selection = c("Best", "Manual"))
    #p<-p+geom_vline(aes(xintercept = x, linetype = name), data = threshold)#geom_vline(aes(xintercept=r$numBestFeat,linetype="Selection"),color="green",show.legend = T)+
    #scale_linetype_manual(values = c("Selection" = "dashed"),labels=c("Best"))
  }
  else if(lineBest && myXSel==0){
    threshold<-data.frame(x = c(r$numBestFeat), Selection = c("Best"))
  }
  
  p<-p+geom_vline(aes(xintercept = x, linetype = Selection), data = threshold, color=if(myXSel==0) "green" else c("green","orange"))
  
  # if(myXSel==0){
  #   p<-p+#geom_vline(aes(xintercept=myXSel),color="orange", linetype=5,show.legend = T) +
  #     scale_shape("Feature selection", labels=c(tech,"None","Best")) 
  #     #scale_color_discrete(name="Feature selection", labels=c(tech,"None","Best"))
  #   #scale_shape_manual(name="Symbols ", labels=c(tech,"None","Best"), breaks=c(0, 1), values=c(1,4)) 
  # }
  # else{
  #   p<-p+scale_color_discrete(name="Feature selection", labels=c(tech,"None","Best"))
  # }
  
  return(p)
}

plot_features_vs_acc2<-function(features, title="Figura", minF=1, maxF=0, interv=3,tech="Random Forest", lineBest=F){
  r<-getDFFromAllFeatures(features)
  df<-r$df
  maxF<-ifelse(maxF<=0, max(df$total_features), maxF)
  minF<-ifelse(maxF<=0, min(df$total_features), minF)
  
  
  p<-ggplot(df, aes(features, accuracy)) +
    geom_line(color="blue") +
    scale_x_continuous(breaks = seq(minF, maxF,interv)) +
    ggtitle(title)+theme(plot.title = element_text(hjust = 0.5))
  
  p<-if(lineBest) p+geom_vline(aes(xintercept=r$numBestFeat),color="green", linetype=5) else p
  return(p)
}

plot_features_vs_acc_list_features<-function(features, metric="acc", title="Figura", minF=1, maxF=0, interv=3,title_techs="Technique",tech="Random Forest", lineBest=F,myXSel=0, save_path="", width_in=5, height_in=4){
  r<-getDFFromAllFeatures(features[[1]], metric)
  df<-r$df
  
  df.melt<-NULL
  bestAcc<--Inf
  bestAccNFeatures<-0
  for(i in seq(length(features))){
    r<-getDFFromAllFeatures(features[[i]], metric)
    dfaux<-r$df
    colnames(dfaux)[1]<-paste("f",i,sep="")
    # print(colnames(dfaux))
    
    if(r$bestAcc > bestAcc){
      bestAcc<-r$bestAcc
      bestAccNFeatures<-r$numBestFeat
    }
    
    #x=dfaux[,2]: total_features |  y=dfaux[,3]: accuracies
    temp_df <- data.frame(x=dfaux[,2], y=dfaux[,3], col=rep(i:i, each=nrow(dfaux)))
    df.melt <- rbind(df.melt,temp_df)
  }
  
  maxF<-ifelse(maxF<=0, max(df.melt$x), maxF)
  minF<-ifelse(maxF<=0, min(df.melt$x), minF)
  
  # for(i in 1:(ncol(df)-1)){
  #   print(colnames(df)[i])
  #   print(colnames(df)[i+1])
  #   temp_df <- data.frame(x=df[,i+1], y=df[,i], col=rep(i:i, each=nrow(df)))
  #   df.melt <- rbind(df.melt,temp_df)
  # } 
  y_lab <- if(metric=="acc") "Accuracy" else if(metric=="f1") "F1-Score" else if(metric=="tpr") "Sensitivity" else if(metric=="tnr") "Specificity" else if(metric=="prod.tpr.tnr") "TPR * TNR" else if(metric=="prec") "Precision" else if(metric=="rec") "Recall" else if(metric=="auc") "AUC" else "Accuracy"
  p<-ggplot(df.melt,aes(x=x,y=y,group=col,colour=factor(col))) + geom_line() +
    scale_color_discrete(name=title_techs, labels=tech) +
    scale_x_continuous(breaks = seq(minF, maxF,interv)) + labs(x="Features", y=y_lab) 
  
  
  
  # p<-ggplot(df.melt, aes(x=features, y=value, color=variable)) + geom_line() + 
  #   scale_x_continuous(breaks = seq(minF, maxF,interv)) + labs(x="Features", y="Accuracy") +
  #   scale_color_discrete(name="Feature selection", labels=c(tech,"None")) +
  #   
  #   ggtitle(title)+theme(plot.title = element_text(hjust = 0.5))
  # 
  # 
  threshold<-NULL
  if(lineBest){
    if(myXSel>0) {
      threshold <- data.frame(x = c(bestAccNFeatures, myXSel), Selection = c("Best", "Manual"))
    }
    else{
      threshold<-data.frame(x = c(bestAccNFeatures), Selection = c("Best"))
    }
    p<-p+geom_vline(aes(xintercept = x, linetype = Selection), data = threshold) #, color=if(myXSel==0) "green" else c("green","orange"))
  }
  
  if(save_path != ""){
    ggsave(save_path, units="in", width=width_in, height=height_in, dpi=300)
  }
  
  return(p)
}


getDFFromAllFeatures<-function(features, metric="acc"){
  formulas<-c() #features
  nf<-c() #number of features
  acc<-c() #accuracy
  max<--Inf
  numBestFeat<-0
  bestAcc<-0
  for(feat in features){
    # print(feat)
    formulas[length(formulas)+1] <- paste(feat$features, sep="")
    nf[length(nf)+1]<-feat$total_features
    mvalue <- if(metric=="acc") feat$accuracy else if(metric=="f1") feat$f1score else if(metric=="tpr") feat$tpr else if(metric=="tnr") feat$tnr else if(metric=="prod.tpr.tnr") feat$tpr * feat$tnr else if(metric=="prec") feat$prec else if(metric=="rec") feat$rec else if(metric=="auc") feat$auc else feat$accuracy
    acc[length(acc)+1]<- mvalue
    
    if(mvalue>max){
      max=mvalue
      numBestFeat<-feat$total_features
      bestFeat <- feat$features
    }
  }
  df<-data.frame(features=formulas, total_features=nf, accuracy=acc)
  return(list(df=df, bestFeat=bestFeat, numBestFeat=numBestFeat ,bestAcc=max))
}

#' Devuelve los pesos según el método
#' @param method "chi", "lin.cor", "rank.cor", "oneR", "relief"
fs_get_weights<-function(data, method="chi", model="knn"){
  weights<-0
  data <- data_numeric(data)

  if(method=="chi")
    weights<-FSelector::chi.squared(y~., data_numeric(data))#if(model!="knn") data else data_numeric(data))
  else if(method=="lin.cor")
    weights <- FSelector::linear.correlation(y~., data_numeric(data))
  else if(method=="rank.cor")
    weights <- FSelector::rank.correlation(y~., data_numeric(data))
  else if(method=="inf.gain")
    weights <- FSelector::information.gain(y~., data_numeric(data))#if(model!="knn") data else data_numeric(data))
  else if(method=="gain.ratio")
    weights <- FSelector::gain.ratio(y~., data_numeric(data))#if(model!="knn") data else data_numeric(data))
  else if(method=="sym.unc")
    weights <- symmetrical.uncertainty(y~., data_numeric(data))#if(model!="knn") data else data_numeric(data))
  else if(method=="oneR") #reglas de asociación (discretos)
    weights <- FSelector::oneR(y~.,data_numeric(data))
  else if(method=="relief") #atributos continuos y discretos
    weights <- FSelector::relief(y~.,data_numeric(data))#if(model!="knn") data else data_numeric(data), neighbours.count=5, sample.size=20)
  else if(method=="rf")
    weights <- FSelector::random.forest.importance(y~.,data_numeric(data))#if(model!="knn") data else data_numeric(data), importance.type=1)
  else if (method =="boruta"){
    set.seed(33)
    boruta_data <- Boruta::Boruta(y~., data = data, doTrace = 2)
    weights <- Boruta::getSelectedAttributes(boruta_data, withTentative = F)
  }
  return(weights)
}


run_train_model<-function(formula, data, method="knn"){
  acc<-0
  #set.seed(33)
  model<-train_model_form(formula, data, method)
  if(method=="knn" || method=="JRip" || method=="rpart"){
    acc<-model$results$Accuracy
  }
  return(acc)
}


#' Ejecuta una evaluacion estratificada k-fold cross-validation
#' de una técnica indicada.
#' @param formula la fórmula con la selección de variables
run_stratified_kfold_cv<-function(formula, data, method="knn", folds=10, inPct=F, simpl=F,group1=c(0,1),group2=c(2,3), preProcess="na.imp", ova=F){
  need_numeric<-method=="knn"
  data<-if(need_numeric) data_numeric(data) else data
  parts<-caret::createFolds(data[,ncol(data)], k=folds)
  accuracy<-c()
  
  for(i in 1:folds){
    train <- data[-parts[[i]],]
    test<-data[parts[[i]],]
    
    # print(dim(train))
    # print(dim(test))
    
    accuracy[i]<-get_accuracy(formula, train, test, method, simpl,group1,group2, preProcess, ova)
    if(ova){
      print(paste("Accuracy in fold",i,":",accuracy[i]))
    }
  }
  res<-mean(accuracy)
  return(if(inPct)res*100 else res)
}

#' Get the model accuracy
get_accuracy<-function(formula,train,test,method="knn", simpl=F,group1=c(0,1),group2=c(2,3), preProcess="na.imp", ova=F){
  test_labels <- test[, ncol(test)]
  prediction<-run_cv_prediction(formula, train, test, method, simpl,group1,group2, preProcess, ova)
  acc<-(length(prediction[prediction==test_labels])/length(prediction))
  return(acc)
}

#' Ejecuta la predicción según el método
#' Modelos sin usar caret
# run_cv_prediction<-function(formula, train, test, method="knn", simpl=F,group1=c(0,1),group2=c(2,3), preProcess="na.imp", ova=F){
#   prediction<-c()
#   if(simpl){
#     prediction<-prediction_double_classifier(formula, train, test, method,group1,group2)
#   }
#   else{
#     need_scale<-!ova&&method=="knn"
#     train_labels <- train[, ncol(train)]
#     
#     if(need_scale){
#       model_train<-model.frame(formula,train)
#       model_test<-model.frame(formula, test)
# 
#       if(preProcess=="dummy"){
#         # indx.cont<-c(6:18,21,22,28:60,63:71,84,87:97)+1
#         # indx.disc<-c(1:5,19,20,23:27,61,62,72:83,85,86)+1
#         # print(ncol(model_train))
#         # print(indx.cont)
#         # train.scaled<-scale(model_train[,indx.cont])
#         # train<-cbind(model_train[,indx.disc], train.scaled)
#         # print(str(train))
#         # print(total_na(train))
#         # 
#         # test.scaled<-scale(model_test[,indx.cont])
#         # test<-cbind(model_test[,indx.disc], test.scaled)
#       }
#       else{
#         train<-my_own_scale(model_train[-1])
#         test<-my_own_scale(model_test[-1])
#       }
# 
# 
#       if(total_na(train)>0){
#         print("nas en train")
#         print(formula)
#         print(index_na(train))
#         #imp <- mice(train, m=5, method="pmm")
#         # train<-imputar_nas_mean(train) # complete(imp)
#         train <- replace_missing_values(train)
#       }
#       if(total_na(test)>0){
#         print("nas en test")
#         print(index_na(test))
#         print(formula)
#         
#         #imp <- mice(test, method="pmm")
#         # test<-imputar_nas_mean(test) #complete(imp)
#         test <- replace_missing_values(test, check_y_col = F)
#       }
#     }
#     
#     if(ova){
#       #prediction<-predict_OneVSall(cbind(train, model_train[1]), cbind(test,model_test[1]), method, formula)
#       prediction<-predict_1vs1(train, test, method, formula)
#     }
#     else if(method=="svm"){
#       train.aux<-train
#       train.aux$y<-as.factor(train.aux$y)
#       # tune.out=e1071::tune(svm, y~., data=train.aux,
#       #               kernel="radial",
#       #               ranges=list(cost=c(0.1,1,10,100,1000),
#       #                           gamma=c(0.5,1,2,3,4) ))
#       
#       # dn<-data_numeric(data)
#       # tune.out=e1071::tune(svm, train.x=dn[,-ncol(dn)],
#       #                      train.y=as.factor(dn$y),
#       #                          kernel="radial",
#       #                          ranges=list(cost=10^(-1:3),
#       #                                      gamma=c(0.5,1,2,3,4) ))
#       # print(tune.out)
#                            
#                            
#       # print(summary(tune.out$best.model))
#       model<-e1071::svm(formula, data=train,
#                 kernel='radial', cost=0.1, gamma=0.5, scale=T)
#       # model<-tune.out$best.model
#       #plot(model,train)
#       test.aux<-test
#       test.aux$y<-as.factor(test.aux$y)
#       prediction=round(predict(model,test.aux))
#     }
#     else if(method=="nb"){
#       train.aux<-train
#       train.aux$y<-as.factor(train.aux$y)
#       model=e1071::naiveBayes(formula, data=train.aux)
#       test.aux<-test
#       test.aux$y<-as.factor(test.aux$y)
#       prediction=predict(model,test.aux)
#       # print(head(prediction))
#     }
#     else if(method=="rf"){
#       model<-randomForest(formula, data = train)
#       prediction=round(predict(model,test))
#     }
#     else if(method=="knn") #1nn
#       prediction<-class::knn(train, test, train_labels, 1)
#     else if(method=="JRip"){
#       train$y<-as.factor(train$y)
#       model <- JRip(formula, train)
#       prediction <- predict(model, test)
#     }
#     else if(method=="glm"){
#       model<-glm(formula, data=train)
#       prediction<-round(predict(model,test))
#     }
#     else{ #rpart
#       tree <- rpart(formula,data=train)
#       prediction<-round(predict(tree,test))
#     }
#   }
#   
#   return(prediction)
# }

my_own_scale <- function(data){
  # data<-data.frame(a=c(1,2,3,4), b=c(1,1,1,1), c=c(2,2,2,2))
  data.scaled <- scale(data)
  nan_cols <- colnames(data.scaled)[apply(is.nan(data.scaled), 2, any)]
  
  if(length(nan_cols) > 0){
    data.scaled[, nan_cols] <- apply(data[nan_cols], 2, function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y)))
  }
  
  return(data.scaled)
}

run_prediction<-function(formula, train, test, method="knn",simpl=F,group1=c(0,1),group2=c(2,3), preprocess="na.imp"){
  need_numeric<-method=="knn"
  train<-if(need_numeric) data_numeric(train) else train
  test<-if(need_numeric) data_numeric(test) else test
  test$y<-rep(0,nrow(test)) #columna y sintética para luego seleccionar features (via formula)
  pred<-run_cv_prediction(formula, train, test, method,simpl,group1,group2,preprocess)
  return(pred)
}

# funcion de evaluacion
# Se define una funcion de evaluacion : recibe como argumento un # vector de atributos a evaluar y numero de particiones a usar
#' @param k posición de la variable de clase (dependiente)
evaluator <- function (data, subset , k=ncol(data)){
  # genera valores aleatorios (uniforme) para cada muestra del # conjunto de datos
  splits <- runif(nrow(data))
  # tratamiento de cada una de las particiones . Para cada valor de # particion se aplica la funcion que se define a continuacion
  results <- sapply(1:k, function(i) {
    # se determina el indice de las muestras para test (aproximadamente
    # una fraccion 1/k de las muestras del conjunto de datos)
    test.idx <- (splits >= ((i-1)/k) & (splits < (i/k))) # todas las demas muestras seran para training
    train.idx <- !test.idx
    # se seleccionan las muestras en si
    test <- data[test.idx, ,drop=FALSE] 
    train <- data[train.idx, , drop=FALSE]
    # aprende el modelo sobre el conjunto de entrenamiento
    tree <- rpart(FSelector::as.simple.formula(subset,"y"),train)
    # calcula la tasa de error
    error.rate<-sum(test$y !=predict(tree,test))/ nrow( test )
    # devuelve la tasa de aciertos
    return(1-error.rate) })
  
  # se muestra el subconjunto y la media de resultados y se devuelve la media de los resultados (un resultado por particion)
  #print(subset)
  #print(mean(results))
  return(mean(results))
}

#' imputamos NA's por la media de la columna (numericas)
imputar_nas_mean<-function(data){
  data.na.imp<-data
  for(i in index_na(data.na.imp)){
    ind_col<-which(is.na(data.na.imp[i,])) #columnas con NA en fila i
    for(j in ind_col){
      if(is.numeric(data[,j])){
        data.na.imp[i,j]<-mean(data.na.imp[,j], na.rm = T) 
      }
    }
  }
  return(data.na.imp)
}

run_double_classifier<- function(formula, data, training.indexes, method="knn",group1=c(0,1),group2=c(2,3))
{
  train <- data[training.indexes,]
  test <- data[-training.indexes,]
  pred<-prediction_double_classifier(formula, train, test, method,group1,group2)
  accuracy <- length(test$y[test$y == pred])/length(test$y)
  return(accuracy)
}



prediction_double_classifier<- function(formula, train, test, method="knn",group1=c(0,1),group2=c(2,3))
{
  train.simpl01 <- train[train$y %in% group1,]
  model.01 <- train_model_form(formula, train.simpl01, method)
  train.simpl23 <- train[train$y %in% group2,]
  model.23 <- train_model_form(formula, train.simpl23, method)
  
  train.simpl <- train
  #train.simpl$y <- as.numeric(train.simpl$y)
  train.simpl$y[train$y %in% group1] <- 0
  train.simpl$y[train$y %in% group2] <- 1
  #train.simpl$y <- as.factor(train.simpl$y)
  
  model.simpl <- train_model_form(formula, train.simpl, method)
  pred_0 <- as.integer(predict(model.simpl, test))-1
  
  indexes0 <- which(pred_0 == 0)
  indexes1 <- which(pred_0 == 1)
  pred_01 <- as.integer(predict(model.01, test[indexes0, ]))-1
  pred_23 <- as.integer(predict(model.23, test[indexes1, ]))+1
  
  pred <- pred_0
  pred[indexes0] <- pred_01
  pred[indexes1] <- pred_23
  pred
  return(pred)
}


predict_simpl_with_noise<- function(data.clean, data.noise, 
                           test.clean, test.noise, test.clean.ind, 
                           test.noise.ind, method="knn")
{
  f1<-fs_best_features(data.clean, "rf", "knn", simpl = T, group1=c(0,1),group2=c(2,3))
  f2<-fs_best_features(data.noise, "rf", "knn", simpl = T, group1=c(0,1),group2=c(2,3))
  
  pred1<-prediction_double_classifier(f1$features, data.clean, test.clean, group1=c(0,1),group2=c(2,3))
  pred2<-prediction_double_classifier(f2$features, data.noise, test.noise, group1=c(0,1),group2=c(2,3))
  
  pred<-c(pred1,pred2)
  pred[test.clean.ind]<-pred1
  pred[test.noise.ind]<-pred2
  
  return(pred)
}

#' usa mRMR para selección de características
#'@param estimator (pearson o spearman)
apply_mrmr<-function(data, maxFeatures=50){
  data$y<-NULL
  dd <- mRMR.data(data = data)
  
  res<-mRMR.ensemble(data = dd, target_indices = c(1),solution_count = 1, feature_count = maxFeatures)
  #res<-mRMR.classic(data = dd, target_indices = c(1),feature_count = maxFeatures)
  return(res)
}


#' plot de cada columna
plot_all<-function(x,y,r=3,c=5){
  rows<-nrow(data.num)
  par(mfrow=c(r,c))
  for(i in seq(ncol(x))){
    plot(x[,i], seq(1,rows), ylab = "index", xlab = colnames(x)[i],col=y)
  }
  par(mfrow=c(1,1))
}

#' dibuja los datos de una variable, el conjunto de test y junto con el ruido
#' devuelve finalmente el ruido filtrado (índices)
plot_one_var_and_noise<-function(x, ind_noise, test, col=1, nT=nrow(x), nB=0, nL=-50, nR=50, invert=F){
  par(mfrow=c(1,1))
  jpeg(file = paste("noise/noise",col,".jpg",sep=""))
  plot(x[-ind_noise,col], seq(1,nrow(x[-ind_noise,])), xlab = colnames(x)[col], ylab="Data Row Index")
  points(test[,col], seq(1,nrow(test)), col="blue")
  
  if(!invert)
    noise_ind_cut<-which(x[,col] >= nL & x[,col] <= nR)
  else
    noise_ind_cut<-which((x[,col] >= nL & x[,col] <= nR) | (x[,col] >= -nR & x[,col] <= -nL))
    
  noise_ind_cut_final<-ind_noise[ind_noise %in% nB:nT & ind_noise %in% noise_ind_cut]
  noise<-x[noise_ind_cut_final,col]
  points(noise, noise_ind_cut_final, col="red")
  legend("topright",c("Train", "Test", "Noise"), col = c("black", "blue", "red"), pch = c(16,16,16))
  dev.off()
  
  return(noise_ind_cut_final)
}

#' Devuelve un vector con los índices de ruido para cada columna del data.frame
noise_for_each_var<-function(data){
  data.no.y<-data
  data.no.y$y<-NULL
  data.y.factor<-data
  data.y.factor$y<-as.factor(data.y.factor$y)
  noise<-list()
  i<-1
  for(col in colnames(data.no.y)){
    formula<-formula(paste("y~",col,sep=""))
    out<-IPF(formula, data = data.y.factor, s = 2)
    noise[[i]]<-out$remIdx
    i<-i+1
  }
  return(noise)
}

clean_correlated_vars<-function(data, cutoff=0.75,maxVarsDel=0,ycol=ncol(data), plot=T, plot_method="pie", substr_colnames=T){
  colnames(data)<-if(substr_colnames) substr(colnames(data), 1, 20) else colnames(data)
  correlationMatrix <- cor(data_numeric(data[,-ycol]))
  corrplot::corrplot(correlationMatrix, method=plot_method)
  highlyCorrelated <- caret::findCorrelation(correlationMatrix , cutoff =cutoff) #4 correlaciones
  
  if(maxVarsDel<1|| maxVarsDel>length(highlyCorrelated)) #elimanos todas si es incorrecto el valor
    maxVarsDel <- length(highlyCorrelated)
    
  data.no.cor<-data[,-highlyCorrelated[1:maxVarsDel]]
  write.csv(data.no.cor, "data.no.cor.csv", row.names = F)
  return(list(clean=data.no.cor, index=highlyCorrelated))
}

#' Genera el archivo para subir a Kaggle con la predicción
submission<-function(pred, id=1){
  ids<-seq(1,length(pred))
  submission<-data.frame(Id=ids, Prediction=pred)
  write.csv(submission, paste("sub/submission",id,".csv",sep=""), row.names = F)
}

#' Compara dos versiones enviadas y devuelve cuantas instancias coincides
#' y cuantas difieren
compare_submissions<-function(x="sub/submission1.csv",y="sub/submission2.csv"){
  s1<-read.csv(x) 
  s2<-read.csv(y) 
  return(table(s1==s2))
}

generateBinaryModelrpart <- function(data,i,j,method="rpart",formula=formula(y~.)){
  dataset_i <- data[data$y == i,]
  dataset_j <- data[data$y == j,]
  dataset_i$y <- 1
  dataset_j$y <- 0
  dataset <- rbind(dataset_i, dataset_j)
  
  if(method=="knn")
    model <- train_model_form(formula, dataset, "knn")
  else
    model <- rpart(formula, dataset)
  
  return (model)
}

predict_1vs1 <- function(data, test,method="rpart",formula=formula(y~.),hiddenClass=F){
  needscale<-method=="knn"
  model_test<-NULL
  if(needscale){
    model_train<-model.frame(formula,data)
    data<-scale(model_train[-1])
    data<-cbind(data, model_train[1])
    
    if(!hiddenClass){
      model_test<-model.frame(formula, test)
      test<-scale(model_test[-1])
      test<-cbind(test, model_test[1])
    }
    else{
      test$y<-rep(1,nrow(test)) #y sintética para selección de features solo
      model_test<-model.frame(formula, test)
      test<-scale(model_test[-1])
      test<-cbind(test, model_test[1])
    }
  }
  
  model_0vs1 <- generateBinaryModelrpart(data,0,1,method,formula)
  model_0vs2 <- generateBinaryModelrpart(data,0,2,method,formula)
  model_0vs3 <- generateBinaryModelrpart(data,0,3,method,formula)
  model_1vs2 <- generateBinaryModelrpart(data,1,2,method,formula)
  model_1vs3 <- generateBinaryModelrpart(data,1,3,method,formula)
  model_2vs3 <- generateBinaryModelrpart(data,2,3,method,formula)
  
  predict_0vs1 <- as.integer(predict(model_0vs1,test))
  predict_0vs2 <- as.integer(predict(model_0vs2,test))
  predict_0vs3 <- as.integer(predict(model_0vs3,test))
  predict_1vs2 <- as.integer(predict(model_1vs2,test))
  predict_1vs3 <- as.integer(predict(model_1vs3,test))
  predict_2vs3 <- as.integer(predict(model_2vs3,test))
  
  votes_0 <- predict_0vs1 + predict_0vs2 + predict_0vs3
  votes_1 <- (1-predict_0vs1) + predict_1vs2 + predict_1vs3
  votes_2 <- (1-predict_0vs2) + (1-predict_1vs2) + predict_2vs3
  votes_3 <- (1-predict_0vs3) + (1-predict_1vs3) + (1-predict_2vs3)
  
  num_instances_test <- dim(test)[1]
  predictions <- vector(length = num_instances_test)
  
  for(i in 1:num_instances_test){
    votes <- c(votes_0[i],votes_1[i], votes_2[i], votes_3[i])
    max_index <- which.max(votes)
    predicted_class <- max_index - 1
    #cat(i,max_index)
    predictions[i] <- predicted_class
  }
  
  return (predictions)
}

createBinaryModel <- function(data,i,method="rpart",formula=formula("y~."),sampling="up"){
  dataset <- data
  y<-dataset[,"y"]
  y[data$y != i] <- 0 
  y[data$y == i] <- 1
  dataset$y <- y
  
  # if(i==0){
  #   data.y.factor<-dataset
  #   data.y.factor$y<-as.factor(data.y.factor$y)
  #   smote<-ubSMOTE(X=data.y.factor[,-ncol(data.y.factor)], Y=data.y.factor[,"y"], perc.over = 350, perc.under = 500)
  #   table(smote$Y)
  #   smote$Y<-as.integer(smote$Y)-1
  #   dataset<-cbind(smote$X, smote$Y)
  #   colnames(dataset)[ncol(dataset)]<-"y"
  # }
  
  
  if(method=="knn")
    model<-caret::knn3(formula, dataset,k=1)
    #model <- train_model_form(formula, dataset, "knn", sampling)
  else
    model <- rpart(formula, dataset)
  
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


plot_density<-function(data, var="V4", bins=30, hist_col="red", xlab="Linear Acc X", xlim=c(-5,5), ylim=c(0,2.1), plot_mean=F, mean_col="black"){
  p<-ggplot(data, aes_(x =  as.name(var))) + 
    geom_histogram(aes(y = ..density..), fill = hist_col, alpha = 0.5, bins = bins) + 
    geom_density(aes(linetype="Density",color="Density", size="Density"), show.legend=F) + xlab(xlab) + 
    ylab(expression(bold('Density'))) +
    xlim(xlim[1], xlim[2])  + ylim(ylim[1], ylim[2])
  if(plot_mean){
    p<-p+geom_vline(aes(xintercept=mean(V4, na.rm=T), linetype="Mean", color="Mean", size="Mean"), show.legend=F) +
        theme(legend.position="top", text = element_text(size=20)) #quitamos el titulo leyenda 
  }
  p<-p+scale_color_manual(name="", values = c(Density = "blue", Mean = mean_col)) +
    scale_linetype_manual(name="", values = c(Density = "solid", Mean = "dotted")) +
    scale_size_manual(name="", values=c(Density=0.5, Mean =0.8))
  return(p)
}



#https://datascienceplus.com/identify-describe-plot-and-removing-the-outliers-from-the-dataset/
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}

#' prep_methods: lista cuyas claves es el nombre del preprocesamiento a aplicar 
#' y el valor puede ser un vector o un unico valor con los parametros de este preprocesamiento
#' 
#' A los preprocesamientos "cor", "ipf", y "enn" como se trata de eliminar datos, 
#' si se añade un último parámetro, éste indicará la longitud máxima que deberá tomarse
preprocess_data_several_methods <- function(train_data, validation_data, fs_method="rf", model="rf", prep_methods=list(na=0, cor=0.75, ipf=0, enn=5, smote=c(60, 300), enn=5), total_inital_features=15, folds=5, reps=5, metric="acc", posClass=1, verbose=T){
  initial_features <- get_features_selected_given_weights(train_data, fs_method, model, total_features=total_inital_features)
  all_models <- list()
  deleted_vars <- c()
  deleted_subjets <- c()
  
  # cat(paste("Tunning model with preprocessing: none. "))
  # tuned_model <- get_tunning_model_given_weights(initial_features, data, model=model, folds=folds, reps=reps)
  # all_models[[length(all_models)+1]] <- tuned_model
  # cat(paste(". Accuracy:", tuned_model$result_eval$acc, ". F1-Score:", tuned_model$result_eval$f))
  # 
  for(i in seq(length(prep_methods))){
    prep_name <- names(prep_methods[i])
    cat(paste("\nTunning model with preprocessing: ", prep_name, ".", sep = ""))
    
    params <- prep_methods[[i]]
    result <- preprocess_data_one_method(train_data, prep_name, initial_features, params)
    
    if(result$applied_prep){
      if(prep_name == "cor"){
        deleted_vars <- c(deleted_vars, setdiff(colnames(train_data), colnames(result$data)))
        train_data <- result$data
        initial_features <- get_features_selected_given_weights(train_data, fs_method, model, total_features=total_inital_features)
      }
      else if(prep_name == "ipf" || prep_name == "enn"){
        deleted_subjets <- c(deleted_subjets, setdiff(rownames(train_data), rownames(result$data)))
      }
      
      train_data <- result$data
      
      
      set.seed(33)
      tuned_model <- get_tunning_model_given_weights(initial_features, train_data, model=model, folds=folds, reps=reps, metric=metric)
      res <- run_cv_prediction(initial_features, train_data, validation_data, model, tuned_model$bestParam)
      validation_met <- get_metrics(res$prediction, validation_data[,ncol(validation_data)], posClass = posClass) #get_tpr_tnr_auc_in_multiclass(unique(train$y), prediction, test_labels, verbose=verbose)
      
      all_models[[length(all_models)+1]] <- tuned_model
      cat(paste("\n   >> TRAIN Accuracy:", tuned_model$result_eval$acc, ". Precision:", tuned_model$result_eval$prec, ". Recall:", tuned_model$result_eval$rec, ". F1-Score:", tuned_model$result_eval$f, ". TPR:", tuned_model$result_eval$tpr, ". TNR:", tuned_model$result_eval$tnr), ". AUC:", tuned_model$result_eval$auc)
      cat(paste("\n   >> VALIDATION Accuracy:", validation_met$acc, ". Precision:", validation_met$prec, ". Recall:", validation_met$rec, ". F1-Score:", validation_met$f, ". TPR:", validation_met$tpr, ". TNR:", validation_met$tnr))
      
    }
    else{
      cat(" Not applied.")
    }
  }
  
  return(list(data=train_data, all_models=all_models, initial_features=initial_features, deleted_vars=deleted_vars, deleted_subjets=deleted_subjets))
}

preprocess_data_one_method <- function(data, prep_method="ipf", formula=y~., 
                                       params=if (prep_method=="cor") 0.75 #corr value
                                       else if (prep_method=="enn") 5 #k
                                       else if (prep_method=="smote") c(60, 300) #perc.over; perc.under
                                       else NULL, 
                                       verbose=T){
  set.seed(333)
  df.yfactor <- data
  df.yfactor$y <- as.factor(df.yfactor$y)
  applied_prep <- F
  
  if(verbose)
    cat("\n   >> Initial Class proportion:", paste(table(data$y), paste("(", names(table(data$y)), ")", sep=""), collapse = ", "))
  
  
  if(prep_method == "na" && total_na(data) > 0){ #impute na's
    data <- replace_missing_values(data, check_y_col = F)
    applied_prep <- T
  }
  else if(prep_method=="cor"){ #delete high correlated vars
    if(is.null(params))
      params = if(length(params) >= 1)  params[1] else  0.75
    clean<-clean_correlated_vars(data, params, plot_method="pie")
    indexes <- clean$index
    if(length(indexes) > 0){
      cutoff <-  if(length(params) > 1 && params[2]>0 && params[2] <= length(indexes)) indexes[seq(params[2])] else indexes

      if(verbose)
        cat("\n   >> Deleted correlated variables (", length(cutoff),"):", paste('"',colnames(data[, cutoff]),'"', sep = "", collapse = ", "))
      
      data<-data[,-cutoff]
      applied_prep <- T
    }
  }
  else if(prep_method=="ipf"){ #label noise
    out <- NoiseFiltersR::IPF(formula, data = df.yfactor, s = 2, consensus=T)
    indexes <- out$remIdx
    
    if(length(indexes) > 0){
      cutoff <-  if(length(params) > 1 && params[2] >0 && params[2] <= length(indexes)) indexes[seq(params[2])] else indexes#if(params > 0 && params <= length(indexes)) indexes[params] else indexes
      
      if(verbose){
        cat("\n   >> All labels noise (", length(indexes),"):", paste('"',rownames(data[indexes,]),'"', sep = "", collapse = ", "))
        cat("\n   >> Deleted labels noise(", length(cutoff),"):", paste('"',rownames(data[cutoff,]),'"', sep = "", collapse = ", "))
      }
      data <- data[-cutoff,]
      applied_prep <- T
    }
  }
  else if(prep_method=="enn"){
    if(is.null(params))
      params = 5
    p <- NoiseFiltersR::ENN(formula, k=5, data=df.yfactor, scale=T)
    indexes <- p$remIdx
    if(length(indexes) > 0){
      cutoff <-  if(length(params) > 1 && params[2] >0 && params[2] <= length(indexes)) indexes[seq(params[2])] else indexes
      
      if(verbose){
        cat("\n   >> All labels noise (", length(indexes),"):", paste('"',rownames(data[indexes,]),'"', sep = "", collapse = ", "))
        cat("\n   >> Deleted labels noise(", length(cutoff),"):", paste('"',rownames(data[cutoff,]),'"', sep = "", collapse = ", "))
      }
      
      data <- data[-cutoff,]
      applied_prep <- T
    }
    
  }
  else if(prep_method=="tomek"){
    ## Creating tomeklinks and removing the irrelevant datapoints
    if(!all(unique(data$y) %in% c(0, 1))){
      data$y <- ifelse(data$y==1, 1, 0)
    }
    
    tomek = unbalanced::ubTomek(data[,-ncol(data)], data$y)
    model_tomek = cbind(tomek$X,tomek$Y)
    names(model_tomek)[ncol(model_tomek)] = "y"
    indexes <- tomek$id.rm
    cutoff <-  if(params > 0 && params <= length(indexes)) indexes[seq(params)] else indexes
    
    if(all(unique(data$y) %in% c(0, 1))){
      data$y <- ifelse(data$y==1, 1, 2)
    }
    
    if(verbose){
      cat("\n   >> All labels noise (", length(indexes),"):", paste('"',rownames(data[indexes,]),'"', sep = "", collapse = ", "))
      cat("\n   >> Deleted labels noise(", length(cutoff),"):", paste('"',rownames(data[cutoff,]),'"', sep = "", collapse = ", "))
    }
    data <- data[-cutoff,]
    applied_prep <- T
  }
  else if(prep_method=="rose"){
    data <- ROSE(formula, data = data, seed = 1)$data
    applied_prep <- T
  }
  else if(prep_method=="smote"){
    if(is.null(params))
      params <- c(60, 300)
    data <- SMOTE(formula, df.yfactor, perc.over = params[1], perc.under = params[2], k=if(length(params) > 2) params[3] else 5)
    data$y <- as.integer(data$y)
    
    applied_prep <- T
  }
  else if(prep_method=="sbo"){
    if(is.null(params))
      params <- 100
    if(!all(unique(data$y) %in% c(0, 1))){
      data$y <- ifelse(data$y==1, 1, 0)
    }
    df.yfactor$y <- as.factor(data$y)
    
    data <- my_sbo(formula, df.yfactor, size=10, alg="rf", over = params, rf.ntree = 50, svm.ker = "radial")
    data <- data$best_train_data
    if(all(unique(data$y) %in% c(0, 1))){
      data$y <- ifelse(data$y==1, 1, 2)
    }
    applied_prep <- T
  }
  
  if(verbose && applied_prep)
      cat("\n   >> Class proportion:", paste(table(data$y), paste("(", names(table(data$y)), ")", sep=""), collapse = ", "))
  
  
  return(list(applied_prep=applied_prep, data=data))
}


constant_vars <- function(data, verbose=T){
  cols = c()
  for(i in seq(ncol(data))){
    if(length(levels(as.factor(data[, i])))<2)
      cols <- c(cols, i)
  }
  if(verbose){
    cat(paste("Column variables are constant:", paste(colnames(data)[cols], collapse = " ")))
  }
  return(cols)
}

delete_constant_vars <- function(data, verbose=T){
  res <- data
  c<-constant_vars(data, verbose)
  if(!is.null(c)){
    res <- data[, -c]
  }
  return(res)
}




my_sbo <- function (formula, data, size, alg, over = 100, rf.ntree = 50, 
          svm.ker = "radial") 
{
  target <- gsub(" ", "", unlist(strsplit(format(formula), 
                                          split = "~"))[1])
  list_model <- list()
  train_data <- list()
  a <- 0
  n <- data[which(data[, target] == "0"), ]
  p <- data[which(data[, target] == "1"), ]
  data$w <- rep(1/nrow(data), nrow(data))
  label <- data[, target]
  for (i in 1:size) {
    n <- data[which(data[, target] == "0"), ]
    f <- reformulate(paste(colnames(data)[which(colnames(data) != 
                                                  target & colnames(data) != "w")], collapse = "+"), 
                     response = target)
    smote <- DMwR::SMOTE(f, data = data, perc.over = over, 
                         perc.under = 0)
    train <- rbind(n, smote)
    train$w <- train$w/sum(train$w)
    train <- train[sample(nrow(train), nrow(train), replace = TRUE, 
                          prob = train$w), ]
    train$w <- NULL
    if (alg == "svm") {
      list_model[[i]] <- e1071::svm(formula, data = train, 
                                    kernel = svm.ker, probability = TRUE)
      prob <- as.data.frame(attr(predict(list_model[[i]], 
                                         data, probability = TRUE), "prob"))
    }
    else if (alg == "cart") {
      list_model[[i]] <- rpart::rpart(formula, data = train)
      prob <- as.data.frame(predict(list_model[[i]], data, 
                                    type = "prob"))
    }
    else if (alg == "c50") {
      list_model[[i]] <- C50::C5.0(formula, data = train)
      prob <- as.data.frame(predict(list_model[[i]], data, 
                                    type = "prob"))
    }
    else if (alg == "nb") {
      list_model[[i]] <- e1071::naiveBayes(formula, data = train)
      prob <- as.data.frame(predict(list_model[[i]], data, 
                                    type = "raw"))
    }
    else if (alg == "rf") {
      # print(formula)
      # print(train$y)
      list_model[[i]] <- randomForest::randomForest(formula, 
                                                    data = train, ntree = rf.ntree)
      prob <- as.data.frame(predict(list_model[[i]], data, 
                                    type = "prob"))
    }
    pred <- as.factor(ifelse(prob[, "1"] >= 0.5, 1, 0))
    # new <- ebmc::.wt.update(probability = prob, prediction = pred, 
    #                   actual = label, wt = data$w, smooth = 1/nrow(data))
    new <- mysbo_.wt.update(probability = prob, prediction = pred, 
                            actual = label, wt = data$w, smooth = 1/nrow(data))
    data$w <- new[[1]]
    a[i] <- new[[2]]
    train_data[[i]] <- as.data.frame(train)
  }
  result <- list(weakLearners = list_model, train_data=train_data, errorEstimation = a, best_train_data = train_data[[which.min(a)]])
  attr(result, "class") <- "modelBst"
  return(result)
}


mysbo_.wt.update <- function (probability, prediction, actual, wt, smooth) 
{
  fp <- which(ifelse(prediction == "1" & actual == "0", TRUE, 
                     FALSE) == TRUE)
  fn <- which(ifelse(prediction == "0" & actual == "1", TRUE, 
                     FALSE) == TRUE)
  p_loss <- 0.5 * sum(wt[fp] * (1 - probability[fp, ][, "0"] + 
                                  probability[fp, ][, "1"]), wt[fn] * (1 - probability[fn, 
                                                                                       ][, "1"] + probability[fn, ][, "0"]))
  a <- (p_loss + smooth)/(1 - p_loss + smooth)
  wt[c(fp, fn)] <- rep(1/(length(fp) + length(fn)), (length(fp) + 
                                                       length(fn)))
  wt[fn] <- wt[fn] * a^(0.5 * (1 + probability[fn, ][, "1"] - 
                                 probability[fn, ][, "0"]))
  wt[fp] <- wt[fp] * a^(0.5 * (1 + probability[fp, ][, "0"] - 
                                 probability[fp, ][, "1"]))
  wt <- wt/sum(wt)
  result <- list()
  result[[1]] <- wt
  result[[2]] <- a
  return(result)
}