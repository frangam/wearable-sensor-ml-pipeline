#------------------------------------------------------
# © 2020 Francisco M. Garcia-Moreno
#------------------------------------------------------

source("utils/utils.R")
source("utils/predict_fun.R")
source("utils/multiplot.R")
source("utils/sensor_utils.R")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

#------------------------------------------------------
# Hyperparameter tuning example
#------------------------------------------------------
set.seed(33)
if (Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="")


#------------------------------------------------------
# For testing purposes we will use the data generated
# from /examples/extract-features.R
# We can select a single configuration:
# - sampling rate = 0.04
# - window size = 0.5
#------------------------------------------------------
data <- df_smart_emp_sr0.04_w0.5

#we have to pass a validation data split
parts <- createDataPartition(data$y, p = 0.8, list = F)
validation_data <- data[-parts, ]
train_data <- data[parts, ]

#------------------------------------------------------
# Examples of hyperparameter tuning:
#------------------------------------------------------


#++++++
#sr: 0.04; wz: 0.5
# we test 3 models: Random forest ("rf"), SVM ("svm") and k-nn ("knn")
# evaluation metric = f1-score ("f1"); 10-fold (non-repeated)
#++++++
f1_1_1 <- search_features_selection(train_data, method="rf", model="rf", minfeatTune=1, metric="f1", search_method="rfe", folds=10, reps=1, verboseOnlyFeatSel=T, verbose=F)
f1_1_2 <- search_features_selection(train_data, method="rf", model="svm", minfeatTune=1, metric="f1", search_method="rfe", folds=10, reps=1, verboseOnlyFeatSel=T, verbose=F)
f1_1_3 <- search_features_selection(train_data, method="rf", model="knn", minfeatTune=1, metric="f1", search_method="rfe", folds=10, reps=1, verboseOnlyFeatSel=T, verbose=F)

#++++++
# plotting results
#++++++
plot_features_vs_acc_list_features(list(f1_1_1$all, f1_1_2$all, f1_1_3$all), metric="f1", tech=c("RF", "SVM", "k-NN"), lineBest=F, interv = 8)
plot_features_vs_acc_list_features(list(f1_1_3$all), metric="f1", tech=c("k-NN"), lineBest=F, interv = 8)
ggsave("../../../Datasets")
f1_1_3$all[f1_1_3$total_features]
f1_1_3$all[25]
length(f1_1_3$all)
model_knn<-model.frame(f1_1_3$features,data)
colnames(model_knn)


#------------------------------------------------------
# Validation
#------------------------------------------------------
features <- f1_1_3
  
res <- run_cv_prediction(features$features, train_data, validation_data, "knn", features$bestParam)
validation_met <- get_tpr_tnr_auc_in_multiclass(unique(train_data$y), res$prediction, validation_data$y) #get get_metrics(res$prediction, validation_data[,ncol(validation_data)], posClass = 1) #get_tpr_tnr_auc_in_multiclass(unique(train$y), prediction, test_labels, verbose=verbose)
cat(paste("\n   >> VALIDATION Accuracy:", validation_met$acc, ". Precision:", validation_met$prec, ". Recall:", validation_met$rec, ". F1-Score:", validation_met$f, ". TPR:", validation_met$tpr, ". TNR:", validation_met$tnr))

f1_1_3$all[f1_1_3$total_features]

