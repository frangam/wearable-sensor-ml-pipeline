#------------------------------------------------------
# © 2020 Francisco M. Garcia-Moreno
#------------------------------------------------------

source("utils/utils.R")
source("utils/predict_fun.R")
source("utils/multiplot.R")
source("utils/sensor_utils.R")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

#------------------------------------------------------
# Preprocessing example
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
# Examples of preprocessing pipelines:
#------------------------------------------------------


# -- Feature selection: Random forest ("rf")
# -- Classifier: K-Nearest Neighbors ("knn")
# -- Evaluation metric: F1-Score ("f1")
# Preprocessing methods in this order:
# -- 1) impute missing values with 0 (na=0)
# -- 2) smote for balancing data (up-60; down-300)
prep_models <- preprocess_data_several_methods(train_data, validation_data, 
	fs_method="rf", model="knn", metric = "f1",
    prep_methods=list(na=0, smote=c(60, 300)),
    folds=5, reps=1, verbose=T)
