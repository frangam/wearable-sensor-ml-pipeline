#------------------------------------------------------
# © 2020 Francisco M. Garcia-Moreno
#------------------------------------------------------

source("utils/utils.R")
source("utils/predict_fun.R")
source("utils/multiplot.R")
source("utils/sensor_utils.R")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

#------------------------------------------------------
# Extract features and generate dataset windowed
#------------------------------------------------------

# Load questionnaire dataset (where we have the target label CLASS - frail, pre-frail, non-frail)
questionnaires_frailty<-read.csv("Datasets/questionnaires/preprocessed/all_questionnaires_frailty.csv", row.names = "X")
questionnaires_frailty <- questionnaires_frailty[questionnaires_frailty$Edad>=65,]
subjects_excluded <- rownames(questionnaires_frailty[questionnaires_frailty$Edad<65,])

sampling_rates<-c(0.1, 0.01, 0.06, 0.08, 0.05, 0.15, 0.25, 0.5) #desired sampling rates
windows_sizes_in_sec<-c(0.5, 1, 1.5, 2, 2.5, 3) #desired window sizes

# Generate the wearable dataset
complete_dataset_extracting_features(subjects_excluded, devices=c("smartwatch_empatica"),
                                     split_events = T, # split_events: True if we have a column "event" where we have labelled some known events
                                     windows_sizes_in_sec = c(1, 1.5, 2, 2.5, 3),
                                     sampling_rates = )

#------------------------------------------------------
# Load generated dataset (whitout event column)
#------------------------------------------------------
dfs <- load_complete_dataset_with_features_extracted(questionnaires_frailty, devices=c(
                                                    "smartwatch_empatica"
                                                    ),
                                                     windows_sizes_in_sec = windows_sizes_in_sec,
                                                     sampling_rates = sampling_rates,
                                                     split_events = split_events,
                                                     y_col_name = "fried_frailty_risk", new_cols=c(),
                                                     del_cols=c(
                                                         built_col_name_of_basic_stats(c("acc", "gyr", "gra", "linacc"), c("corr.xy", "corr.xz", "corr.yz"))
                                                       # , built_col_name_of_basic_stats(c("pres", "lig", "noise"), c(statisticsID(), "energy"))
                                                       , built_col_name_of_basic_stats(c("pres"), c(statisticsID(), "energy"))
                                                       , built_col_name_of_basic_stats(c("lig"), c(statisticsID(), "energy"))
                                                       , built_col_name_of_basic_stats(c("noise"), c(statisticsID(), "energy"))
                                                       
                                                       , built_col_name_of_basic_stats(c("linacc.x", "linacc.y", "linacc.z"), c(statisticsID(), "energy"))
                                                       , built_col_name_of_basic_stats(c("gra.x", "gra.y", "gra.z"), c(statisticsID(), "energy"))
                                                       # built_col_name_of_basic_stats(SMARTWATCH_TRIAXIAL_SENSORS_COL_NAMES, c(statisticsID(), "energy")),
                                                       # , built_col_name_of_basic_stats(c("eda"), c(statisticsID(), "energy"))
                                                       # , built_col_name_of_basic_stats(c("temp"), c(statisticsID(), "energy"))
                                                       # , built_col_name_of_basic_stats(c("hr"), c(statisticsID(), "energy"))
                                                       # , built_col_name_of_basic_stats(c("gyr.x", "gyr.y", "gyr.z"), c(statisticsID(), "energy"))
                                                       #
                                                       # , built_col_name_of_basic_stats(c("acc.x", "acc.y", "acc.z"), c(statisticsID(), "energy"))
                                                       # , built_col_name_of_basic_stats(c("acc.x"), c(statisticsID(), "energy"))
                                                       # , built_col_name_of_basic_stats(c("acc.y"), c(statisticsID(), "energy"))
                                                       # , built_col_name_of_basic_stats(c("acc.z"), c(statisticsID(), "energy"))
                                                       
                                                       
                                                       

                                                       )
)


#------------------------------------------------------
# Example: Prepare only data for EMPATICA E4 device
# for the different sampling-rates and window-sizes configurations
#------------------------------------------------------
dfs_smart_emp <- dfs$smartwatch_empatica


#Sampling rate = 1/0.1 = 10Hz
df_smart_emp_sr0.1_w0.5 <- dfs_smart_emp$`0.1S`$`0.5s`
df_smart_emp_sr0.1_w1 <- dfs_smart_emp$`0.1S`$`1s`
df_smart_emp_sr0.1_w1.5 <- dfs_smart_emp$`0.1S`$`1.5s`
df_smart_emp_sr0.1_w2 <- dfs_smart_emp$`0.1S`$`2s`
df_smart_emp_sr0.1_w2.5 <- dfs_smart_emp$`0.1S`$`2.5s`
df_smart_emp_sr0.1_w3 <- dfs_smart_emp$`0.1S`$`3s`

#Sampling rate = 1/0.06 = ~15Hz
df_smart_emp_sr0.06_w0.5 <- dfs_smart_emp$`0.06S`$`0.5s`
df_smart_emp_sr0.06_w1 <- dfs_smart_emp$`0.06S`$`1s`
df_smart_emp_sr0.06_w1.5 <- dfs_smart_emp$`0.06S`$`1.5s`
df_smart_emp_sr0.06_w2 <- dfs_smart_emp$`0.06S`$`2s`
df_smart_emp_sr0.06_w2.5 <- dfs_smart_emp$`0.06S`$`2.5s`
df_smart_emp_sr0.06_w3 <- dfs_smart_emp$`0.06S`$`3s`

#Sampling rate = 1/0.04 = 25Hz
df_smart_emp_sr0.04_w0.5 <- dfs_smart_emp$`0.04S`$`0.5s`
df_smart_emp_sr0.04_w1 <- dfs_smart_emp$`0.04S`$`1s`
df_smart_emp_sr0.04_w1.5 <- dfs_smart_emp$`0.04S`$`1.5s`
df_smart_emp_sr0.04_w2 <- dfs_smart_emp$`0.04S`$`2s`
df_smart_emp_sr0.04_w2.5 <- dfs_smart_emp$`0.04S`$`2.5s`
df_smart_emp_sr0.04_w3 <- dfs_smart_emp$`0.04S`$`3s`

#Sampling rate = 1/0.02 = 50Hz
df_smart_emp_sr0.02_w0.5 <- dfs_smart_emp$`0.02S`$`0.5s`
df_smart_emp_sr0.02_w1 <- dfs_smart_emp$`0.02S`$`1s`
df_smart_emp_sr0.02_w1.5 <- dfs_smart_emp$`0.02S`$`1.5s`
df_smart_emp_sr0.02_w2 <- dfs_smart_emp$`0.02S`$`2s`
df_smart_emp_sr0.02_w2.5 <- dfs_smart_emp$`0.02S`$`2.5s`
df_smart_emp_sr0.02_w3 <- dfs_smart_emp$`0.02S`$`3s`

#Sampling rate = 1/0.01 = 100Hz
df_smart_emp_sr0.01_w0.5 <- dfs_smart_emp$`0.01S`$`0.5s`
df_smart_emp_sr0.01_w1 <- dfs_smart_emp$`0.01S`$`1s`
df_smart_emp_sr0.01_w1.5 <- dfs_smart_emp$`0.01S`$`1.5s`
df_smart_emp_sr0.01_w2 <- dfs_smart_emp$`0.01S`$`2s`
df_smart_emp_sr0.01_w2.5 <- dfs_smart_emp$`0.01S`$`2.5s`
df_smart_emp_sr0.01_w3 <- dfs_smart_emp$`0.01S`$`3s`


#------------------------------------------------------
# For testing the library:
# We can select a single configuration:
# - sampling rate = 0.04
# - window size = 0.5
#------------------------------------------------------
data <- df_smart_emp_sr0.04_w0.5
