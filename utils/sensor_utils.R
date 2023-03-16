#------------------------------------------------------
# © 2020 Francisco M. Garcia-Moreno
#------------------------------------------------------
require(psych)
#------------
#Functions
#------------
ALL_EVENTS <- list(start=1, standup=2, walkingToStore=3, inEntrance=4, walkingToCorrectSection=5, pickUpItem=6, walkingToCheckout=7,
                   inTheCheckout=8, inItsTurn=9, goToTheExit=10, inTheExit=11, comingBack=12, inTheStartPoint=13, end=14)
CRUCIAL_EVENTS <- c(ALL_EVENTS$start, ALL_EVENTS$standup, ALL_EVENTS$walkingToStore, ALL_EVENTS$walkingToCorrectSection, ALL_EVENTS$pickUpItem
                    , ALL_EVENTS$walkingToCheckout, ALL_EVENTS$inTheCheckout, ALL_EVENTS$inItsTurn, ALL_EVENTS$comingBack)
events <- function(){
  return(list(start=1, standup=2, walkingToStore=3, inEntrance=4, walkingToCorrectSection=5, pickUpItem=6, walkingToCheckout=7,
              inTheCheckout=8, inItsTurn=9, goToTheExit=10, inTheExit=11, comingBack=12, inTheStartPoint=13, end=14))
}
key_events <- function(){
  e <- events()
  return(c(e$walkingToStore, e$walkingToCorrectSection, e$pickUpItem
           , e$walkingToCheckout, e$comingBack))
}
CRUCIAL_EVENTS <- function(){
  e <- events()
  return(c(e$start, e$standup, e$walkingToStore, e$walkingToCorrectSection, e$pickUpItem
           , e$walkingToCheckout, e$inTheCheckout, e$inItsTurn, e$comingBack))
}
first_walking_event <- function(){
  e <- events()
  return(e$walkingToStore)
}
first_3_events <- function(){
  e <- events()
  return(c(e$start, e$standup, e$walkingToStore))
}
ALL_DISTANCES <- list(cerrillo=85, pts=130, zaidin=210)
TIME_IN_SEMAPHORES <- list(cerrillo=0, pts=40, zaidin=15)

statisticsID <- function(){
  # return(c("n","mean", "sd", "median", "min", "max", "range", "skew", "kurtosis", "se"))
  return(c("mean", "sd", "min", "max", "range", "skew", "kurtosis"))
  # return(c("mean", "sd"))
}
sensorVecIDS <- function(){
  return(c(
    "es.ugr.frailty.accelerometer",
                  "es.ugr.frailty.gravity",
                  "es.ugr.frailty.gyroscope",
                  "es.ugr.frailty.linearacceleration"))
}
sensorIDs <- function(){
  return(c(
    "es.ugr.frailty.accelerometer",
               "es.ugr.frailty.gravity",
               "es.ugr.frailty.gyroscope",
               "es.ugr.frailty.heartrate",
               # "es.ugr.frailty.hrgreenlight",
               # "es.ugr.frailty.pressure",
               # "es.ugr.frailty.light",
               "es.ugr.frailty.linearacceleration"
               ,"EDA"
               ,"TEMP"
              )
           # ,"DecibelXData")
# "es.ugr.frailty.pedometer")
        )
}
smartwatchSensorIDS <- function(){
  return(c(
      "es.ugr.frailty.accelerometer",
             "es.ugr.frailty.gravity",
             "es.ugr.frailty.gyroscope",
             "es.ugr.frailty.heartrate",
             # "es.ugr.frailty.hrgreenlight",
             # "es.ugr.frailty.pressure",
             # "es.ugr.frailty.light",
             "es.ugr.frailty.linearacceleration"))
}
inertialSmartwatchSensorIDS <- function(){
  return(c(
    "es.ugr.frailty.accelerometer",
    # "es.ugr.frailty.gravity",
    "es.ugr.frailty.gyroscope",
    "es.ugr.frailty.heartrate"
    # "es.ugr.frailty.hrgreenlight",
    # "es.ugr.frailty.pressure",
    # "es.ugr.frailty.light",
    # "es.ugr.frailty.linearacceleration"
    ))
}
empaticaSensorIDS <- function(){
  return(c("EDA", "TEMP"))
}
allEmpaticaSensorIDS <- function(){
  return(c("EDA", "TEMP", "ACC", "BVP", "HR", "IBI"))
}
SMARTWATCH_TRIAXIAL_SENSORS_COL_NAMES <- c("gyr.x", "gyr.y", "gyr.z", "linacc.x", "linacc.y", "linacc.z", "gra.x", "gra.y", "gra.z")
TRIAXIAL_SENSORS_COL_NAMES <- c("acc.x", "acc.y", "acc.z", "gyr.x", "gyr.y", "gyr.z", "linacc.x", "linacc.y", "linacc.z", "gra.x", "gra.y", "gra.z")
SMARTWATCH_COL_NAMES <- c(TRIAXIAL_SENSORS_COL_NAMES, "hr"
                          # , "light", "hrgreenlight", "pressure"
                          )
EMPATICA_COL_NAMES <- c("eda", "temp")



#' Calcula the speed of the event
#' @param distancesList list with distances depending on the Center
#' @return a list with (vel=velocity, dist=distance, time=with the time)
calculate_speed_of_event <- function(patientID, distancesList, semaphores, time){
  distance<-get_distance_by_patient_code(patientID, distancesList)
  semaphore_time <- get_time_in_semaphores_by_patient_code(patientID, semaphores)
  finalTime<-time - semaphore_time
  vel<-distance/as.integer(finalTime)
  return(list(vel=vel, dist=distance, time=finalTime))
}

get_distance_by_patient_code <- function(patientID, distancesList){
  res <- 0
  if(grepl("CM",patientID)){
    res<-distancesList$cerrillo
  }
  else if(grepl("E",patientID)){
    res<-distancesList$pts
  }
  else if(grepl("Z",patientID)){
    res<-distancesList$zaidin
  }
  return(res)
}

get_time_in_semaphores_by_patient_code <- function(patientID, semaphores){
  res <- 0
  if(grepl("CM",patientID)){
    res<-semaphores$cerrillo
  }
  else if(grepl("E",patientID)){
    res<-semaphores$pts
  }
  else if(grepl("Z",patientID)){
    res<-semaphores$zaidin
  }
  return(res)
}

get_ids_list_of_wearable_data <- function(data){
  ids<-c()
  for(id in strsplit(rownames(data), "_")){
    i<-id[1]
    if(!(i%in%ids)){
      ids <- c(ids, i)
    }
  }
  return(ids)
}

get_new_col_considering_participantID_with_events <- function(data_with_events, data_without_events, col_to_add_name="y"){
  new_col_values <- c()
  data_without_events_axu <- data.frame(id=rownames(data_without_events), sel_col=data_without_events[, colnames(data_without_events) == col_to_add_name])
  for(id in strsplit(rownames(data_with_events), "_")){
    i<-id[1]
    new_col_values <- c(new_col_values, data_without_events_axu[data_without_events_axu$id == i, "sel_col"])
  }
 
  return(new_col_values)
}

add_column_from_other_data <- function(data, other_data, col_other_data_name, new_col_name){
  other_col <- data.frame(id=get_ids_list_of_wearable_data(data), c=other_data[, col_other_data_name])
  
  print(paste("Adding column:", col_other_data_name, "with name:", new_col_name))
  
  new_col_values <- c()
  for(id in strsplit(rownames(data), "_")){
    i<-id[1]
    # print(i)
    new_col_values <- c(new_col_values, other_col[other_col$id == i, "c"])
    # print(new_col)
  }
  data[new_col_name] <- new_col_values
  return(data)
}





require(xts)

# create_all_wearable_datasets <- function(subjects_excluded, devices=c("smartwatch_empatica", "smartwatch", "empatica"), 
#                                          windows_sizes_in_sec = c(0.5, 1, 1.5, 2, 2.5, 3), 
#                                          sampling_rates = c(0.04, 0.02, 0.1)){
#   for(dev in devices){
#     cat("\n>> Device:", dev, "\n")
#     for(sr in sampling_rates){
#       for(wz in windows_sizes_in_sec){
#         create_complete_wearable_dataset_with_windows_seconds(preprocessed_files_dir = paste("Datasets/Shopping/wearables_data/preprocessed/", dev, "/matrix/", sep = ""),
#                                                               save_filename=paste("Datasets/Shopping/wearables_data/preprocessed/", dev, "_wearable_data", sep = ""), 
#                                                               subjects_excluded, windows_size_in_sec = wz, sampling_rate = sr)
#       }
#     }
#   }
# }

#'@param split_events Si es True separamos por eventos las ventanas, 
#'con lo cual tendremos un numero de filas igual al numero de eventos por cada participante
complete_dataset_extracting_features <- function(avoid_participant_ids=c(), 
                                                 preprocessed_files_dir="Datasets/Shopping/wearables_data/preprocessed/",
                                                 save_filename="Datasets/Shopping/wearables_data/preprocessed/",
                                                 devices=c("smartwatch_empatica", "smartwatch", "empatica"),
                                                 windows_sizes_in_sec = c(0.1, 0.5, 1, 1.5, 2, 2.5, 3, 5), 
                                                 sampling_rates = c(0.04, 0.02, 0.1), 
                                                 avoid_windos_sizes_in_sr=list(),
                                                 avoid_events=c(), #c(1,2,3,4,10,11,12,13,14),
                                                 basic_stats=statisticsID(), 
                                                 last_col_is_for_event=T,
                                                 split_events=F){
  
  for(dev in devices){
    sr_i <- 0
    for(sr in sampling_rates){
      for(wz in windows_sizes_in_sec){
        sr_i <- sr_i + 1
        if(!(length(avoid_windos_sizes_in_sr) >= sr_i && wz %in% avoid_windos_sizes_in_sr[[sr_i]])){
          if(split_events){
            win_indexes_direname <- paste(preprocessed_files_dir, dev, "/windowed_indexes/split_events/sr_",sr,"S/w_",wz,"/", sep="")
          }
          else{
            win_indexes_direname <- paste(preprocessed_files_dir, dev, "/windowed_indexes/non-split_events/sr_",sr,"S/w_",wz,"/", sep="")
          }
          participant_files = list.files(win_indexes_direname)
          print(win_indexes_direname)
          sr_w_data <- data.frame()
          row_names <- c()
          col_names <- c()
          for (p_file in participant_files){
            file_spl <- unlist(strsplit(p_file, "\\."))
            p_id <- file_spl[1]
  
            if(!(p_id %in% avoid_participant_ids)){
              cat("\n-------------------------------------------------------------------")
              cat(paste("\nExtracting features. Subject id: ", p_id, " (sr: ", sr,"s ", 1/sr, "Hz; window: ", wz, "s)", sep=""))
              cat("\n-------------------------------------------------------------------")
              
              wearable_data_dirname <- paste(preprocessed_files_dir, dev, "/matrix/",p_id,"_sr_",sr,"S.csv", sep="")
              d <- read.csv(wearable_data_dirname, header=T, stringsAsFactors = F)
              w_indexes <- read.csv(paste(win_indexes_direname, p_id, ".csv", sep=""), header=T, stringsAsFactors = F, row.names = "X")
              features <- data.frame()
              
              #extraer features de cada ventana
              for (w in 1:nrow(w_indexes)){
                row <- w_indexes[w,]
                the_event <- row[3]
                if(!(the_event %in% avoid_events)){
                  # print(row)
                  # w_spl <- unlist(strsplit(w, "\\:"))
                  w_start_i <- as.numeric(row[1]) #as.numeric(w_spl[1])+1 #+1 porque el preprocesamiento en python empezó con indice 0
                  w_end_i <- as.numeric(row[2])#as.numeric(w_spl[2])+1
                  w_event <- if(length(row) > 2) as.numeric(row[3]) else 0#if(length(w_spl)>2) as.numeric(w_spl[3]) else 0
                  window_samples <- d[seq(w_start_i, w_end_i),]
                  # window_samples$X<-NULL
                  window_samples$timestamp<-NULL
                  feat_extracted <- wearables_feature_extraction(window_samples, basic_stats, last_col_is_for_event, add_event_col = w_event!=0, w_event)
                  features <- rbind(features, feat_extracted)
                }
              }
              
              # print(features)
              
              if(split_events){
                names(features) <- names(feat_extracted)
                events <- unique(features$event)
                for(ev in events){
                  if(ev>=ALL_EVENTS$start && ev<=ALL_EVENTS$end){ #asegurar que el evento es correcto
                    features_p_window <- features[features$event == ev,]
                    cur_features <- apply(features_p_window, 2, mean)
                    
                    all_ts <- as.POSIXct(d[d$event == ev,]$timestamp, origin="1970-01-01")
                    ev_time <- as.numeric(difftime(all_ts[length(all_ts)], all_ts[1], units="secs"))
                    ev_speed <- 0
                    ev_distance <- 0
                    
                    if(ev==ALL_EVENTS$walkingToStore || ev==ALL_EVENTS$comingBack){ 
                      ev_speed_list <- calculate_speed_of_event(p_id, ALL_DISTANCES, TIME_IN_SEMAPHORES, ev_time)
                      ev_time <- ev_speed_list$time
                      ev_speed <- ev_speed_list$vel
                      ev_distance <- ev_speed_list$dist
                    }
                    
                    sr_w_data <- rbind(sr_w_data, c(cur_features, ev_time, ev_speed, ev_distance))
                    row_names <- c(row_names, paste(p_id, ev, sep="_"))
                  }
                }
                col_names <-c(names(feat_extracted), "event_time", "event_speed", "supermarket_distance")
              }
              else{
                #hacemos la media de cada columna, resultando una unica fila por individuo
                features <- apply(features, 2, mean)
                
                #-----------------------------------------------------------------------
                #extraer variables temporales y de velocidad según eventos etiquetados
                #-----------------------------------------------------------------------
                t1<-as.POSIXct(d[d$event == 1,]$timestamp, origin="1970-01-01")[1]
                t2<-as.POSIXct(d[d$event == 2,]$timestamp, origin="1970-01-01")[1]
                t3<-as.POSIXct(d[d$event == 3,]$timestamp, origin="1970-01-01")[1]
                t4<-as.POSIXct(d[d$event == 4,]$timestamp, origin="1970-01-01")[1]
                t5<-as.POSIXct(d[d$event == 5,]$timestamp, origin="1970-01-01")[1]
                t6<-as.POSIXct(d[d$event == 6,]$timestamp, origin="1970-01-01")[1]
                t7<-as.POSIXct(d[d$event == 7,]$timestamp, origin="1970-01-01")[1]
                t8<-as.POSIXct(d[d$event == 8,]$timestamp, origin="1970-01-01")[1]
                t9<-as.POSIXct(d[d$event == 9,]$timestamp, origin="1970-01-01")[1]
                t10<-as.POSIXct(d[d$event == 10,]$timestamp, origin="1970-01-01")[1]
                t11<-as.POSIXct(d[d$event == 11,]$timestamp, origin="1970-01-01")[1]
                t12<-as.POSIXct(d[d$event == 12,]$timestamp, origin="1970-01-01")[1]
                t13<-as.POSIXct(d[d$event == 13,]$timestamp, origin="1970-01-01")[1]
                t14<-as.POSIXct(d[d$event == 14,]$timestamp, origin="1970-01-01")[1]
                tlast<-as.POSIXct(d[d$event == 14,]$timestamp, origin="1970-01-01")[length(as.POSIXct(d[d$event == 14,]$timestamp, origin="1970-01-01"))]
                
                time_1 <- as.numeric(difftime(t2, t1, units="secs"))
                time_2 <- as.numeric(difftime(t3, t2, units="secs"))
                time_3 <- as.numeric(difftime(t4, t3, units="secs"))
                time_4 <- as.numeric(difftime(t5, t4, units="secs"))
                time_5 <- as.numeric(difftime(t6, t5, units="secs"))
                time_6 <- as.numeric(difftime(t7, t6, units="secs"))
                time_7 <- as.numeric(difftime(t8, t7, units="secs"))
                time_8 <- as.numeric(difftime(t9, t8, units="secs"))
                time_9 <- as.numeric(difftime(t10, t9, units="secs"))
                time_10 <- as.numeric(difftime(t11, t10, units="secs"))
                time_11 <- as.numeric(difftime(t12, t11, units="secs"))
                time_12 <- as.numeric(difftime(t13, t12, units="secs"))
                time_13 <- as.numeric(difftime(t14, t13, units="secs"))
                time_14 <- as.numeric(difftime(tlast, t14, units="secs"))
                
                goToStoreTime <- time_3 #as.numeric(difftime(t4, t3, units="secs"))
                comebackTime <- time_12 #as.numeric(difftime(t13, t12, units="secs"))
                correctSectionTime <- time_5 #as.numeric(difftime(t6, t5, units="secs"))
                
                vel_gotostore <- calculate_speed_of_event(p_id, ALL_DISTANCES, TIME_IN_SEMAPHORES, goToStoreTime)
                vel_comeback <- calculate_speed_of_event(p_id, ALL_DISTANCES, TIME_IN_SEMAPHORES, comebackTime)
                distance<-get_distance_by_patient_code(p_id, ALL_DISTANCES)
                
                
                # features <- c(features, vel_gotostore$vel, vel_comeback$vel, correctSectionTime, distance)
                
                #establecer colnames y rownames
                # col_names <- c(names(feat_extracted), "gotostore.speed", "comeback.speed", "timetoget.correctsection", "distance")
                
                features <- c(features, distance, vel_gotostore$vel, vel_comeback$vel, time_1, time_2, time_3, time_4, time_5, time_6, time_7, time_8, time_9, time_10, time_11, time_12, time_13, time_14)
                col_names <- c(names(feat_extracted), "supermarket_distance", "gotostore.speed", "comeback.speed", paste("time_", ALL_EVENTS, sep = ""))
                
                names(features) <- col_names
                sr_w_data <- rbind(sr_w_data, features)
                row_names <- c(row_names, p_id)
              }
            }
          }
          
          if(length(sr_w_data)>0){
            rownames(sr_w_data) <- row_names
            colnames(sr_w_data) <- col_names
            
            add_events <- if(split_events) "/split_events" else "/non-split_events"
            
            write.csv(sr_w_data, paste(save_filename, dev, "/features_extracted", add_events, "/features_extracted_sr_",sr,"S_wz_", wz,"s", ".csv", sep=""), row.names = T)
            print("File saved")
          }
          else{
            print("File not saved")
          }
        }
      }
    }
  }
}

load_complete_dataset_with_features_extracted <- function(questionnaires, 
                                                 preprocessed_files_dir="Datasets/Shopping/wearables_data/preprocessed/",
                                                 save_filename="Datasets/Shopping/wearables_data/preprocessed/",
                                                 devices=c("smartwatch_empatica", "smartwatch", "empatica"),
                                                 windows_sizes_in_sec = c(0.1, 0.5, 1, 1.5, 2, 2.5, 3, 5), 
                                                 sampling_rates = c(0.04, 0.05, 0.02), basic_stats=statisticsID(), 
                                                 split_events=F,
                                                 last_col_is_for_event=T, y_col_name = "", new_cols=c(), del_cols=c()){
  row_names <- c()
  col_names <- c()
  dfs <- list()
  
  for(dev in devices){
    sr_data <- list()
    for(sr in sampling_rates){
      sr_w_data <- list()
      windows_sizes_in_sec_file_exists <- c()
      for(wz in windows_sizes_in_sec){
        add_events <- if(split_events) "/split_events" else "non-split_events"
        filename <- paste(preprocessed_files_dir, dev,"/features_extracted/", add_events, "/features_extracted_sr_",sr,"S_wz_", wz,"s", ".csv", sep="")
        print(filename)
        if(file.exists(filename)){
          windows_sizes_in_sec_file_exists[length(windows_sizes_in_sec_file_exists)+1] <- wz
          
          all.sensor.data<-read.csv(filename, header=T, row.names = "X")

          # if(all.sensor.data)
          # all.sensor.data <- read.csv(paste("Datasets/Shopping/wearables_data/preprocessed/", dev, "/windowed/",wz,"_sr_",sr,".csv",sep=""), header=T, row.names = "X")
          all.sensor.data <- all.sensor.data[order(rownames(all.sensor.data)),]
          
          patientsIDS<-get_ids_list_of_wearable_data(all.sensor.data)
          finalIDS <- c()
          new_patientIDS <- NULL
          
          #filter only patients in questionnaires
          for(p_id in patientsIDS){
            if(p_id %in% rownames(questionnaires)){
              if(split_events){
                p_id <- paste(p_id, ALL_EVENTS, sep="_")
              }
              finalIDS <- c(finalIDS, p_id)
            }
          }
          all.sensor.data <- all.sensor.data[order(rownames(all.sensor.data)), ]
          all.sensor.data <- all.sensor.data[rownames(all.sensor.data)%in%finalIDS, ]
          all.sensor.data <- all.sensor.data[order(rownames(all.sensor.data)), ]
          patientsIDS<-get_ids_list_of_wearable_data(all.sensor.data)
          
          
          if(split_events){
            finalIDS <- c()
            
            #delete patients with any missing events data
            for(p_id in patientsIDS){
                id_with_ev_list <- paste(p_id, ALL_EVENTS, sep="_")
                rows_num <- nrow(all.sensor.data[rownames(all.sensor.data) %in% id_with_ev_list,]) 
                # print(id_with_ev_list)
                # print(rows_num)
                
                if(rows_num == length(ALL_EVENTS)){
                  finalIDS <- c(finalIDS, id_with_ev_list)
                }
            }
            # print(patientsIDS)
            # print(finalIDS)
            # print(length(finalIDS))
            
            all.sensor.data <- all.sensor.data[order(rownames(all.sensor.data)), ]
            all.sensor.data <- all.sensor.data[rownames(all.sensor.data)%in%finalIDS, ]
            all.sensor.data <- all.sensor.data[order(rownames(all.sensor.data)), ]
            patientsIDS<-get_ids_list_of_wearable_data(all.sensor.data)
            # print(paste("'", patientsIDS, "'", collapse = ",", sep = ""))
            # print(length(patientsIDS))
          }
          
          print("Dim sensor dataset")
          print(dim(all.sensor.data))
          
          # print(paste("'", patientsIDS, "'", collapse = ",", sep = ""))
          print("Patient IDs")
          print(patientsIDS)
          # print(length(rownames(questionnaires)%in%patientsIDS))
          # print(rownames(questionnaires))
          questionnaires <- questionnaires[order(rownames(questionnaires)), ]
          questionnaires <- questionnaires[rownames(questionnaires)%in%patientsIDS, ]
          questionnaires <- questionnaires[order(rownames(questionnaires)), ]
          print("Dim questionnaires")
          print(dim(questionnaires))
          
          #eliminamos columnas
          if(length(del_cols)>0){
            for(c in del_cols){
              if(c%in%colnames(all.sensor.data)){
                all.sensor.data[c] <- NULL
                # colnames(all.sensor.data)[c] <- NULL
              }
            }
          }
          
          #agregamos mas columnas al dataset de sensores
          if(length(new_cols)>0){
            # cols_names <- c()
            for(c in new_cols){
              if(c%in%colnames(questionnaires)){
                # all.sensor.data[c] <- questionnaires[c]
                # cols_names <- c(cols_names, c)
                all.sensor.data <- add_column_from_other_data(all.sensor.data, questionnaires, c, c)
              }
            }
            # # print(colnames(all.sensor.data) )
            # # print(cols_names)
            # # colnames(all.sensor.data) <- c(all.sensor.data, cols_names)
            # 
          }
          # data.clean <- all.sensor.data
          # print(dim(all.sensor.data))
          # print(dim(questionnaires))
          
          if(y_col_name != ""){
            data.clean <- add_column_from_other_data(all.sensor.data, questionnaires, y_col_name, "y")
          }
          # cat("\nClass proportion:", paste(table(data.clean$y), paste("(", names(table(data.clean$y)), ")", sep=""), collapse = ", "))
          # ggplot(data.clean, aes(y)) + geom_bar(fill = "#4CA8E4")
  
          #Eliminamos variables que son constantes
          zv <- apply(data.clean, 2, function(x) length(unique(x)) == 1)
          data.clean<-data.clean[,!zv]
          
          sr_w_data[[length(sr_w_data)+1]] <- data.clean
          
        }#_end_if(file.exists(filename))
      }
      if(!is.null(windows_sizes_in_sec_file_exists)){
        names(sr_w_data) <- paste(windows_sizes_in_sec_file_exists, "s", sep="")
        sr_data[[length(sr_data)+1]] <- sr_w_data 
      }
    }
    if(!is.null(windows_sizes_in_sec_file_exists)){
      names(sr_data) <- paste(sampling_rates, "S", sep="")
      dfs[[length(dfs)+1]] <- sr_data
    }  
  }
  names(dfs) <- devices
  return(dfs)
}

# create_complete_wearable_dataset_with_windows_seconds <- function(avoid_participant_ids=c(), events=c(100), preprocessed_files_dir = "Datasets/Shopping/wearables_data/preprocessed/matrix/",
#                                                                   windows_indexes_files = "Datasets/Shopping/wearables_data/preprocessed/windowed/",
#                                                                   save_filename="Datasets/Shopping/wearables_data/preprocessed/",
#                                                                   windows_size_in_sec =0.5, sampling_rate=0.04, total_w_overlapping_samples=0.5, use_total_sample_as_window=F, basic_stats=statisticsID(), last_col_is_for_event=T){
#   participant_files = list.files(preprocessed_files_dir)
#   all_data <- data.frame()
#   col_names <- c()
#   row_names <- c()
#   
#   
#   for (p_file in participant_files){
#     file_spl <- unlist(strsplit(p_file, "\\_"))
#     p_id <- file_spl[1]
#     sr <- as.numeric(unlist(strsplit(file_spl[2], "S.csv")))
#     
#     if(!(p_id %in% avoid_participant_ids) && !is.na(sr) && sr==sampling_rate){
#       d <- read.csv(paste(preprocessed_files_dir, p_file, sep=""), header=T)
#       d$timestamp <- as.POSIXlt(as.character(d$timestamp))
#       # windows_indexes <- endpoints(xts::xts(d[,-1], d[,1]), "milliseconds", windows_size_in_sec*1000)
#       windows_indexes <- df$X
#       
#       
#       print("-------------------------------------------------------------------")
#       print(paste("Extracting features. Subject id: ", p_id, " (windows: ", length(windows_indexes),")", sep=""))
#       print("-------------------------------------------------------------------")
#       # print(windows_indexes)
#       features <- data.frame()
#       # prev_start_index <- 1
#       for (i in seq(length(windows_indexes))){
#         # start_index <- if(i==1) 1 else floor(((windows_indexes[i]+1)-prev_start_index) * total_w_overlapping_samples)
#         # prev_start_index <- start_index
#         # last_index <- windows_indexes[i+1]
#         if(i != length(windows_indexes) - 1)
#           last_index <- last_index + 1
#         
#         # print(start_index)
#         # print(last_index)
#         
#         # print(paste("Extracting features from window:", i))
#         window_samples <- d[start_index:last_index, -1] #seleccionamos los samples de la ventana #quitamos la columna timestamp
#         feat_extracted <- wearables_feature_extraction(window_samples, basic_stats, last_col_is_for_event)
#         features <- rbind(features, feat_extracted)
#       }
#       colnames(features) <- names(feat_extracted)
#       col_names <- colnames(features)
#       features <- apply(features, 2, mean)
#       all_data <- rbind(all_data, features)
#       row_names <- c(row_names, p_id)
#       
#     }# end_if(!(p_id %in% avoid_participant_ids))
#   }
#   print(paste("total rows:",nrow(all_data)))
#   
#   colnames(all_data) <- col_names
#   rownames(all_data) <- row_names
#   
#   print("Saving file...")
#   write.csv(all_data, paste(save_filename,"_w_", windows_size_in_sec,"_sr_",sampling_rate, ".csv", sep=""), row.names = T)
#   print("File saved")
# }

# create_complete_wearable_dataset_with_windows_samples <- function(avoid_participant_ids=c(), events=c(100), preprocessed_files_dir = "Datasets/Shopping/wearables_data/preprocessed/matrix/",
#                                              save_filename="Datasets/Shopping/wearables_data/preprocessed/wearable_data",
#                                              total_w_samples =512, total_w_overlapping_samples=total_w_samples/2, use_total_sample_as_window=F, basic_stats=statisticsID(), last_col_is_for_event=T){
#   participant_files = list.files(preprocessed_files_dir)
#   all_data <- data.frame()
#   col_names <- c()
#   row_names <- c()
#   first_time <- T
#   total_windows <- 0
#   last_all_data_rows <- 0
#   last_length_rows <- 0
#   rows_per_win <- 0
#   
#   for (p_file in participant_files){
#     p_id <- unlist(strsplit(p_file, "\\."))[1]
#     if(!(p_id %in% avoid_participant_ids)){
#       d <- read.csv(paste(preprocessed_files_dir, p_file, sep=""), header=T)
#       windows_n <- floor(nrow(d)/total_w_samples)
#       use_total_sample_as_window <- windows_n == 0
#       total_windows <- total_windows + windows_n
#       
#       print("-------------------------------------------------------------------")
#       print(paste("Extracting features. Subject id: ", p_id, " (windows: ", windows_n,")", sep=""))
#       print("-------------------------------------------------------------------")
#       
#       
#       start_samples_index <- 1
#       end_samples_index <- min(total_w_samples, nrow(d))
#       window_samples <- d[seq(start_samples_index, end_samples_index), ] #seleccionamos los samples de la ventana
#       feat_extracted <- wearables_feature_extraction(window_samples, basic_stats, last_col_is_for_event)
#       
#       all_data <- rbind(all_data, feat_extracted) #extraemos la caracteristicas de esta ventana
#       
#       rows_per_win <- nrow(all_data) - last_all_data_rows
#       last_all_data_rows <- nrow(all_data)
#       # print(paste("total rows for window 1:", rows_per_win))
#       if(rows_per_win > 1){
#         print(feat_extracted)
#       }
#       
#       if(first_time){
#         col_names <- names(feat_extracted)
#         # print(col_names)
#         first_time <- F
#       }
#      
#       if(!use_total_sample_as_window){
#         # siguientes ventanas (produciendo overlapping cada total_w_overlapping_samples [256, en general] samples)
#         for(window_i in seq(2, windows_n)){
#           start_samples_index <- start_samples_index + total_w_overlapping_samples
#           end_samples_index <- start_samples_index + (total_w_samples - 1)
#           window_samples <- d[seq(start_samples_index, end_samples_index), ] #seleccionamos los samples de la ventana
#           windows_data <- wearables_feature_extraction(window_samples, basic_stats, last_col_is_for_event)
#           all_data <- rbind(all_data, windows_data) #extraemos la caracteristicas de esta ventana
#           rows_per_win <- nrow(all_data) - last_all_data_rows
#           last_all_data_rows <- nrow(all_data)
#           # print(paste("total rows for window", window_i, ":", rows_per_win))
#           if(rows_per_win > 1){
#             print(windows_data)
#           }
#         } 
#         row_names <- c(row_names, paste(p_id,"_w", seq(windows_n), sep = ""))
#         cur_length_row_names <- length(row_names) - last_length_rows
#         last_length_rows <- length(row_names)
#         if(cur_length_row_names != windows_n){
#           print(paste("different rows length in window", window_i))
#           print(paste("total rows in", window_i, ":", cur_length_row_names))
#         }
#       }
#       else{
#         row_names <- c(row_names, paste(p_id,"_w1", sep = ""))
#       }
#   
#       
#     }# end_if(!(p_id %in% avoid_participant_ids))
#   }
#   print(paste("total windows:",total_windows))
#   print(paste("total rows:",nrow(all_data)))
#   
#   colnames(all_data) <- col_names
#   rownames(all_data) <- row_names
#   
#   print("Saving file...")
#   if(use_total_sample_as_window){
#     write.csv(all_data, paste(save_filename, "_unique_window.csv", sep=""), row.names = T)
#   }
#   else{
#     write.csv(all_data, paste(save_filename, total_w_samples, ".csv", sep=""), row.names = T)
#   }
#   print("File saved")
# }

#' Devuelve un vector de caracteristicas extraidas de un conjunto de samples
#' indicado en el data.frame data
#' @param add_event_col True para incluir la columna evento como feature
wearables_feature_extraction <- function(data, basic_stats=statisticsID(), last_col_is_for_event=T, add_event_col=F, event=1){
  stats <- as.data.frame(psych::describe(data))
  stats<-stats[seq(length(stats$vars)-1), basic_stats] #quitamos la var "event" y selesccionamos las basic_stats que queremos
  data.values <- c()
  
  # if(!(sensorID %in% sensorVecIDS())){
  #   row.names(stats) <- paste(sensor_name, if(want_all_events) "all.events" else paste("event",event,sep="."), sep = ".")
  # }
  
  #construimos las variables convertidas en estaticas (mediante diferentes medidas estadísticas)
  for(i in 1:nrow(stats)){
    for(j in 1:ncol(stats)){
      var.name <- paste(rownames(stats)[i],".",colnames(stats)[j],sep="")
      # print(var.name)
      val <- stats[i,j]
      if(is.na(val) | is.nan(val)){
        val <- 0
      }
      
      data.values <- c(data.values, val)
      names(data.values)[length(data.values)] <- var.name
    }
  }
  
  all_valid_cols <- if(last_col_is_for_event) colnames(data)[-ncol(data)] else colnames(data)
  
  # Average Mean over 3 axes sensors
  data.values <- get_average_of_a_stat(data.values, "mean")
  # Average Standard Deviation over 3 axes
  data.values <- get_average_of_a_stat(data.values, "sd")
  # Average Skewness over 3 axes
  data.values <- get_average_of_a_stat(data.values, "skew")
  # Average Kurtosis over 3 axes
  data.values <- get_average_of_a_stat(data.values, "kurtosis")
  # Average minimum over 3 axes
  data.values <- get_average_of_a_stat(data.values, "min")
  # Average maximum over 3 axes
  data.values <- get_average_of_a_stat(data.values, "max")
  # Average range over 3 axes
  data.values <- get_average_of_a_stat(data.values, "range")
 
   # Energy value for each axis (x, y, and z)
  for(c in all_valid_cols){
    energy <- sum(base::Mod(stats::fft(data[, c]))^2) / nrow(data)
    data.values <- c(data.values, energy)
    names(data.values)[length(data.values)] <- paste(c, "energy", sep=".")
  }
  
  # Average Energy over 3 axes
  data.values <- get_average_of_a_stat(data.values, "energy")
  
  # Correlations: x_y, x_z, x_total, y_z, y_total, z_total
  for (c in seq(1, length(TRIAXIAL_SENSORS_COL_NAMES), 3)){
    sensor_name <- unlist(strsplit(TRIAXIAL_SENSORS_COL_NAMES[c], "\\."))[1]
    sel_cols <- TRIAXIAL_SENSORS_COL_NAMES[seq(c, c+2)]
    if(sel_cols %in% colnames(data)){
      #Creamos tres variables nuevas sobre la correlación de ejes
      correlationXY <- stats::cor(data[, sel_cols[1]], data[, sel_cols[2]])
      correlationXZ <- stats::cor(data[, sel_cols[1]], data[, sel_cols[3]])
      correlationYZ <- stats::cor(data[, sel_cols[2]], data[, sel_cols[3]])
      data.values <- c(data.values, correlationXY, correlationXZ, correlationYZ)
      # if(is.na(correlationXY) || is.na(correlationXZ) || is.na(correlationYZ)){
      #   print(paste("X:", data[, sel_cols[1]], ", Y:", data[, sel_cols[2]], ", Z:", data[, sel_cols[3]]))
      #   print(paste("XY:", correlationXY, ", XZ:", correlationXZ, ", YZ:", correlationYZ))
      # }
      names(data.values)[seq(length(data.values)-2, length(data.values))] <- paste(sensor_name, c("corr.xy", "corr.xz", "corr.yz"), sep=".")
    }
  }
  
  if(add_event_col){
    data.values <- c(data.values, event)
    names(data.values)[length(data.values)] <- "event"
  }
  
  return(data.values)
}

get_average_of_a_stat <- function(data, stat_name){
  mean_triaxial_cols <- built_col_name_of_basic_stats(TRIAXIAL_SENSORS_COL_NAMES, stat_name)
  
  for (c in seq(1, length(mean_triaxial_cols), 3)){
    sensor_name <- unlist(strsplit(mean_triaxial_cols[c], "\\."))[1]
    sel_cols <- mean_triaxial_cols[seq(c, c+2)]
    if(sel_cols %in% colnames(data)){
      data <- c(data, mean(data[sel_cols]))
      names(data)[length(data)] <- paste(sensor_name, paste("avg", stat_name, sep = "."), sep=".")
    }
  }
  return(data)
}

built_col_name_of_basic_stats <- function(cols, basic_stats){
  vars_rep_by_each_stat <- rep(cols, length(basic_stats))
  vars_rep_by_each_stat <- vars_rep_by_each_stat[order(vars_rep_by_each_stat)]
  return(paste(vars_rep_by_each_stat, rep(basic_stats, length(cols)), sep="."))
}

get_sensor_values_for_an_event <- function(patientID, sensorIDs, event=-1){
  single.sensor.data <- c()
  for(sensorID in sensorIDs){
    if(!(sensorID %in% smartwatchSensorIDS()) & !(event < events()$start | event >events()$end)){
      break
      # print(paste(sensorID,"event:",event))
    }
    d<-NULL
    fix.id <- "acc"
    
    if(sensorID %in% empaticaSensorIDS()){
      # print(patientID)
      # print(sensorID)
      d<-read.csv(paste("Datasets/Shopping/wearables_data/",patientID,"/empatica/",sensorID,".csv",sep=""), header=F)
      d<-as.data.frame(d[-seq(2),]) #quitamos las 2 primeras filas
    }
    else{
      if(sensorID %in% smartwatchSensorIDS()){
        d<-read.csv(paste("Datasets/Shopping/wearables_data/",patientID,"/",sensorID,".csv",sep=""), header=F)
        d<-as.data.frame(d[,-c(1,2,3)]) #quitamos las colunas de timestamp, date y time
      }
      else{ #sensor de ruido
        d<-read.csv(paste("Datasets/Shopping/wearables_data/",patientID,"/",sensorID,".csv",sep=""), header=F, dec = ".", skip = 10)
        d<-as.data.frame(d[, -2]) #quitamos la ultima columna y las 9 primeras filas
      }
    }
    # print(head(d))
    
    stats_index <- c(4)
    
    if(sensorID == "es.ugr.frailty.heartrate"){
      colnames(d) <-c("heart.rate", "event")
      sensor_name <- "hr"
    }
    else if(sensorID == "es.ugr.frailty.hrgreenlight"){
      colnames(d) <- c("hrgreenlight", "event")
      sensor_name <- "hrgreenlight"
    }
    else if(sensorID == "es.ugr.frailty.light"){
      colnames(d) <- c("light", "event")
      sensor_name <- "light"
    }
    else if(sensorID == "es.ugr.frailty.pressure"){
      colnames(d) <- c("pressure", "event")
      sensor_name <- "pressure"
    }
    else if(sensorID == "es.ugr.frailty.accelerometer"){
      colnames(d) <- c("acc.x", "acc.y", "acc.z", "event")
      sensor_name <- "acc"
    }
    else if(sensorID == "es.ugr.frailty.gravity"){
      colnames(d) <- c("gra.x", "gra.y", "gra.z", "event")
      sensor_name <- "gra"
    }
    else if(sensorID == "es.ugr.frailty.gyroscope"){
      colnames(d) <- c("gyr.x", "gyr.y", "gyr.z", "event")
      sensor_name <- "gyr"
    }
    else if(sensorID == "es.ugr.frailty.linearacceleration"){
      colnames(d) <- c("linacc.x", "linacc.y", "linacc.z", "event")
      sensor_name <- "linacc"
    }
    # else if(sensorID == "ACC"){
    #   colnames(d) <- c("acc.empatica.x", "acc.empatica.y", "acc.empatica.z")
    #   sensor_name <- "acc.empatica"
    # }
    else{
      colnames(d) <- c(sensorID)
      sensor_name <- sensorID
    }
    
    if(sensorID%in%smartwatchSensorIDS()){
      #Seleccionar solo parte del data frame filtrando por la columna event
      daux<-d[d$event==event,]
      want_all_events <- nrow(daux) == 0 #si el numero indicado es mayor al ultimo evento es que queremos todos los datos
      
      # print(sensorID)
      # print(want_all_events)
      # print(nrow(daux))
    }
    else{
      want_all_events <- event<events()$start | event> events()$end
    }
    
    if(!want_all_events){
      d <- daux
    }
    
    colnames(d) <- paste(colnames(d), if(want_all_events) "all.events" else paste("event", event, sep="."), sep = ".")
    
    #incluimos columna magnitud vectorial solo en los sensores tipo vector: sqrt(sum(x^2)) 
    if(sensorID %in% sensorVecIDS()){
      # stats_index <- c(4,5,6)
      
      magnitude <- apply(d[,seq(ncol(d)-1)], 1, function(x){sqrt(sum(x^2))})
      d <- cbind(d, magnitude)
      
      colnames(d)[ncol(d)] <- paste(sensor_name, colnames(d)[ncol(d)], if(want_all_events) "all.events" else paste("event",event,sep="."), sep = ".")
      
      # #quitamos ya los ejes x, y, z, solo mantenemos el evento y la magnitud calculada
      # d <- d[,-seq(3)]
      
      
      # print(paste(sensor_name, "magnitude", sep = "."))
      # print(colnames(d)[ncol(d)])
    }
    # else{
    #   stats_index <- c(4)
    # }
    
    # print(colnames(d))
    #nos quedamos con las variables numéricas
    stats_index <- unlist(lapply(d, is.numeric)) 
    if(sensorID%in%smartwatchSensorIDS()){
      #seleccionamos columna del evento de la compra
      col_to_del <- if(sensorID %in% sensorVecIDS()) -(length(stats_index)) else (length(stats_index))
      stats_index[col_to_del] <- F #quitamos la penultima columna que aun siendo numerica no la queremos (columna event)
    }
    stats <- as.data.frame(psych::describe(d))
    stats<-stats[stats_index, statisticsID()]
    data.values <- c()
    
    if(!(sensorID %in% sensorVecIDS())){
        row.names(stats) <- paste(sensor_name, if(want_all_events) "all.events" else paste("event",event,sep="."), sep = ".")
    }
    
    #construimos las variables convertidas en estaticas (mediante diferentes medidas estadísticas)
    for(i in 1:nrow(stats)){
      for(j in 1:ncol(stats)){
        var.name <- paste(rownames(stats)[i],".",colnames(stats)[j],sep="")
        # print(var.name)
        val <- stats[i,j]
        if(is.na(val) | is.nan(val)){
          val <- 0
        }
          
        data.values <- c(data.values, val)
        names(data.values)[length(data.values)] <- var.name
      }
    }
    
    
    window <- nrow(d)
    last_index <- 1
    prev_names <- c()
    #Creamos mas variables para los sensores uniaxiales
    if(!(sensorID %in% sensorVecIDS())){
      variance <- stats::var(d[,1])
      fftMagnitude <- sum(base::Mod(stats::fft(d[,ncol(d)]))^2) / window
      data.values <- c(data.values, variance, fftMagnitude)
      last_index <- 1
      prev_names <- c("variance", "fftMagnitude")
    }
    #Creamos mas variables para los sensores triaxiales
    else{
      # varianceX <- stats::var(d[,1])
      # varianceY <- stats::var(d[,2])
      # varianceZ <- stats::var(d[,3])
      variance <- stats::var(d[,ncol(d)]) #varianza de la magnitud
      # fftX <- sum(base::Mod(stats::fft(d[,1]))^2) / window
      # fftY <- sum(base::Mod(stats::fft(d[,2]))^2) / window
      # fftZ <- sum(base::Mod(stats::fft(d[,3]))^2) / window
      fftMagnitude <- sum(base::Mod(stats::fft(d[,ncol(d)]))^2) / window
      
      #Creamos tres variables nuevas sobre la correlación de ejes
      correlationXY <- stats::cor(d[,1],d[,2])
      correlationXZ <- stats::cor(d[,1],d[,3])
      correlationYZ <- stats::cor(d[,2],d[,3])
      
      last_index <- 4
      
      data.values <- c(data.values, variance, correlationXY, correlationXZ, correlationYZ, fftMagnitude)
      prev_names <- c("variance", "correlationXY", "correlationXZ", "correlationYZ", "fftMagnitude")
    }
    names(data.values)[seq(length(data.values)-last_index, length(data.values))] <- paste(sensor_name, prev_names, sep=".")
    selected_cols <- names(data.values)[seq(length(data.values)-last_index, length(data.values))]
    names(data.values)[seq(length(data.values)-last_index, length(data.values))] <- paste(selected_cols, if(want_all_events) "all.events" else paste("event",event, sep="."), sep = ".")
    
    
    
    # print(names(data.values))
    single.sensor.data <- c(single.sensor.data, data.values)
    # print(names(single.sensor.data))
  }
  return(single.sensor.data)
}

#' Creates the full wearable dataset for all patients identifies given
#' @param patientsIDs patients identifiers list
#' @param events_split  wearable data by each event (100 or != c(1:14) indicates consider all events too)
construct_sensor_dataset_all_patients <- function(patientsIDs, events_split = c(100, unlist(events(), use.names = F)), sensorIDs, generateNAs=T, questionnaires=NULL){
  all.sensor.data <- c()
  all_events <- events()
  all_distances <- store_distances()
  all_timeInsemaphores <- timeInsemaphores()
  first_patient <- T
  wearable_variable_names <- read.csv(paste("Datasets/Shopping/wearables_data/preprocessed/wearable_variable_names.csv",sep=""), stringsAsFactors = F, header=T)
  wearable_variable_names <- unlist(sapply(wearable_variable_names, as.character))
  no_patient_data_IDs <- c()
  
  for(patientID in patientIDs){
    all_single.sensor.data <- c()
    if(!dir.exists(paste("Datasets/Shopping/wearables_data/",patientID,"/",sep = ""))){
      #creamos missing values para los participantes que no hicieron la prueba pero hicieron otros tests
      print(paste("Sin wearable data para", patientID))
      if(generateNAs){
        all_single.sensor.data <- rep(NA, length(wearable_variable_names))
        names(all_single.sensor.data) <- wearable_variable_names
        print(paste("NA's a insertar:",total_na(all_single.sensor.data)))
      }
      
      # print(head(all_single.sensor.data))
      
      no_patient_data_IDs[length(no_patient_data_IDs)+1] <- patientID
     
    }
    else{
      for(event in events_split){ 
        single.sensor.data <- get_sensor_values_for_an_event(patientID, sensorIDs, event=event)
        # print(single.sensor.data)
        all_single.sensor.data <- c(all_single.sensor.data, single.sensor.data)
      }
      

      #---------------------------------------------------------------------
      #integramos variables de tiempo para diferentes eventos y velocidades
      #---------------------------------------------------------------------
      times <- read.csv(paste("Datasets/Shopping/wearables_data/",patientID,"/es.ugr.frailty.timemark.csv",sep=""), header=F)
      colnames(times) <- c("time", "event")
      t1<-as.POSIXct(times[times$event == all_events$start,]$time / 1000, origin="1970-01-01")
      t2<-as.POSIXct(times[times$event == all_events$standup,]$time / 1000, origin="1970-01-01")
      t3<-as.POSIXct(times[times$event == all_events$walkingToStore,]$time / 1000, origin="1970-01-01")
      t4<-as.POSIXct(times[times$event == all_events$inEntrance,]$time / 1000, origin="1970-01-01")
      t5<-as.POSIXct(times[times$event == all_events$walkingToCorrectSection,]$time / 1000, origin="1970-01-01")
      t6<-as.POSIXct(times[times$event == all_events$pickUpItem,]$time / 1000, origin="1970-01-01")
      t7<-as.POSIXct(times[times$event == all_events$walkingToCheckout,]$time / 1000, origin="1970-01-01")
      t8<-as.POSIXct(times[times$event == all_events$inTheCheckout,]$time / 1000, origin="1970-01-01")
      t9<-as.POSIXct(times[times$event == all_events$inItsTurn,]$time / 1000, origin="1970-01-01")
      t10<-as.POSIXct(times[times$event == all_events$goToTheExit,]$time / 1000, origin="1970-01-01")
      t11<-as.POSIXct(times[times$event == all_events$inTheExit,]$time / 1000, origin="1970-01-01")
      t12<-as.POSIXct(times[times$event == all_events$comingBack,]$time / 1000, origin="1970-01-01")
      t13<-as.POSIXct(times[times$event == all_events$inTheStartPoint,]$time / 1000, origin="1970-01-01")
      t14<-as.POSIXct(times[times$event == all_events$end,]$time / 1000, origin="1970-01-01")
      
      #Segundos que ha tardado en realizar la prueba completa
      all_single.sensor.data[length(all_single.sensor.data)+1] <- secs<-difftime(t14, t11, units="secs")
      names(all_single.sensor.data)[length(all_single.sensor.data)] <- "total.time"
      
      #segundos en llegar a la tienda
      goToStoreTime <- difftime(t4, t3, units="secs")
      v <- calculate_speed_of_event(patientID, all_distances, all_timeInsemaphores, goToStoreTime)
      all_single.sensor.data[length(all_single.sensor.data)+1] <- v$dist
      names(all_single.sensor.data)[length(all_single.sensor.data)] <- "go.to.store.distance"
      all_single.sensor.data[length(all_single.sensor.data)+1] <- goToStoreTime
      names(all_single.sensor.data)[length(all_single.sensor.data)] <- "go.to.store.time"
      all_single.sensor.data[length(all_single.sensor.data)+1] <- v$vel
      names(all_single.sensor.data)[length(all_single.sensor.data)] <- "go.to.store.speed"
      
      #segundos en encontrar el producto
      all_single.sensor.data[length(all_single.sensor.data)+1] <- difftime(t6, t4, units="secs")
      names(all_single.sensor.data)[length(all_single.sensor.data)] <- "go.to.correct.section.time"
      
      #segundos cogiendo el producto
      all_single.sensor.data[length(all_single.sensor.data)+1] <- difftime(t7, t6, units="secs")
      names(all_single.sensor.data)[length(all_single.sensor.data)] <- "pickup.item.time"
      
      #segundos en llegar a la caja
      all_single.sensor.data[length(all_single.sensor.data)+1] <- difftime(t8, t7, units="secs")
      names(all_single.sensor.data)[length(all_single.sensor.data)] <- "go.to.checkout.time"
      
      #segundos llegar a su turno
      all_single.sensor.data[length(all_single.sensor.data)+1] <- difftime(t9, t8, units="secs")
      names(all_single.sensor.data)[length(all_single.sensor.data)] <- "waiting.turn.time"
      
      #segundos llegar al final
      comingbackTime <- difftime(t13, t12, units="secs")
      v <- calculate_speed_of_event(patientID, all_distances, all_timeInsemaphores, comingbackTime)
      all_single.sensor.data[length(all_single.sensor.data)+1] <- comingbackTime
      names(all_single.sensor.data)[length(all_single.sensor.data)] <- "comingback.time"
      all_single.sensor.data[length(all_single.sensor.data)+1] <- v$vel
      names(all_single.sensor.data)[length(all_single.sensor.data)] <- "comingback.speed"
      
      if(first_patient){
        first_patient <- FALSE
        write.csv(names(all_single.sensor.data), paste("Datasets/Shopping/Shopping/wearables_data/preprocessed/wearable_variable_names.csv", sep=""), row.names = F)
      }
    }#end_else of if(!dir.exists(paste("Datasets/Shopping/wearables_data/",patientID)))
    
    all.sensor.data <- rbind(all.sensor.data, all_single.sensor.data)
  }
  all.sensor.data <- as.data.frame(all.sensor.data)
  if(generateNAs){
    rownames(all.sensor.data) <- patientIDs
    
    if(total_na(all.sensor.data)>0){
      print("Reemplazando NA's")
      questionnaires <- questionnaires[order(rownames(questionnaires)),]
      all.sensor.data.aux <- cbind(questionnaires, all.sensor.data)
      colnames(all.sensor.data.aux) <- c(colnames(questionnaires), colnames(all.sensor.data))
      rownames(all.sensor.data.aux) <- rownames(all.sensor.data)
      all.sensor.data <- all.sensor.data.aux
      all.sensor.data <- replace_missing_values(all.sensor.data, check_y_col = F, sensors = T)
    }
  }
  else{
    rownames(all.sensor.data) <- patientIDs[!(patientIDs%in%no_patient_data_IDs)]
  }
  
  # print(head(all.sensor.data[rownames(all.sensor.data) %in% no_patient_data_IDs,]))
  
  
  
  write.csv(all.sensor.data, paste("Datasets/Shopping/wearables_data/preprocessed/wearable_data.csv", sep=""), row.names = T)
  write.csv(rownames(all.sensor.data), paste("Datasets/Shopping/wearables_data/preprocessed/wearable_data_patientIDS.csv", sep=""), row.names = F)
  
  return(no_patient_data_IDs)
}


pack_events_into_one <- function(data, all_event_ids=unique(data$event)
                                 , patientsIDS=get_ids_list_of_wearable_data(data)
                                 , pack_events_ids=c(ALL_EVENTS$inEntrance, ALL_EVENTS$walkingToCorrectSection
                                                     , ALL_EVENTS$pickUpItem, ALL_EVENTS$walkingToCheckout
                                                     , ALL_EVENTS$inTheCheckout, ALL_EVENTS$inItsTurn
                                                     , ALL_EVENTS$goToTheExit, ALL_EVENTS$inTheExit)
                                 , sufix_pack_col = "shopping"){
  rest_events <- all_event_ids[!(all_event_ids %in% pack_events_ids)]
  df_res <- data.frame()
  
  for(p_id in patientsIDS){
    #eventos no comprimidos
    if(length(rest_events) > 0 )
      df_res <-  rbind(df_res, data[rownames(data) %in% paste(p_id, rest_events, sep="_"),])
    #comprimir eventos (p.e. todos los de la compra)
    y<-data[paste(p_id, pack_events_ids[1], sep="_"),ncol(data)]
    r <- c(apply(data[paste(p_id, pack_events_ids, sep="_"),-ncol(data)], 2 , mean), y)
    df_res <- rbind(df_res, r)
    r_name <- paste(p_id, sufix_pack_col, sep = "_")
    rownames(df_res) <- c(rownames(df_res)[-nrow(df_res)], r_name)
  }
  colnames(df_res) <- colnames(data)
  
  return(df_res)
}
