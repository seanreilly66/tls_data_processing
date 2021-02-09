
library(doParallel) # used for parallel processing
library(caret) # used for SVM tuning (takes advantage of doParallel)
library(tools)
library(kernlab)
library(dplyr)
library(tidyverse)

folder <- 'D:/Analyses/ladder_fuels_rf_uas_zeb_als_tls/'
files <- list.files(folder, pattern = '_210109.csv', full.names = T)

predict_outfile <- 'D:/Analyses/ladder_fuels_rf_uas_zeb_als_tls/ladder_fuel_rbr_predict_210123.csv'
stats_outfile <- 'D:/Analyses/ladder_fuels_rf_uas_zeb_als_tls/ladder_fuel_rbr_stats_210123.csv'

stats <- tibble(method=as.character(), mtry=as.numeric(), RMSE=as.numeric(),Rsquared=as.numeric(),MAE=as.numeric(),RMSESD=as.numeric(), RsquaredSD=as.numeric(),MAESD=as.numeric(),)

# response variable
response <- c('RBR_3x3avg')

predictors <-
  c(
    'ladder_fuel_1to2',
    'ladder_fuel_1to3'
  )


set.seed(111)
numfolds <- 10

for (file in files) {
  data <- read.csv(file, header = T) %>%
    filter(!is.na(ladder_fuel_1to2))
  
  method <- unlist(strsplit(file, '_'))[9]
  
  ctrl <- trainControl(method = "cv",
                       number = numfolds,
                       allowParallel = TRUE)
  
  # Random Forests
  rfTrain <- train(
    data[, predictors],
    data[, response],
    method = "rf",
    #preProcess = c("center", "scale"),
    trControl = ctrl,
    ntree = 1000,
    tuneLength = 10,
    metric = "RMSE",
    allowParallel = TRUE,
    na.action = na.omit
  )
  
  mtry <- rfTrain$bestTune[1]
  rfStats <-
    rfTrain$results[which(rfTrain$results$mtry == as.numeric(mtry)),]
  
  rfStats <- rfStats %>%
    add_column(method, .before = 1)
  
  rfStats$method <- method
  
  stats <- stats %>%
    add_row(rfStats)
  
  csv_select <- data %>%
    select(ladder_fuel_1to2,
           ladder_fuel_7to8,
           ladder_fuel_1to3)
  
  
  predict <- predict(rfTrain, csv_select)
 
   rbr_data <- data %>%
    select(plot, RBR_3x3avg ) %>%
    rename('rbr_actual' = 'RBR_3x3avg')
  
  rbr_data$rbr_predict <- predict
  
  base <- paste(tools::file_path_sans_ext(file), "_", response, sep = '')
  predict_outfile <- paste('D:/Analyses/ladder_fuels_rf_uas_zeb_als_tls/',base, "_rf_predict.csv", sep = "")
  write.csv(rbr_data, predict_outfile)
  
}

write_csv(stats,stats_outfile)


