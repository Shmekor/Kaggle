kaggle.africa.generalModel.train <- function(ds, sFeatures, tAll, tMethod='lasso', tControl=NULL) {
  
  trainModel <- function(formula, tDS) {
    model <- NULL
    if(!is.null(tControl)) {
      model <- train(formula, method=tMethod, data=tDS, trainControl=tControl)
    } else {
      model <- train(formula, method=tMethod, data=tDS)
    }
    print(getTrainPerf(model))
    
    model
  }
  
  result <- list()
  print('train Ca')
  result$caModel <- trainModel(Ca ~ ., ds[,c(sFeatures, 'Ca')])
  print('train P')
  result$pModel <- trainModel(P ~ ., ds[,c(sFeatures, 'P')])
  print('train pH')
  result$pHModel <- trainModel(pH ~ ., ds[,c(sFeatures, 'pH')])
  print('train SOC')
  result$socModel <- trainModel(SOC ~ ., ds[,c(sFeatures, 'SOC')])
  print('train Sand')
  result$sandModel <- trainModel(Sand ~ ., ds[,c(sFeatures, 'Sand')])
  
  result
}

kaggle.africa.generalModel.predict <- function(model, ds) {
  result <- data.frame(PIDN=ds$PIDN)
  result$Ca <- predict(model$caModel, ds)
  result$P <- predict(model$pModel, ds)
  result$pH <- predict(model$pHModel, ds)
  result$SOC <- predict(model$socModel, ds)
  result$Sand <- predict(model$sandModel, ds)
  
  result
}