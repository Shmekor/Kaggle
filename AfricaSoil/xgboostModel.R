kaggle.africa.xgboostModel.train <- function(ds, sFeatures, ...) {
  
  trainModel <- function(tDS, target) {
    dtrain <- xgb.DMatrix(as.matrix(tDS), label = target)
    model <- xgboost(data = dtrain, objective = "reg:linear", eval_metric='rmse', ...)
    model
  }
  
  result <- list()
  result$sFeatures <- names(ds)[sFeatures]
  
  result$caModel <- trainModel(ds[,result$sFeatures], ds$Ca)
  result$pModel <- trainModel(ds[,result$sFeatures], ds$P)
  result$pHModel <- trainModel(ds[,result$sFeatures], ds$pH)
  result$socModel <- trainModel(ds[,result$sFeatures], ds$SOC)
  result$sandModel <- trainModel(ds[,result$sFeatures], ds$Sand)
  
  result
}

kaggle.africa.xgboostModel.predict <- function(model, ds) {
  result <- data.frame(PIDN=ds$PIDN)
  result$Ca <- predict(model$caModel, as.matrix(ds[,model$sFeatures]))
  result$P <- predict(model$pModel, as.matrix(ds[,model$sFeatures]))
  result$pH <- predict(model$pHModel, as.matrix(ds[,model$sFeatures]))
  result$SOC <- predict(model$socModel, as.matrix(ds[,model$sFeatures]))
  result$Sand <- predict(model$sandModel, as.matrix(ds[,model$sFeatures]))
  
  result
}