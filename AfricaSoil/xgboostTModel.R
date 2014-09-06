kaggle.africa.xgboostTModel.train <- function(ds, sFeatures, mDepth = 3, geta = 0.5, gnround = 60, gverbose = 0) {
  
  trainModel <- function(tDS, target) {
    dtrain <- xgb.DMatrix(as.matrix(tDS), label = target)
    model <- xgboost(data = dtrain, max_depth = mDepth, eta = geta, nround = gnround,
                     objective = "reg:linear", eval_metric='rmse', verbose = gverbose)
    model
  }
  
  result <- list()
  result$caFeatures <- names(ds)[sFeatures]
  result$sandFeatures <- c(names(ds)[sFeatures], 'Ca')
  result$socFeatures <- c(names(ds)[sFeatures], 'Ca', 'Sand')
  result$phFeatures <- c(names(ds)[sFeatures], 'Ca', 'Sand', 'SOC')
  result$pFeatures <- c(names(ds)[sFeatures], 'Ca', 'Sand', 'SOC', 'pH')
  
  result$caModel <- trainModel(ds[,result$caFeatures], ds$Ca)
  result$sandModel <- trainModel(ds[,result$sandFeatures], ds$Sand)
  result$socModel <- trainModel(ds[,result$socFeatures], ds$SOC)
  result$pHModel <- trainModel(ds[,result$phFeatures], ds$pH)
  result$pModel <- trainModel(ds[,result$pFeatures], ds$P)
  
  result
}

kaggle.africa.xgboostTModel.predict <- function(model, ds) {
  result <- data.frame(PIDN=ds$PIDN)
  result$Ca <- predict(model$caModel, as.matrix(ds[,model$caFeatures]))
  result$Sand <- predict(model$sandModel, as.matrix(cbind(ds, result$Ca)[,model$sandFeatures]))
  result$SOC <- predict(model$socModel, as.matrix(cbind(ds, result$Ca, result$Sand)[,model$socFeatures]))
  result$pH <- predict(model$pHModel, as.matrix(cbind(ds, result$Ca, result$Sand, result$SOC)[,model$phFeatures]))
  result$P <- predict(model$pModel, as.matrix(cbind(ds, result$Ca, result$Sand, result$SOC, result$pH)[,model$socFeatures]))
  
  result
}