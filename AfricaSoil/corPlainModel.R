kaggle.africa.glmTop5.train <- function(ds, sFeatures, tAll, maxPc = 0.05, tMethod='glm', tControl=NULL) {
  afCor <- cor(ds[,sFeatures], ds[tAll])
  
  corMax5Ca <- names(sort(abs(afCor[,1]), decreasing = TRUE)[1:(as.integer(nrow(afCor) * maxPc))])
  corMax5P <- names(sort(abs(afCor[,2]), decreasing = TRUE)[1:(as.integer(nrow(afCor) * maxPc))])
  corMax5pH <- names(sort(abs(afCor[,3]), decreasing = TRUE)[1:(as.integer(nrow(afCor) * maxPc))])
  corMax5SOC <- names(sort(abs(afCor[,4]), decreasing = TRUE)[1:(as.integer(nrow(afCor) * maxPc))])
  corMax5Sand <- names(sort(abs(afCor[,5]), decreasing = TRUE)[1:(as.integer(nrow(afCor) * maxPc))])
  
  #/////////////////////////////////////////////////////
  
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
  result$caModel <- trainModel(Ca ~ ., ds[,c(corMax5Ca, 'Ca')])
  result$pModel <- trainModel(P ~ ., ds[,c(corMax5P, 'P')])
  result$pHModel <- trainModel(pH ~ ., ds[,c(corMax5pH, 'pH')])
  result$socModel <- trainModel(SOC ~ ., ds[,c(corMax5SOC, 'SOC')])
  result$sandModel <- trainModel(Sand ~ ., ds[,c(corMax5Sand, 'Sand')])
  
  result
}

kaggle.africa.glmTop5.predict <- function(model, ds) {
  result <- data.frame(PIDN=ds$PIDN)
  result$Ca <- predict(model$caModel, ds)
  result$P <- predict(model$pModel, ds)
  result$pH <- predict(model$pHModel, ds)
  result$SOC <- predict(model$socModel, ds)
  result$Sand <- predict(model$sandModel, ds)
  
  result
}