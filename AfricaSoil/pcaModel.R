kaggle.africa.pcaModel.train <- function(ds, pcaFeatures, tAll, maxPc = '0.10', tMethod = 'glm', tControl = NULL) {
  pca <- preProcess(ds[,pcaFeatures], method='pca', thresh = 1)
  
  featuresPCA <- predict(pca, ds[,pcaFeatures])
  pcaDS <- cbind(featuresPCA, ds[,tAll])
  
  afCor <- cor(featuresPCA, ds[,tAll])
  
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
  result$pca <- pca
  result$pcaFeatures <- pcaFeatures
  result$caModel <- trainModel(Ca ~ ., pcaDS[,c(corMax5Ca, 'Ca')])
  result$pModel <- trainModel(P ~ ., pcaDS[,c(corMax5P, 'P')])
  result$pHModel <- trainModel(pH ~ ., pcaDS[,c(corMax5pH, 'pH')])
  result$socModel <- trainModel(SOC ~ ., pcaDS[,c(corMax5SOC, 'SOC')])
  result$sandModel <- trainModel(Sand ~ ., pcaDS[,c(corMax5Sand, 'Sand')])
  
  result
}

kaggle.africa.pcaModel.predict <- function(model, ds) {

  pcaDs <- predict(model$pca, ds[,model$pcaFeatures])

  result <- data.frame(PIDN=ds$PIDN)
  result$Ca <- predict(model$caModel, pcaDs)
  result$P <- predict(model$pModel, pcaDs)
  result$pH <- predict(model$pHModel, pcaDs)
  result$SOC <- predict(model$socModel, pcaDs)
  result$Sand <- predict(model$sandModel, pcaDs)
    
  result
}