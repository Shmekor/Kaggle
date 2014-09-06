kaggle.africa.evaluate <- function(predicted, actual) {
  err <- kaggle.africa.evaluateV(predicted, actual)
  print(err)
  
  sum(err) / 5
}

kaggle.africa.evaluateV <- function(predicted, actual) {
  err = c()
  err <- append(err, rmse(actual$Ca, predicted$Ca))
  err <- append(err, rmse(actual$P, predicted$P))
  err <- append(err, rmse(actual$pH, predicted$pH))
  err <- append(err, rmse(actual$SOC, predicted$SOC))
  err <- append(err, rmse(actual$Sand, predicted$Sand))
  
  names(err) <- c('Ca', 'P', 'pH', 'SOC', 'Sand')
  err
}

kaggle.africa.kfEvaluate <- function(kfResults, folds, ds) {
  res = data.frame()
  for(i in 1:length(folds)) {
    res <- rbind(res, kaggle.africa.evaluateV(kfResults[[i]], ds[folds[[i]]$test,]))
  }
  names(res) <- c('Ca', 'P', 'pH', 'SOC', 'Sand')
  print(res)
  
  res 
}

kaggle.africa.kfoldsTrain <- function(ds, folds, trainFunc, ...) {
  result <- list()
  for(i in 1:length(folds)) {
    result[[i]] <- list()
    result[[i]]$test <- folds[[i]]
    
    training <- ds[unlist(folds[-i]),]
    print(sprintf('Train fold %s', i))
    result[[i]]$model <- trainFunc(training, ...)
  }
  
  result
}

kaggle.africa.kfoldsPredict <- function(foldsModels, ds, predictFn) {
  results = list()
  
  for(i in 1:length(foldsModels)) {
    results[[i]] = predictFn(foldsModels[[i]]$model, ds[foldsModels[[i]]$test,])
  }
  
  results
}