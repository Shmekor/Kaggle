source('model.splitBySex.R')
source('model.randomLuck.R')

model.sbsAndRl.train <- function(trainDS) {
  result <- model.splitBySex.train(trainDS)
  result$RL <- model.randomLuck.train(trainDS)
  
  newDs <- model.sbsAndRl.predictSub(trainDS, result)

  formula <- Survived ~ SBSPredict + RLPredict
  result$SBSRL <- train(formula, method='glm', data=newDs)
  
  result
}

model.sbsAndRl.predictSub <- function(ds, result) {
  sbsPredict <- model.splitBySex.predict(ds, result)
  sbsPredict$SBSPredict <- sbsPredict$SPredicted
  
  rlPredict <-  model.randomLuck.predict(ds, result$RL)
  rlPredict$RLPredict <- rlPredict$SPredicted
  
  merge(sbsPredict, rlPredict, by=c('PassengerId', 'Survived'))
}

model.sbsAndRl.predict <- function(ds, model) {
  newDs <- model.sbsAndRl.predictSub(ds, model)
  p <- predict(model$SBSRL, newDs)
  
  data.frame(PassengerId=newDs$PassengerId, SPredicted=p)
}