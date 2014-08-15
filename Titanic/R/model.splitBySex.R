model.splitBySex.train <- function(trainDS) {
  trainBG <- split(trainDS, trainDS$Sex)
  
  formula <- Survived ~ Luck + Pclass + Fare + Age + AgePclass + SibSp + Parch + Embarked
  
  mfFemale1 <- train(formula, method="rf", data=trainBG[[1]])
  mfFemale2 <- ''#train(formula, method="nnet", data=trainBG[[1]])
  mfFemale3 <- ''#train(formula, method="lda", data=trainBG[[1]])
  mfFemale4 <- ''#train(formula, method="gbm", data=trainBG[[1]])
  
  mfMale1 <- train(formula, method="rf", data=trainBG[[2]])
  mfMale2 <- ''#train(formula, method="nnet", data=trainBG[[2]])
  mfMale3 <- ''#train(formula, method="lda", data=trainBG[[2]])
  mfMale4 <- ''#train(formula, method="gbm", data=trainBG[[2]])
  
  resultModel <- list(PF1=mfFemale1, PF2=mfFemale2, PF3=mfFemale3, PF4=mfFemale4, PM1=mfMale1, PM2=mfMale2, PM3=mfMale3, PM4=mfMale4)
  
  prSplit <- model.splitBySex.predictSplitted(trainBG, resultModel)
  cmFM <- model.splitBySex.merge(trainBG, prSplit)
  cmFM$Luck <- rep(1, nrow(cmFM))#rnorm(nrow(cmFM), 0.5, 0.5)
  
  formulaCM <- Survived ~ Sex + PF1 #+ PF2 + PF3 + PF4
  cmModel <- train(formulaCM, method="lda", data=cmFM)
  
  resultModel$PCM <- cmModel
  resultModel
}

model.splitBySex.predictSplitted <- function(ds, model) {
  predictF1 <- predict(model$PF1, ds[[1]])
  predictF2 <- ''#predict(model$PF2, ds[[1]])
  predictF3 <- ''#predict(model$PF3, ds[[1]])
  predictF4 <- ''#predict(model$PF4, ds[[1]])
  
  predictM1 <- predict(model$PM1, ds[[2]])
  predictM2 <- ''#predict(model$PM2, ds[[2]])
  predictM3 <- ''#predict(model$PM3, ds[[2]])
  predictM4 <- ''#predict(model$PM4, ds[[2]])
  
  list(PrF1=predictF1,PrF2=predictF2,PrF3=predictF3,PrF4=predictF4,PrM1=predictM1,PrM2=predictM2,PrM3=predictM3,PrM4=predictM4)
}

model.splitBySex.merge <- function(sDs, prSplit) {
  cmF <- sDs[[1]]
  cmF$PF1 <- prSplit$PrF1
  cmF$PF2 <- prSplit$PrF2
  cmF$PF3 <- prSplit$PrF3
  cmF$PF4 <- prSplit$PrF4
  
  cmM <- sDs[[2]]
  cmM$PF1 <- prSplit$PrM1
  cmM$PF2 <- prSplit$PrM2
  cmM$PF3 <- prSplit$PrM3
  cmM$PF4 <- prSplit$PrM4
  
  rbind(cmF, cmM)
}

model.splitBySex.predict <- function(testDS, model) {
  testBG <- split(testDS, testDS$Sex)
  prSplit <- model.splitBySex.predictSplitted(testBG, model)
  
  cmF <- model.splitBySex.merge(testBG, prSplit)
  cmF$Luck <- rep(1, nrow(cmF))#rnorm(nrow(cmF), 0.5, 0.5)
  
  cmF$SPredicted <- predict(model$PCM, cmF)
  cmF
}