source('prepare.R')

#/////////////////////////////////////////////////////
set.seed(44311)

inTrain <- createDataPartition(trainDS$Ca, p=0.75, list=FALSE)
trainDS <- trainDS[inTrain,]
cvDS <- trainDS[-inTrain,]

#/////////////////////////////////////////////////////

simpleGLM <- function(formula, tDS, cvDS, target) {
  model <- train(formula, method="lm", data=tDS)
  print(getTrainPerf(model))
  
  predict <- predict(model, cvDS)
  print(rmse(cvDS[,target], predict))
  
  predict
}

#CV error - 0.6352328
caPred <- simpleGLM(Ca ~ ., trainDS[,c(addFeatures, 'Ca')], cvDS, 'Ca')
#CV error - 0.7845864
pPred <- simpleGLM(P ~ ., trainDS[,c(addFeatures, 'P')], cvDS, 'P')
#CV error - 0.6815413
pHPred <- simpleGLM(pH ~ ., trainDS[,c(addFeatures, 'pH')], cvDS, 'pH')
#CV error - 0.8993766
socPred <- simpleGLM(SOC ~ ., trainDS[,c(addFeatures, 'SOC')], cvDS, 'SOC')
#CV error - 0.7790011
sandPred <- simpleGLM(Sand ~ ., trainDS[,c(addFeatures, 'Sand')], cvDS, 'Sand')
