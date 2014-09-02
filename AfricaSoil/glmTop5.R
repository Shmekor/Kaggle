source('prepare.R')

afCor <- cor(trainDS[ssFeatures], trainDS[targetsAll])
summary(abs(afCor))

maxPc = 0.05

corMax5Ca <- names(sort(abs(afCor[,1]), decreasing = TRUE)[1:(as.integer(nrow(afCor) * maxPc))])
corMax5P <- names(sort(abs(afCor[,2]), decreasing = TRUE)[1:(as.integer(nrow(afCor) * maxPc))])
corMax5pH <- names(sort(abs(afCor[,3]), decreasing = TRUE)[1:(as.integer(nrow(afCor) * maxPc))])
corMax5SOC <- names(sort(abs(afCor[,4]), decreasing = TRUE)[1:(as.integer(nrow(afCor) * maxPc))])
corMax5Sand <- names(sort(abs(afCor[,5]), decreasing = TRUE)[1:(as.integer(nrow(afCor) * maxPc))])

#/////////////////////////////////////////////////////
set.seed(45453)

inTrainCa <- createDataPartition(trainDS$Ca, p=0.75, list=FALSE)
trainDSCa <- trainDS[inTrainCa,]
cvDSCa <- trainDS[-inTrainCa,]

#/////////////////////////////////////////////////////

simpleGLM <- function(formula, tDS, cvDS, target) {
  model <- train(formula, method="lm", data=tDS)
  print(getTrainPerf(model))
  
  predict <- predict(model, cvDS)
  print(rmse(cvDS[,target], predict))
  
  predict
}

#CV error - 0.3760926
caPred <- simpleGLM(Ca ~ ., trainDSCa[,c(corMax5Ca, 'Ca')], cvDSCa, 'Ca')
#CV error - 1.173141
pPred <- simpleGLM(P ~ ., trainDSCa[,c(corMax5P, 'P')], cvDSCa, 'P')
#CV error - 0.4545637
pHPred <- simpleGLM(pH ~ ., trainDSCa[,c(corMax5pH, 'pH')], cvDSCa, 'pH')
#CV error - 0.6406343
socPred <- simpleGLM(SOC ~ ., trainDSCa[,c(corMax5SOC, 'SOC')], cvDSCa, 'SOC')
#CV error - 0.4664847
sandPred <- simpleGLM(Sand ~ ., trainDSCa[,c(corMax5Sand, 'Sand')], cvDSCa, 'Sand')

#//////////////////////////////////////////////////////

kaggleTestDS <- read.csv('data/sorted_test.csv')

result <- data.frame(PIDN=kaggleTestDS$PIDN)

result$Ca <- simpleGLM(Ca ~ ., trainDS[,c(corMax5Ca, 'Ca')], kaggleTestDS, 'Ca')
result$P <- simpleGLM(P ~ ., trainDS[,c(corMax5P, 'P')], kaggleTestDS, 'P')
result$pH <- simpleGLM(pH ~ ., trainDS[,c(corMax5pH, 'pH')], kaggleTestDS, 'pH')
result$SOC <- simpleGLM(SOC ~ ., trainDS[,c(corMax5SOC, 'SOC')], kaggleTestDS, 'SOC')
result$Sand <- simpleGLM(Sand ~ ., trainDS[,c(corMax5Sand, 'Sand')], kaggleTestDS, 'Sand')

write.csv(result, file = "data/glmSubmit.csv", row.names = FALSE)