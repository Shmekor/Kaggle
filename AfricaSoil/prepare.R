library('caret')
library('Metrics')
library('doMC')

setwd("~/Documents/Workspace/DataScience/Kaggle/AfricaSoil")

fullTrainDS <- read.csv('data/training.csv', stringsAsFactors=FALSE)
inTrain <- createDataPartition(fullTrainDS$P, p = 0.80, list = FALSE)
trainDS <- fullTrainDS[inTrain,]
testDS <- fullTrainDS[-inTrain,]
kaggleDS <- read.csv('data/sorted_test.csv')

cvFolds <- createFolds(trainDS$P, 10, list = TRUE, returnTrain = FALSE)

#1 - ID, 2-3579 - spectroscopy features
soilId = 'PIDN'
co2Features <- names(trainDS)[2656:2670]
ssFeatures = names(trainDS)[2:3579]
ssFeaturesMCO2 <- names(trainDS)[c(2:2655, 2671:3579)]
addFeatures = names(trainDS)[3580:3594]
depth = 3595
targetCa = 3596
targetP = 3597
targetpH = 3598
targetSOC = 3599
targetSand = 3600
targetsAll = names(trainDS)[3596:3600]

globalSeed = 543533
set.seed(globalSeed)

getDerivative <- function(ds) {
  forDer <- ds[,2:2655]
  firstDerivative <- forDer - cbind(NA, forDer)[, -(dim(forDer)[2]+1)]
  result <- cbind(ds[,3580:3595], forDer[,-1])
  
  forDer <- ds[,2671:3579]
  firstDerivative <- forDer - cbind(NA, forDer)[, -(dim(forDer)[2]+1)]
  result <- cbind(result, forDer[,-1])
  
  result
}

trainDSDer <- getDerivative(trainDS)
testDSDer <- getDerivative(testDS)
kaggleDSDer <- getDerivative(kaggleDS)

trainDSDer <- cbind(trainDSDer, trainDS[,c(soilId, targetsAll)])
testDSDer <- cbind(testDSDer, testDS[,c(soilId, targetsAll)])