library('caret')
library('Hmisc')
setwd("D:/Workspace/DataScience/Kaggle/Titanic")

source('R/prepareData.R')

dataSet <- read.csv('train.csv', stringsAsFactors=FALSE)
dataSetP <- prepareData(dataSet)

dataSetP$Survived <- as.factor(dataSetP$Survived)
dataSetP$SurvivedInt <- dataSet$Survived

#train
inTrain <- createDataPartition(dataSetP$Survived, p=.75, list = FALSE)
trainDS <- dataSetP[inTrain,]
testDS <- dataSetP[-inTrain,]

#AdaBoost ###############################################

formula <- Survived ~ Luck + Sex + Pclass + Fare + Age + Embarked + SibSp + Parch
mfAda <- train(formula, method='C5.0Cost', data=trainDS)

formulaP1 <- Survived ~ Sex + Pclass + Fare
formulaP2 <- Survived ~ Sex + Pclass + Age
formulaP3 <- Survived ~ Embarked

formulaP4 <- Survived ~ Pclass + Fare

mfP1 <- train(formulaP1, method='rf', data=trainDS)
mfP2 <- train(formulaP2, method='rf', data=trainDS)
mfP3 <- train(formulaP3, method='lda', data=trainDS)
mfP4 <- train(formulaP4, method='lda', data=trainDS)
mfP5 <- train(formulaP5, method='lda', data=trainDS)

mfP1$finalModel
mfP2$finalModel
mfP3$finalModel
mfP4$finalModel
mfP5$finalModel

testPr <- predict(mfP3, testDS)
confusionMatrix(testPr, testDS$Survived)

#train Model 
formulaRF1 <- Survived ~ Sex + Pclass + Fare + Age + SibSp + Parch + Embarked
formulaRF2 <- Survived ~ Sex + Pclass + Fare + Age + Embarked
formulaRF3 <- Survived ~ Sex + Pclass + Fare
formulaS1 <- Survived ~ Pclass
formulaS2 <- Survived ~ Fare
formulaS3 <- Survived ~ Age

mf1 <- train(formulaRF1, method="rf", prox=TRUE, data=trainDS)
mf2 <- train(formulaRF2, method="rf", prox=TRUE, data=trainDS)
mf3 <- train(formulaRF3, method="rf", prox=TRUE, data=trainDS)

mf4 <- train(formulaRF1, method="avNNet", data=trainDS)
mf5 <- train(formulaRF2, method="avNNet", data=trainDS)
mf6 <- train(formulaRF3, method="avNNet", data=trainDS)

mf7 <- train(formulaS1, method="lda", data=trainDS)
mf8 <- train(formulaS2, method="lda", data=trainDS)
mf9 <- train(formulaS3, method="lda", data=trainDS)

#predict on each ################################################################

testPr <- predict(mf9, testDS)
confusionMatrix(testPr, testDS$Survived)

predictOnModel <- function(ds) {
  predM1 <- predict(mf1, ds)
  predM2 <- predict(mf2, ds)
  predM3 <- predict(mf3, ds)
  predM4 <- predict(mf4, ds)
  predM5 <- predict(mf5, ds)
  predM6 <- predict(mf6, ds)
  predM7 <- predict(mf7, ds)
  predM8 <- predict(mf8, ds)
  predM9 <- predict(mf9, ds)
  
  data.frame(predM1, predM2, predM3, predM4, predM5, predM6, predM7, predM8, predM9)
}

predDF <- predictOnModel(trainDS)
predDF$Survived <- trainDS$Survived

combModFit <- train(Survived ~.,method="rf", prox=TRUE, data=predDF)
combModFit$finalModel

combModFit2 <- train(Survived ~.,method="nnet", prox=TRUE, data=predDF)
combModFit2$finalModel

combModFit3 <- train(Survived ~.,method="knn", data=predDF)
combModFit3$finalModel

combModFit4 <- train(Survived ~.,method="glm", data=predDF)
combModFit4$finalModel

#simple by Gender #####################################################
source('R/model.splitBySex.R')
sbsTrain <- model.splitBySex.train(dataSetP)

predictTrain <- model.splitBySex.predict(trainDS, sbsTrain)
confusionMatrix(predictTrain$SPredicted, predictTrain$Survived)

predictTest <- model.splitBySex.predict(testDS, sbsTrain)
confusionMatrix(predictTest$SPredicted, predictTest$Survived)

source('R/kaggle.submit.R')
kaggle.submit(model.splitBySex.predict, sbsTrain)

#with random "Luck" ############################################################
source('model.randomLuck.R')
rlMF <- model.randomLuck.train(trainDS)
rlPredictTest <- model.randomLuck.predict(testDS, rlMF)
confusionMatrix(rlPredictTest$SPredicted, rlPredictTest$Survived)



#with split and random "Luck" ##################################################
source('model.sbsAndRl.R')
sbsrlMf <- model.sbsAndRl.train(trainDS)

sbsrlPTest <- model.sbsAndRl.predict(testDS, sbsrlMf)
pTestC <- merge(testDS, sbsrlPTest, by='PassengerId')

confusionMatrix(pTestC$SPredicted, pTestC$Survived)


#simple by Gender Kaggle #######################################################

submit <- data.frame(PassengerId=tcmFM$PassengerId, Survived=predictTCM)


##############################################################################

confusionMatrix(predictF, testBG[[1]]$Survived)
confusionMatrix(predictM, testBG[[2]]$Survived)

compileTest <- data.frame(Survived=as.factor(c(as.character(testBG[[1]]$Survived), as.character(testBG[[2]]$Survived))), Predicted=as.factor(c(as.character(predictF), as.character(predictM))))
confusionMatrix(compileTest$Predicted, compileTest$Survived)

#validate train Model #################################################
combPredTrain <- predict(combModFit, predDF)
confusionMatrix(combPredTrain, trainDS$Survived)

trainFail <- trainDS
trainFail$SPredicted <- combPredTrain
trainFail2 <- trainFail[trainFail$Survived != trainFail$SPredicted,]

predDFV <- predDF
ffTrain <- trainDS
ffTrain$SPredicted <- predDF$Survived
ffTrain$FF <- (predDFV$predM1 == 1 & predDFV$predM2 == 1 & predDFV$predM3 == 1 & predDFV$predM4 == 1 & predDFV$Survived == 0) | (predDFV$predM1 == 0 & predDFV$predM2 == 0 & predDFV$predM3 == 0 & predDFV$predM4 == 0 & predDFV$Survived == 1)
fullFail <- ffTrain[ffTrain$FF == TRUE,]
summary(fullFail)

failCR <- train(Survived ~ Sex + Pclass, method="lda", data=fullFail)
failCR$finalModel

#predict test Model ###################################################

predDFTest <- predictOnModel(testDS)

combPredTest <- predict(combModFit, predDFTest)
confusionMatrix(combPredTest, testDS$Survived)

combPredTest2 <- predict(combModFit2, predDFTest)
confusionMatrix(combPredTest2, testDS$Survived)

combPredTest3 <- predict(combModFit3, predDFTest)
confusionMatrix(combPredTest3, testDS$Survived)

combPredTest4 <- predict(combModFit4, predDFTest)
confusionMatrix(combPredTest4, testDS$Survived)

confusionMatrix(predDFTest$predM1, testDS$Survived)

#submission
testKaggle <- read.csv('test.csv')
testKaggle <- prepareData(testKaggle)

predDFKaggle <- predictOnModel(testKaggle)
combPredKaggle <- predict(combModFit, predDFKaggle)

testKaggle$Survived <- combPredKaggle

submit <- data.frame(PassengerId = testKaggle$PassengerId, Survived = testKaggle$Survived)
write.csv(submit, file = "submit.csv", row.names = FALSE)

#by Gender
testKaggle <- read.csv('test.csv')
testKaggle <- prepareData(testKaggle)

testKaggleBG <- split(testKaggle, testKaggle$Sex)

predictKF <- predict(mfFemale, testKaggleBG[[1]])
predictKM <- predict(mfMale, testKaggleBG[[2]])

submit <- data.frame(PassengerId=c(testKaggleBG[[1]]$PassengerId, testKaggleBG[[2]]$PassengerId), Survived=as.factor(c(as.character(predictKF), as.character(predictKM))))
write.csv(submit, file = "submit.csv", row.names = FALSE)