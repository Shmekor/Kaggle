library('caret')
library('Hmisc')

dataSet <- read.csv('train.csv', stringsAsFactors=FALSE)

#explore data
summary(dataSet)

prepareData <- function(data) {
  #data$Survived <- as.factor(data$Survived)
  data$Pclass <- as.factor(data$Pclass)
  data$Sex <- as.factor(data$Sex)
  data[is.na(data$Age),]$Age <- median(data$Age, na.rm=TRUE)
  data$AgeGroup <- cut2(data$Age, g=4)
  if(nrow(data[is.na(data$Fare),]) > 0) {
    data[is.na(data$Fare),]$Fare <- median(data$Fare, na.rm=TRUE)
  }
  
  data$FareG <- 40
  data[data$Fare < 30,]$FareG <- 30
  data[data$Fare < 20,]$FareG <- 20
  data[data$Fare < 10,]$FareG <- 10
  data$FareG <- as.factor(data$FareG)
  
  data$HasCabin <- 0
  data[data$Cabin != '',]$HasCabin <- 1
  data$HasCabin <- as.factor(data$HasCabin)
  
  if(nrow(data[data$Embarked == '',]) > 0) {
    data[data$Embarked == '',]$Embarked <- 'S'
  }
  data$Embarked <- as.factor(data$Embarked)
  
  data
}


dataSetP <- prepareData(dataSet)
summary(dataSetP)

dataSetP$Survived <- as.factor(dataSetP$Survived)
dataSetP$SurvivedInt <- dataSet$Survived

dataSetP$HasRelatives <- 0
dataSetP[dataSetP$SibSp > 0 | dataSetP$Parch > 0,]$HasRelatives <- 1
dataSetP$HasRelatives <- as.factor(dataSetP$HasRelatives)



dataSetP$IsChild <- 0
dataSetP[dataSetP$Age < 18,]$IsChild <- 1
dataSetP$IsChild <- as.factor(dataSetP$IsChild)


summary(dataSetP$Embarked)

dataSetP$PclassInt <- as.integer(dataSetP$Pclass)^2

dataSetP$SexPC <- 1
dataSetP[dataSetP$Pclass == 1 & dataSetP$Sex == 'male',]$SexPC <- 1
dataSetP[dataSetP$Pclass == 2 & dataSetP$Sex == 'male',]$SexPC <- 2
dataSetP[dataSetP$Pclass == 3 & dataSetP$Sex == 'male',]$SexPC <- 3
dataSetP[dataSetP$Pclass == 1 & dataSetP$Sex == 'female',]$SexPC <- 4
dataSetP[dataSetP$Pclass == 2 & dataSetP$Sex == 'female',]$SexPC <- 5
dataSetP[dataSetP$Pclass == 3 & dataSetP$Sex == 'female',]$SexPC <- 6
dataSetP$SexPC <- as.factor(dataSetP$SexPC)
summary(dataSetP$SexPC)

dataSetP$RelativesCount <- dataSetP$SibSp + dataSetP$Parch

bySurv <- split(dataSetP, dataSetP$Survived)
ticketsDied <- bySurv[[1]]$Ticket
ticketsSurv <- bySurv[[2]]$Ticket
tickets <- data.frame(surv=ticketsSurv, died=ticketsDied[1:342])

died <- bySurv[[1]]
fDied <- died[died$Sex == 'female',]
table(fDied$Pclass)
survived <- bySurv[[2]]
fSurv <- survived[survived$Sex == 'female',]
table(fSurv$Pclass)

table(dataSetP$Survived, dataSetP$Embarked)

table(dataSetP$Survived, dataSetP$Sex)

prop.table(table(dataSetP$Survived))
rbinom(n=nrow(dataSetP), size=1, prob=0.3838384)
?rbinom

qplot(Survived, data=dataSetP, fill=Pclass, geom='bar')
qplot(Survived, data=dataSetP, fill=HasRelatives, geom='bar')
qplot(Survived, data=dataSetP, fill=HasCabin, geom='bar')
qplot(IsChild, data=dataSetP, fill=Survived, geom='bar')
qplot(AgeGroup, data=dataSetP, fill=Survived, geom='bar')

t <- table(dataSetP$Survived, dataSetP$SibSp)
t[1,] / t[2,]
t <- table(dataSetP$Survived, dataSetP$Parch)
t[1,] / t[2,]

dataSetP$MaleLuck <- 0
dataSetP[dataSetP$Sex == 'male' & dataSetP$Pclass == '1',]$MaleLuck <- 10
dataSetP$MaleLuck <- as.factor(dataSetP$MaleLuck)
summary(dataSetP)

table(dataSetP$Survived, dataSetP$MaleLuck)

#train
summary(dataSetP)
str(dataSetP)

set.seed(32132)

inTrain <- createDataPartition(dataSetP$Survived, p=.75, list = FALSE)
trainDS <- dataSetP[inTrain,]
testDS <- dataSetP[-inTrain,]
summary(trainDS)

qplot(Survived, data=trainDS, fill=HasRelatives, geom='bar')
featurePlot(trainDS, y=trainDS$Survived, plot='pairs')

trainBG <- split(trainDS, trainDS$Sex)
testBG <- split(testDS, testDS$Sex)
summary(testBG[[2]])

#simple train and validate ###############################################

formulaT <- SurvivedInt ~ Sex + Pclass + Fare + Age + SibSp + Parch + Embarked

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
formula <- Survived ~ Pclass + Fare + Age + SibSp + Parch + Embarked

mfFemale <- train(formula, method="rf", prox=TRUE, data=trainBG[[1]])
mfMale <- train(formula, method="rf", prox=TRUE, data=trainBG[[2]])

predictF <- predict(mfFemale, testBG[[1]])
predictM <- predict(mfMale, testBG[[2]])

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