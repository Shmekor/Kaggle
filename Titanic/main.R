library('caret')
library('ggplot2')


dataSet <- read.csv('train.csv', stringsAsFactors=FALSE)

#explore data
summary(dataSet)

prepareData <- function(data) {
  #data$Survived <- as.factor(data$Survived)
  data$Pclass <- as.factor(data$Pclass)
  data$Sex <- as.factor(data$Sex)
  data[is.na(data$Age),]$Age <- median(data$Age, na.rm=TRUE)
  data$AgeGroup <- cut(data$Age, breaks=c((0:6)*10, 100))
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
  
  data
}


dataSetP <- prepareData(dataSet)

dataSetP$Survived <- as.factor(dataSetP$Survived)
dataSetP$SurvivedInt <- dataSet$Survived

dataSetP$HasRelatives <- 0
dataSetP[dataSetP$SibSp > 0 | dataSetP$Parch > 0,]$HasRelatives <- 1
dataSetP$HasRelatives <- as.factor(dataSetP$HasRelatives)



dataSetP$IsChild <- 0
dataSetP[dataSetP$Age < 18,]$IsChild <- 1
dataSetP$IsChild <- as.factor(dataSetP$IsChild)

dataSetP$Embarked <- dataSet$Embarked
dataSetP[dataSetP$Embarked == '',]$Embarked <- 'S'
dataSetP$Embarked <- as.factor(dataSetP$Embarked)
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

set.seed(32133)

inTrain <- createDataPartition(dataSetP$Survived, p=.75, list = FALSE)
trainDS <- dataSetP[inTrain,]
testDS <- dataSetP[-inTrain,]
summary(trainDS)

qplot(Survived, data=trainDS, fill=HasRelatives, geom='bar')

#train Model #################################################
formula <- Survived ~ Sex + Pclass + Fare

mf1 <- train(formula, method="rf", prox=TRUE, data=trainDS)
mf2 <- train(formula, method="nnet", data=trainDS)
mf3 <- train(formula, method="lda", data=trainDS)

formula2 <- Survived ~ Sex + Pclass + Fare + Age
mf4 <- train(formula2, method="rf", prox=TRUE, data=trainDS)
mf5 <- train(formula2, method="nnet", data=trainDS)
mf6 <- train(formula2, method="lda", data=trainDS)

formula3 <- Survived ~ Sex + Pclass + FareG + AgeGroup
mf7 <- train(formula3, method="rf", prox=TRUE, data=trainDS)
mf8 <- train(formula3, method="nnet", data=trainDS)
mf9 <- train(formula3, method="lda", data=trainDS)

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

confusionMatrix(predDFTest$predM1, testDS$Survived)

#submission
testKaggle <- read.csv('test.csv')
testKaggle <- prepareData(testKaggle)

predDFKaggle <- predictOnModel(testKaggle)
combPredKaggle <- predict(combModFit, predDFKaggle)

testKaggle$Survived <- combPredKaggle

submit <- data.frame(PassengerId = testKaggle$PassengerId, Survived = testKaggle$Survived)
write.csv(submit, file = "submit.csv", row.names = FALSE)