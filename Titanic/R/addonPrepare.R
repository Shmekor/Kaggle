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