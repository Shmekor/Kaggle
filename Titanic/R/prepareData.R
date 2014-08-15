prepareData <- function(data) {
  #data$Survived <- as.factor(data$Survived)
  data[is.na(data$Age),]$Age <- median(data$Age, na.rm=TRUE)
  data$AgePclass = data$Age*data$Pclass
  data$Pclass <- as.factor(data$Pclass)
    data$Sex <- as.factor(data$Sex)
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
  data$Luck <- rnorm(nrow(data), 0.5, 0.5)
  
  data
}