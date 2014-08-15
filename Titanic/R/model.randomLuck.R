model.randomLuck.train <- function(trainDS) {
  trainDS$Luck <- rnorm(nrow(trainDS), 0.5, 0.5)
  
  formula <- Survived ~ Luck + Sex + Pclass + Fare + Age + SibSp + Parch + Embarked
  
  mf1 <- train(formula, method='rf', data=trainDS)
  mf1
}

model.randomLuck.predict <- function(ds, model) {
  ds$Luck <- rnorm(nrow(ds), 0.5, 0.5)
  
  p <- predict(model, ds)
  ds$SPredicted <- p
  ds
}