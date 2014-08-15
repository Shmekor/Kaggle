kaggle.submit <- function(predictFn, model) {
  testKaggle <- read.csv('test.csv')
  testKaggle <- prepareData(testKaggle)
  
  p <- predictFn(testKaggle, model)
    
  submit <- data.frame(PassengerId=p$PassengerId, Survived=p$SPredicted)
  write.csv(submit, file = "submit.csv", row.names = FALSE)
}