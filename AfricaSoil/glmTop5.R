source('prepare.R')
source('corPlainModel.R')
source('evaluationMetrics.R')

cvModels <- kaggle.africa.kfoldsTrain(trainDSDer, cvFolds, kaggle.africa.glmTop5.train,
                                      c(1:15, 17:3577), targetsAll, 0.05, 'glm')

kfPredict <- kaggle.africa.kfoldsPredict(cvModels, trainDSDer, kaggle.africa.glmTop5.predict)
evMetrics <- kaggle.africa.kfEvaluate(kfPredict, cvModels, trainDSDer)

colMeans(evMetrics)
mean(colMeans(evMetrics))

models <- kaggle.africa.glmTop5.train(trainDSDer, c(1:15, 17:3577),  targetsAll, 0.05, 'rf')
predictions <- kaggle.africa.kfoldsPredict(trainDSDer, cvFolds, models, kaggle.africa.glmTop5.predict)

evRes <- kaggle.africa.kfEvaluate(predictions, cvFolds, trainDSDer)

colMeans(evRes)
mean(colMeans(evRes))

predTest <- kaggle.africa.glmTop5.predict(models, testDSDer)
evRes <- kaggle.africa.evaluateV(predTest, testDSDer)
evRes
mean(evRes)
#//////////////////////////////////////////////////////

models2 <- kaggle.africa.glmTop5.train(trainDSDer, c(1:3563, 3565:3579),  targetsAll, 0.10, 'glm')
predictions2 <- kaggle.africa.kfoldsPredict(trainDSDer, cvFolds, models2, kaggle.africa.glmTop5.predict)

evRes2 <- kaggle.africa.kfEvaluate(predictions2, cvFolds, trainDSDer)

colMeans(evRes2)
mean(colMeans(evRes2))

predTest2 <- kaggle.africa.glmTop5.predict(models2, testDSDer)
evRes2 <- kaggle.africa.evaluateV(predTest2, testDSDer)
evRes2
mean(evRes2)
#//////////////////////////////////////////////////////

models3 <- kaggle.africa.glmTop5.train(trainDSDer, c(1:3563, 3565:3579),  targetsAll, 0.15, 'glm')
predictions3 <- kaggle.africa.kfoldsPredict(trainDSDer, cvFolds, models3, kaggle.africa.glmTop5.predict)

evRes3 <- kaggle.africa.kfEvaluate(predictions3, cvFolds, trainDSDer)

colMeans(evRes3)
mean(colMeans(evRes3))

predTest3 <- kaggle.africa.glmTop5.predict(models3, testDSDer)
evRes3 <- kaggle.africa.evaluateV(predTest3, testDSDer)
evRes3
mean(evRes3)
#//////////////////////////////////////////////////////

#//////////////////////////////////////////////////////

kagglePredict <- kaggle.africa.glmTop5.predict(models, kaggleDSDer)

write.csv(kagglePredict, file = "data/glmSubmit.csv", row.names = FALSE)