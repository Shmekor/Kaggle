source('pcaModel.R')
source('evaluationMetrics.R')

models <- kaggle.africa.pcaModel.train(trainDSDer, c(2:3563, 3565:3579),  targetsAll, 0.05, 'glm')
predictions <- kaggle.africa.kfoldsPredict(trainDSDer, cvFolds, models, kaggle.africa.pcaModel.predict)

evRes <- kaggle.africa.kfEvaluate(predictions, cvFolds, trainDSDer)

colMeans(evRes)
mean(colMeans(evRes))

predTest <- kaggle.africa.pcaModel.predict(models, testDSDer)
evRes <- kaggle.africa.evaluateV(predTest, testDSDer)
evRes
mean(evRes)
