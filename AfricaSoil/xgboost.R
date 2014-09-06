source('prepare.R')
source('xgboostModel.R')
source('xgboostTModel.R')
source('evaluationMetrics.R')

kfResults <- kaggle.africa.kfoldsTrain(trainDSDer, cvFolds, kaggle.africa.xgboostModel.train,
                                       c(1:15, 17:3577), max_depth = 3, eta = 0.5, nround = 60, verbose = 0)
kfPredict <- kaggle.africa.kfoldsPredict(kfResults, trainDSDer, kaggle.africa.xgboostModel.predict)
evMetrics <- kaggle.africa.kfEvaluate(kfPredict, kfResults, trainDSDer)

colMeans(evMetrics)
mean(colMeans(evMetrics))
summary(evMetrics)

#//////////////////////////////////////////////////////////////////////////////////////////////////
kfResults <- kaggle.africa.kfoldsTrain(trainDSDer, cvFolds, kaggle.africa.xgboostModel.train,
                                       c(1:15, 17:3577), booster='gbtree', eta=0.1,
                                       max_depth=3, subsample=1, nround=60, verbose=1, min_child_weight=2)
kfPredict <- kaggle.africa.kfoldsPredict(kfResults, trainDSDer, kaggle.africa.xgboostModel.predict)
evMetrics <- kaggle.africa.kfEvaluate(kfPredict, kfResults, trainDSDer)

colMeans(evMetrics)
mean(colMeans(evMetrics))

#//////////////////////////////////////////////////////////////////////////////////////////////////

kfResults <- kaggle.africa.kfoldsTrain(trainDSDer, cvFolds, kaggle.africa.xgboostModel.train,
                                       c(1:15))
kfPredict <- kaggle.africa.kfoldsPredict(kfResults, trainDSDer, kaggle.africa.xgboostModel.predict)
evMetrics <- kaggle.africa.kfEvaluate(kfPredict, kfResults, trainDSDer)

colMeans(evMetrics)
mean(colMeans(evMetrics))

#//////////////////////////////////////////////////////////////////////////////////////////////////

pkfRes <-
