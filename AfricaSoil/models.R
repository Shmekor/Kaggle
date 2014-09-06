source('prepare.R')
source('evaluationMetrics.R')
source('lassoModel.R')

models <- kaggle.africa.generalModel.train(trainDSDer, names(trainDSDer)[c(1:15, 17:3577)], targetsAll, 'lasso')
