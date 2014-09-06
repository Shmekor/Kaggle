source('prepare.R')

qplot(trainDS$Ca, trainDS[,ssFeatures[1]], data=trainDS)

tCor <- cor(trainDS[,targetsAll])
abs(tCor)

require('xgboost')

tDs <- as.matrix(trainDSDer))

dtrain <- xgb.DMatrix(as.matrix(trainDSDer[,c(1:15, 17:3577)]), label = trainDSDer$P)

bst <- xgboost(data = dtrain, max_depth = 3, eta = 0.5, nround = 60, objective = "reg:linear", eval_metric='rmse')

prRes <- predict(bst, as.matrix(trainDSDer[,c(1:15, 17:3577)]))

rmse(trainDSDer$Ca, prRes)
tprRes <- predict(bst, as.matrix(testDSDer[,c(1:15, 17:3577)]))
rmse(testDSDer$P, tprRes)


source('xgboostModel.R')

model <- kaggle.africa.xgboostModel.train(trainDSDer, c(1:15, 17:3577),  targetsAll, 1)
pred <- kaggle.africa.xgboostModel.predict(model, trainDSDer)
summary(pred)

ev <- kaggle.africa.evaluateV(pred, trainDSDer)
mean(ev)
