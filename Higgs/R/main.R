library('caret')
library('Hmisc')
library('doSNOW')

dataSet <- read.csv('../training.csv')

summary(dataSet[dataSet$Label=='b',])
summary(dataSet[dataSet$Label=='s',])

head(dataSet[dataSet$Label=='s',])
as.factor(dataSet[dataSet$Label=='s',]$Weight)

sampleData <- createDataPartition(dataSet$Label, p=0.1, list=FALSE)
sampleData <- dataSet[sampleData,]


str(sampleData)

#При значениях -1.666 - 1.117 видно увеличение
qplot(cut2(sampleData$PRI_tau_eta, g=15), data=sampleData, fill=Label, geom='bar')
#Не видно корреляции
qplot(cut2(sampleData$PRI_tau_phi, g=5), data=sampleData, fill=Label, geom='bar')
#Не видно корреляции
qplot(cut2(sampleData$PRI_lep_pt, g=5), data=sampleData, fill=Label, geom='bar')
#Не видно корреляции
qplot(cut2(sampleData$PRI_lep_phi, g=5), data=sampleData, fill=Label, geom='bar')
#Не видно корреляции
qplot(cut2(sampleData$PRI_met_phi, g=5), data=sampleData, fill=Label, geom='bar')
#С виду пропорции сохраняются
qplot(cut2(sampleData$PRI_jet_num, g=5), data=sampleData, fill=Label, geom='bar')
#Не видно корреляции
qplot(cut2(sampleData$PRI_jet_leading_phi, g=5), data=sampleData, fill=Label, geom='bar')
#Не видно корреляции
qplot(cut2(sampleData$PRI_jet_subleading_pt, g=5), data=sampleData, fill=Label, geom='bar')
#Не видно корреляции
qplot(cut2(sampleData$PRI_jet_subleading_eta, g=5), data=sampleData, fill=Label, geom='bar')
#Не видно корреляции
qplot(cut2(sampleData$PRI_jet_subleading_phi, g=5), data=sampleData, fill=Label, geom='bar')


#Далее производные поля
#Не видно корреляции
qplot(cut2(sampleData$DER_deltaeta_jet_jet, g=5), data=sampleData, fill=Label, geom='bar')
#Не видно корреляции
qplot(cut2(sampleData$DER_mass_jet_jet, g=5), data=sampleData, fill=Label, geom='bar')
#При значениях меньше -1.850 возможен тренд
qplot(cut2(sampleData$DER_prodeta_jet_jet, g=10), data=sampleData, fill=Label, geom='bar')
#От 3 до 3.20 видно небольшое увеличение значения, но в целом тренда нет
qplot(cut2(sampleData$DER_deltar_tau_lep, g=10), data=sampleData, fill=Label, geom='bar')
#Не видно корреляции
qplot(cut2(sampleData$DER_pt_tot, g=10), data=sampleData, fill=Label, geom='bar')
#Возможен тренд при увеличении
qplot(cut2(sampleData$DER_sum_pt, g=10), data=sampleData, fill=Label, geom='bar')
#Похоже на тренд при увеличении, но много пропущенных значений
qplot(cut2(sampleData$DER_lep_eta_centrality, g=15), data=sampleData, fill=Label, geom='bar')


############################################################################################
sampleForTrain <- sampleData[,c('PRI_tau_pt', 'PRI_lep_eta', 'PRI_met', 'PRI_met_sumet', 'PRI_jet_leading_pt', 'PRI_jet_leading_eta', 'PRI_jet_all_pt', 'DER_mass_MMC', 'DER_mass_transverse_met_lep', 'DER_mass_vis', 'DER_pt_h', 'DER_pt_ratio_lep_tau', 'DER_met_phi_centrality', 'Label')]

fullForTrain <- dataSet[,c('PRI_tau_pt', 'PRI_lep_eta', 'PRI_met', 'PRI_met_sumet', 'PRI_jet_leading_pt', 'PRI_jet_leading_eta', 'PRI_jet_all_pt', 'DER_mass_MMC', 'DER_mass_transverse_met_lep', 'DER_mass_vis', 'DER_pt_h', 'DER_pt_ratio_lep_tau', 'DER_met_phi_centrality', 'Label')]

getTrainData1 <- function(ds) {
  ds[,c('PRI_tau_pt', 'PRI_lep_eta', 'PRI_met', 'PRI_met_sumet', 'PRI_jet_leading_pt', 'PRI_jet_leading_eta', 'PRI_jet_all_pt', 'DER_mass_MMC', 'DER_mass_transverse_met_lep', 'DER_mass_vis', 'DER_pt_h', 'DER_pt_ratio_lep_tau', 'DER_met_phi_centrality')]
}

inTrain <- createDataPartition(fullForTrain$Label, p=0.75, list=FALSE)
restDS <- fullForTrain[inTrain,]
testDS <- fullForTrain[-inTrain,]

inCV <- createDataPartition(restDS$Label, p=0.15, list=FALSE)
cvDS <- restDS[inCV,]

trainDS <- restDS[-inCV,]

modelFit <- train(Label ~ ., method="gbm", data=trainDS)
summary(trainDS)
summary(cvDS)
testPr <- predict(modelFit, testDS)
confusionMatrix(testPr, testDS$Label)

cvPr <- predict(modelFit, cvDS)
summary(testDS)
confusionMatrix(cvPr, cvDS$Label)

############################################################################################
testKaggle <- read.csv('../test.csv')
summary(testKaggle)

tkSample1 <- getTrainData1(testKaggle)
tkPr <- predict(modelFit, tkSample1)

submit <- data.frame(EventId=testKaggle$EventId,RankOrder=1:nrow(testKaggle),Class=tkPr)
write.csv(submit, file = "submit.csv", row.names = FALSE)