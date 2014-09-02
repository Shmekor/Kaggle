library('caret')
library('Metrics')

setwd("~/Documents/Workspace/DataScience/Kaggle/AfricaSoil")

trainDS <- read.csv('data/training.csv', stringsAsFactors=FALSE)

#1 - ID, 2-3579 - spectroscopy features
soilId = 1
ssFeatures = names(trainDS)[2:3579]
addFeatures = names(trainDS)[3580:3594]
depth = 3595
targetCa = 3596
targetP = 3597
targetpH = 3598
targetSOC = 3599
targetSand = 3600
targetsAll = 3596:3600