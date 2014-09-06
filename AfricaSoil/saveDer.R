source('prepare.R')

write.csv(trainDSDer, file = "data/trainingDer.csv", row.names = FALSE)
write.csv(testDSDer, file = "data/testDer.csv", row.names = FALSE)
write.csv(kaggleDSDer, file = "data/kaggleDer.csv", row.names = FALSE)
