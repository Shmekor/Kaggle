qplot(Survived, data=trainDS, fill=HasRelatives, geom='bar')
featurePlot(trainDS, y=trainDS$Survived, plot='pairs')