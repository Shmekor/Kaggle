library('caret')
library('Hmisc')

dataSet <- read.csv('../training.csv')

summary(dataSet)

sampleData <- createDataPartition(dataSet$Label, p=0.1, list=FALSE)
sampleData <- dataSet[sampleData,]

#����� ��� ���������� ��������
qplot(cut2(sampleData$PRI_tau_pt, g=10), data=sampleData, fill=Label, geom='bar')
#����� � ������� ����������
qplot(cut2(sampleData$PRI_lep_eta, g=10), data=sampleData, fill=Label, geom='bar')
#���������� �� ���������� �������� (57.5)
qplot(cut2(sampleData$PRI_met, g=10), data=sampleData, fill=Label, geom='bar')
#����� ��� ���������� �������� �� ������ (289)
qplot(cut2(sampleData$PRI_met_sumet, g=20), data=sampleData, fill=Label, geom='bar')
#��������� ����� ��� ���������� ��������, �� ����� ����������� (-999)
qplot(cut2(sampleData$PRI_jet_leading_pt, g=20), data=sampleData, fill=Label, geom='bar')
#����� ����������� ��������, �� ������������ ������ ���������� �� �����
qplot(cut2(sampleData$PRI_jet_leading_eta, g=20), data=sampleData, fill=Label, geom='bar')
#������ ����� ��� ���������� ��������
qplot(cut2(sampleData$PRI_jet_all_pt, g=20), data=sampleData, fill=Label, geom='bar')

#����������� ��������
#���������� ������������� � �������� ����� 125-138
qplot(cut2(sampleData$DER_mass_MMC, g=10), data=sampleData, fill=Label, geom='bar')
#����� ��� ���������� ��������
qplot(cut2(sampleData$DER_mass_transverse_met_lep, g=5), data=sampleData, fill=Label, geom='bar')
#������������� � �������� ����� 80-98
qplot(cut2(sampleData$DER_mass_vis, g=10), data=sampleData, fill=Label, geom='bar')
#����� ��� ���������� ����� 38
qplot(cut2(sampleData$DER_pt_h, g=10), data=sampleData, fill=Label, geom='bar')
#����� ��� ����������
qplot(cut2(sampleData$DER_pt_ratio_lep_tau, g=10), data=sampleData, fill=Label, geom='bar')
#���������� ��� �������� ������ 0
qplot(cut2(sampleData$DER_met_phi_centrality, g=15), data=sampleData, fill=Label, geom='bar')

qplot(DER_met_phi_centrality, PRI_tau_pt, data=sampleData)

