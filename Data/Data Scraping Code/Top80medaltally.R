data <- read.csv("paris-paralympic-2024-medal-table-3.csv")
data <- data[1:80,]

data$G_Female <- as.numeric(data$G_Female)
data$G_Male <- as.numeric(data$G_Male)
data$G_Mixed <- as.numeric(data$G_Mixed)
data$S_Female <- as.numeric(data$S_Female)

T_Gold <- data$G_Female + data$G_Male + data$G_Mixed
T_Silver <- data$S_Female + data$S_Male + data$S_Mixed
T_Bronze <- data$B_Female + data$B_Male + data$B_Mixed
data$T_Gold <- T_Gold
data$T_Silver <- T_Silver
data$T_Bronze<-T_Bronze

Top80medaltally<-data
save(Top80medaltally,file="Top80medaltally.Rdata")
