#Load packages
library(readxl)
library(ggplot2)
library(biclust)
library(MSA)
library(flexclust)
library(kernlab)
library(viridis)


library(FactoMineR) # Für LCM
library(MBCbook)

#Set WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("~/GitHub/BScThesis/01_data")

#Data import & cleaning
vacActivities <- data.frame(read.csv("ausActiv.csv"))
setwd("~/GitHub/BScThesis/02_figures")
for (i in seq(1,ncol(vacActivities))) {
  vacActivities[,i] <- as.factor(vacActivities[,i])
}


View(vacActivities)

data("vacmot",package="flexclust")
vacMot <- as.matrix(read.csv("vacation_motives.csv"))
View(vacMot)

#Main code - Activities
bicluster <- biclust::biclust(x=vacActivities,
                     method=BCrepBimax, 
                     minc=2, minr=50, number=100, maxc=100)
biclustmember(x=vacActivities, bicResult = bicluster)

data("vacmot",package="flexclust")

#Main code 
sc <- kernlab::specc(vacMotives, centers=2)
sc



################## Analysis through Latent Class Model (LCM) ################
res_activity = mixmodCluster(vacActivities, nbCluster=1:10, dataType="qualitative",
                          model=mixmodMultinomialModel(listModels = "Binary_pk_Ekj"), criterion=c("ICL"))

# Criterion comparison
nb_Cluster = c(1,2,3,4,5,6,7,8)
BIC_res = c(45924.8585, 42259.2516, 40760.3397, 40216.9568, 39799.2641, 39615.0704, 39599.3906, 39715.3073)
ICL_res = c(45924.8585, 42358.3106, 40982.4277, 40585.0744, 40192.5646, 40056.6824, 40099.5462, 40236.9255)
BIC_comparison <- data.frame(cbind(BIC_res, ICL_res), row.names = nb_Cluster)
plot(BIC_comparison$BIC_res, type="o",ylab="Criterion value",xlab="Number of clusters", col="red", main="Number of cluster comparison (ICL vs. BIC)")
lines(BIC_comparison$ICL_res, type="o", col="blue")
legend(x="topright", legend=c("BIC", "ICL"), col=c("red", "blue"), lty=1:2, cex=1.5)
res_activity

# Visualization of the best result according to BIC
par(mfrow=c(1,2))
lbl = res_activity@results[[1]]@partition 
acm = mca(vacActivities,abbrev = TRUE)
plot(predict(acm,vacActivities),col=lbl,pch=c(17,15,18,19)[lbl],
     xlab='',ylab='',main="Factor map of observations")
plot(acm,rows = FALSE,cex=0.75,main="Factor map of levels")


# Barplot of cluster-conditional level frequency
par(mfrow=c(1,1))
summary(res_activity)$BIC
png("plot_LCM.png", width = 4000, height = 4000)
barplot(res_activity)
dev.off()



################## Analysis through Finite Mixture of Bernoulli ################
library(flexmix)
bernoulliMixture <- stepFlexmix(as.matrix(vacActivities)~1, k=1:15, model = FLXMCmvbinary(), nrep=2, verbose=FALSE)
bernoulliMixture
best.Bernoulli <- getModel(bernoulliMixture)
best.Bernoulli
plot(bernoulliMixture)
dev.print(file="CH05_FMB_ModelComparison.png", device=png, width=764, height=500)

plot(best.Bernoulli, col=viridis(12))
vac.m6 <- getModel(bernoulliMixture, "6")

propRes <- propBarchart(as.matrix(vacActivities),clusters(best.Bernoulli), alpha = 1, strip.prefix = "Segment ")
dev.print(file="CH05_FMB_AllClustersA4.png", device=png, width=1000, height=1500)

propRes@table
propRes@chart[c(1,2)]
dev.print(file="CH05_FMB_Cluster12.png", device=png, width=764, height=600)
propRes@chart[c(3,4)]
propRes@chart[c(3,5)]
dev.print(file="CH05_FMB_Cluster34.png", device=png, width=764, height=600)
propRes@chart[c(5,6)]
propRes@chart[c(4,6)]
dev.print(file="CH05_FMB_Cluster56.png", device=png, width=764, height=600)

p <- round(parameters(best.Bernoulli),2)
p






################## Analysis through Bagged Clustering ################
act.baggedclust <- bclust(as.matrix(vacActivities), base.k = 6, base.iter = 50)
barchart(act.baggedclust, k=6)
dev.print(file="CH05_Bagged_AllClustersA4.png", device=png, width=1000, height=1500)



################## Analysis through Biclustering ################
act.biclust <- biclust(as.matrix(vacActivities), method = BCrepBimax, minc=2,minr=50, number=10, maxc=100)
save(act.biclust, file = "ausact-bic.RData")
biclustmember(as.matrix(vacActivities), bicResult = act.biclust)

################## Analysis with VSBD before ################
library(MSA)
vac.vsbd <- vsbd(vacmot, centers = 6, delta=0.5)
colnames(dat)[vac.vsbd]
#fpc::cluster.stats(dist(as.matrix(vacActivities)), clustering = 10)
#clusterSim::cluster.Sim(as.matrix(vacActivities), 5, minClusterNo = 2, maxClusterNo = 15, distances = c("manhatten"))
NbClust::NbClust(data=as.matrix(vacActivities), method="cent",min.nc=2, max.nc=45 ,index="kl")

dat = as.matrix(vacActivities)[,1:20]

vacmot
