#Load packages
library(readxl)
library(ggplot2)
library(igraph)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(magrittr)
library(forcats)
library(dplyr)
library(psych)
library(GGally)

#Set WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("~/GitHub/BScThesis/01_data")

#Data import & cleaning
vacActivities <- data.frame(read.csv("ausActiv.csv"))
View(vacActivities)

vacActivitiesDesc <- data.frame(read.csv("ausActivDesc.csv"))
View(vacActivitiesDesc)

categories <- data.frame(names = colnames(vacActivities))
categories["category"] <- 0
categories <- edit(categories) # Sport = 1, Culture = 2, Misc = 3
categories <- categories[order(categories$category), ]
save(categories, file = "cat.Rda") 

#Descriptive Analysis
setwd("~/GitHub/BScThesis/02_figures")
str(vacActivities)

sample <- vacActivities[1:10,1:10] # Get first then rows and columns

colSums <- colSums(vacActivities)
rowSums <- rowSums(vacActivities)
leastcommonAct <- data.frame(count=sort(colSums)[1:10])
mostcommonAct <- data.frame(count=sort(colSums)[36:45])

## Upper ten activities
ggplot(data=leastcommonAct, aes(x=count, y=reorder(rownames(leastcommonAct), -count), order = count))+
  geom_bar(stat="identity",, fill="steelblue", alpha=0.6, width=0.6)+
  geom_text(aes(label=count), hjust=1.5,vjust=0.3, size=4, col="white")+
  xlab("Number of occurences")+
  ylab("Actitivy")+
  scale_fill_grey()+
  theme(text = element_text(size = 15), axis.text = element_text(size = 15))
dev.print(file="CH03_Barchart_leastcommon.png", device=png, width=600, height=500) 

## Lower ten activities
ggplot(data=mostcommonAct, aes(x=count, y=reorder(rownames(mostcommonAct), count), order = count))+
  geom_bar(stat="identity", fill="steelblue", alpha=0.6, width=0.6)+
  geom_text(aes(label=count), hjust=1.5,vjust=0.3, size=4, col="white")+
  xlab("Number of occurences")+
  ylab("Actitivy")+
  scale_fill_grey()+
  theme(text = element_text(size = 15), axis.text = element_text(size = 15))
dev.print(file="CH03_Barchart_mostcommon.png", device=png, width=600, height=500)   

## Hamming
correlationstable <- data.frame(col1 = 0, col2 = 0, hamming = 0)
columns <- seq(1,ncol(vacActivities),1)
combinations <- t(combn(columns, 2))
for(i in seq(nrow(combinations))){
  a <- combinations[i,][1]
  b <- combinations[i,][2]
  hamming <- sum(vacActivities[,a]!=vacActivities[,b])
  correlationstable <- rbind(correlationstable, c(a,b,hamming))
}

lowestHamming <- correlationstable[order(correlationstable$hamming, decreasing=F),][2:11,]
translatet <- lowestHamming
cnames <- colnames(vacActivities)
for(i in seq(1,10)){
  translatet[i,1] <- cnames[lowestHamming[i,1]]
  translatet[i,2] <- cnames[lowestHamming[i,2]]
}
print(knitr::kable(translatet, "latex"))

## Average number of activites
ggplot(data.frame(count = rowSums), aes(x=count)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="steelblue")+
  geom_vline(aes(xintercept=mean(count)),
             color="steelblue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=summary(rowSums)[2]),
             color="firebrick", linetype="dashed", size=0.8)+
  geom_vline(aes(xintercept=summary(rowSums)[5]),
             color="firebrick", linetype="dashed", size=0.8)+
  annotate("rect", xmin = 25, xmax = 37, ymin = 0.065, ymax = 0.08,
           alpha = 1, fill="white", col="white")+
  annotate("text", x = 28, y = 0.077, label = "Mean:", cex=5, col="steelblue")+
  annotate("text", x = 28, y = 0.073, label = "1st Qu.:", cex=5, col="firebrick")+
  annotate("text", x = 28, y = 0.069, label = "3st Qu.:", cex=5, col="firebrick")+
  annotate("text", x = 32, y = 0.077, label = round(mean(rowSums),2), cex=5, col="steelblue")+
  annotate("text", x = 32, y = 0.073, label = round(summary(rowSums)[2],2), cex=5, col="firebrick")+
  annotate("text", x = 32, y = 0.069, label = round(summary(rowSums)[5],2), cex=5, col="firebrick")+
  xlab("Average number of activities")+
  ylab("Density")+
  theme(legend.position = "right", text = element_text(size = 15), axis.text = element_text(size = 15))
dev.print(file="CH03_Histogram.png", device=png, width=764, height=500)

print(knitr::kable(sample, "latex"))
summary(rowSums)[2]



kable(vacActivities[1:10,1:10]) %>%
  kable_styling("striped", full_width = FALSE, htmltable_class = 'lightable-classic-2') %>%
  add_header_above(c("Measurements" = 9L, " " = 1L)) %>% 
  kable_paper() %>%
  save_kable(file = 'tableX.png') 



#Network graph
links <- data.frame(source=c("Activity","Activity","Activity",
                             "Sports","Sports","Sports","Sports","Sports","Sports","Sports","Sports","Sports","Sports","Sports","Sports","Sports","Sports","Sports","Sports","Sports","Sports",
                             "Culture","Culture","Culture","Culture","Culture","Culture","Culture","Culture","Culture","Culture",
                             "Misc.","Misc.","Misc.","Misc.","Misc.","Misc.","Misc.","Misc.","Misc.","Misc.","Misc.","Misc.","Misc.","Misc.","Misc.","Misc.","Misc."),
                    target=c(c("Sports","Culture","Misc.",categories$names)))
nodes <- data.frame(name=c("Activity","Sports","Culture","Misc.",categories$names),
                    carac=c(rep("Activity",4),rep("Sports",18),rep("Culture",10), rep("Misc.",17)))
network <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 
coul  <- brewer.pal(4, "Accent") 
my_color <- coul[as.numeric(as.factor(V(network)$carac))]
V(network)$label.cex=0.6
V(network)
node.size<-setNames(c(40, rep(25,3),rep(11,45)),c("Activity","Sports","Culture","Misc.",categories$names))
plot(network,vertex.label.family="sans", vertex.color=my_color,vertex.label.color="black",vertex.size=as.matrix(node.size))
setwd("~/GitHub/BScThesis/02_figures")
dev.print(file="CH03_Network.png", device=png, width=2000)                    
