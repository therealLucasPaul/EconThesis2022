#Load packages
library(readxl)
library(ggplot2)
library(scales)

#Set WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("~/GitHub/BScThesis/01_data")

#Data cleaning 
inbound_data <- read_excel("unwto-inbound-expenditure-data.xlsx")
inbound_data <- data.frame(t(inbound_data[c(2,48),12:36])) #get AUS
inbound_data[,1] <- as.numeric(inbound_data[,1])
inbound_data[,2] <- as.numeric(inbound_data[,2])
colnames(inbound_data) <- c("Year","" )

inbound_arrivals <- read_excel("unwto-inbound-arrivals-data.xlsx")
inbound_arrivals <- data.frame(t(inbound_arrivals[c(2,71),12:36])) #get AUS
inbound_arrivals[,1] <- as.numeric(inbound_arrivals[,1])
inbound_arrivals[,2] <- as.numeric(inbound_arrivals[,2])
colnames(inbound_arrivals) <- c("Year","Arrivals" )

#Main Code
setwd("~/GitHub/BScThesis/02_figures")
ggplot(inbound_arrivals, aes(x=Year, y=Arrivals)) + 
  geom_line(col="steelblue", lwd=1.5)+
  theme(text = element_text(size = 15), axis.text = element_text(size = 15),
        panel.background = element_rect(fill = "lightgrey",
                                        colour = "lightgrey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"))+
  ylab("Total arrivals (in 1000)")+
  scale_y_continuous(limits=c(4000,10000),oob = rescale_none)



#Print Plots
setwd("~/GitHub/BScThesis/02_figures")
dev.print(file="CH01_TotalArrivals.png", device=png, width=400)

ggplot(inbound_data, aes(x=Year, y=Exp)) + 
  geom_line()+
  ylab("Inbound Expenditure in Million USD")+
  ggtitle("Inbound expenditure in Million USD (Prototype)")


#Print Plots
setwd("~/GitHub/BScThesis/02_figures")
dev.print(file="CH01_InboundExp.png", device=png, width=400)

