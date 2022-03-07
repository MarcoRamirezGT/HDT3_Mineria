library(tibbletime)
library(dplyr)
library(tidyverse)
library(scales)
library(ggplot2)


data<-read.csv('train.csv')
View(data)
summary(data)


#Pregunta 1
#¿En qué zona se han hecho más ventas?
zona<-data[,'MSZoning']
df<-data.frame(zona)
table_zona<-table(df$zona)
view(table_zona)

lbls <- c("C (all)", "FV", "RH", "RL", "RM")
pct <- round(table_zona/sum(table_zona)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pich<-pie(table_zona, labels = lbls, col=rainbow(length(lbls)), main="")
