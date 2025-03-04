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


#Pregunta 2
#¿Qué tipo de carretera de acceso tienen la mayoría de las viviendas?
calle<-data[,'Street']
df1<-data.frame(calle)
table_calle<-table(df1$calle)
view(table_calle)

lbls1 <- c("Gravel", "Paved")
pct1 <- round(table_calle/sum(table_calle)*100)
lbls1 <- paste(lbls1, pct1)
lbls1 <- paste(lbls1,"%",sep="")
pich1<-pie(table_calle, labels = lbls1, col=rainbow(length(lbls1)), main="")
