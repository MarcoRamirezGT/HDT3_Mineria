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


#Pregunta 3
#¿Qué forma tienen las viviendas que más se han vendido?
id<-datos[,'Id']
forma<-datos[,'LotShape']
df2<-data.frame(forma)
table_forma<-table(df2$forma)
view(table_forma)
ndf<-aggregate(df2$forma, df2, length)
q<-data.frame(ndf)
q1<-head(q)


ggplot(data=q1, aes(x=reorder(forma, x) , y=x,fill=forma)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Que forma tienen las viviendas que más se han vendido?", x="Forma de la vivienda", y="Cantidad")


#Pregunta 4
#Pregunta 5


#Graficos de dispersion
#Correlacion de las variables a utilizar vs el precio de venta
scatter.smooth(data$LotFrontage, data$SalePrice)
scatter.smooth(data$LotArea, data$SalePrice)
scatter.smooth(data$BsmtUnfSF, data$SalePrice)
scatter.smooth(data$TotalBsmtSF, data$SalePrice)
scatter.smooth(data$X1stFlrSF, data$SalePrice)
scatter.smooth(data$GrLivArea, data$SalePrice)
scatter.smooth(data$GarageArea, data$SalePrice)
scatter.smooth(data$YearBuilt, data$SalePrice)
scatter.smooth(data$YearRemodAdd, data$SalePrice)
scatter.smooth(data$GarageYrBlt, data$SalePrice)
scatter.smooth(data$MoSold, data$SalePrice)
