library(tibbletime)
library(dplyr)
library(tidyverse)


data<-read.csv('train.csv')
View(data)

#Pregunta 1

data<-data[order(-data$YearBuilt),]
data<-head(data,n=10)
str(data$YearBuilt)

ggplot(data=data, aes(x=reorder(Id,SalePrice) , y=SalePrice,fill=factor(BldgType) )) +
  geom_bar(stat="identity")+theme_minimal()+
  scale_y_continuous(labels=scales::dollar)
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Top 10 peliculas con mas ingresos", x="Peliculas", y="Ingresos")


data<-data[order(-data$SalePrice),]

  
data$BldgType[1]
View(data)  
#Pregunta 2


#Cuantas casas se han remodelado
data<-read.csv('train.csv')


A<-data$YearBuilt==data$YearRemodAdd
Total<-length(A)

Remodelada<-sum(A, na.rm = TRUE)
NoRemodelada<-length(A)-Remodelada

# Pie Chart with Percentages
slices <- c(Remodelada, NoRemodelada)
lbls <- c("Remodelada", "No remodelada")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pich<-pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Porcentaje de casas remodeladas")
  


#Pregunta 3
library(dplyr)
library(scales)
library(ggplot2)

data<-read.csv('train.csv')

data<-data[order(-data$LotArea),]
data<-head(data,n=5)

View(data)

ggplot(data=data, aes(x=reorder(Id,LotArea) , y=LotArea,fill=Id )) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = unit_format(unit = "ft^2", scale = 1))
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
  
  
#Pregunta 4
  
  library(dplyr)
  library(scales)
  library(ggplot2)
  
  data<-read.csv('train.csv')
  
  tipoVivienda<-data[,'BldgType']
  punt<-data[,'OverallCond'] 
  acabado<-data[,'OverallQual'] 
  
  
  
  ask4<-data.frame(tipoVivienda,punt,acabado)

  
  ggplot(data, aes(tipoVivienda, punt, fill=tipoVivienda)) +           
    geom_bar(position = "dodge",
             stat = "summary",
             fun = "mean",
             show.legend=FALSE)+labs(title="Tipos de vivienda con mejor puntuacion en estado de casa")
              

    
  
  ggplot(data, aes(tipoVivienda, acabado, fill=tipoVivienda)) +           
    geom_bar(position = "dodge",
             stat = "summary",
             fun = "mean")+labs(title="Tipos de vivienda con mejor puntuacion en acabados")
  
  
#Calculo de percentiles
  
  data<-read.csv('train.csv')
  percentil <- quantile(data$SalePrice)
percentil
  percentil[2]


ggplot(data = data, aes(SalePrice, color = 'red')) +
  geom_histogram(position = "stack", bins = 30)



#Percentiles
data<-read.csv('train.csv')

estado<-c('Estado')

data$Estado<-estado


data <- within(data, Estado[SalePrice<=129975] <- 'Economica')

data$Estado[(data$SalePrice>129975 & data$SalePrice<=163000)] <- "Intermedio"
data$Estado[data$SalePrice>163000] <- "Cara"
# data[data$SalePrice<129975 ,] <- 'Barato'

ts<-data %>%
  group_by(Estado) %>%
  tally()



View(ts)
ggplot(data=ts, aes(x=Estado , y=n,fill=Estado )) +
  geom_bar(stat="summary",show.legend=FALSE)+labs(title="Cantidad de precios de viviendas")


#Tree


library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(randomForest)
View(data)

porciento <- 70/100


#Variables numericas
datos <- data[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd", "SalePrice")]

#Variables con enfoque en Grupo
datosFiltertree <- data[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd", "Estado")]


set.seed(123)
trainRowsNumber<-sample(1:nrow(datosFiltertree),porciento*nrow(datosFiltertree))
train<-datosFiltertree[trainRowsNumber,]
test<-datosFiltertree[-trainRowsNumber,]

dt_model<-rpart(train$Estado~.,train,method = "class")
rpart.plot(dt_model)

prediccion <- predict(dt_model, newdata = test[1:10])

columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test$prediccion<-columnaMasAlta

cfm<-table(test$Estado,test$prediccion)
cfm













