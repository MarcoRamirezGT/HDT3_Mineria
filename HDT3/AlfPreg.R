# Preguntas Alfredo 

#Preguna 1
library(ggplot2)
datos<-read.csv('train.csv')
id<-datos[,'Id']
precio<-datos[,'SalePrice']
q1<-data.frame(id, precio)
ask1<-q1[order(-q1$precio),]
ask1f<-head(ask1,n=5)
View(ask1f)

ggplot(data=ask1f, aes(x=reorder(id,-precio) , y=precio,fill=id)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title=" Cual es el precio de venta mas alto registrado?", x="Id de la propiedad", y="Precio")


#Pregunta 2
id<-datos[,'Id']
esti<-datos[,'HouseStyle']
df<-data.frame(esti)
ndf<-aggregate(df$esti, df, length)
q2<-data.frame(ndf)
q3<-head(order(-q2),)

View(q3)

ggplot(data=q3, aes(x=reorder(esti, x) , y=x,fill=esti)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title=" Que estilo de vivienda es el mas vendido?", x="Estilo de la vivienda", y="Veces vendida")



#Pregunta 3 
esven<-datos[,'SaleType']
de<-data.frame(esven)
nde<-aggregate(de$esven, de, length)

ggplot(data=nde, aes(x=reorder(esven, x) , y=x,fill=esven)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="  Cuales son los tipos de ventas mas utilizados?", x="Tipo de venta", y="Veces utiizada")


#Pregunta 4 

condi<-datos[, 'SaleCondition']
dc<-data.frame(condi)
ndc<-aggregate(dc$condi, dc, length)
View(ndc)

ggplot(data=ndc, aes(x=reorder(condi, x) , y=x,fill=condi)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="En que condiciones son vendidas y cual es la que sobresale", x="COndiciones", y="Veces utilizada")


#Pregunta 5 
mesi<-datos[,'MoSold']
dm<-data.frame(mesi)
ndm<-aggregate(dm$mesi, dm, length)
View(ndm)

ggplot(data=ndm, aes(x=reorder(mesi, x) , y=x,fill=mesi)) +
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Cual es el mes que registra una mayor venta?", x="Mes de la compra", y="Veces utilizada")




#ARBOL DE REGRESION
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)

train<-data[1:1460,]



porciento <- 70/100

datos <- data[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd", "SalePrice")]
datos <- na.omit(datos)

train <-na.omit(datos)
datosFiltertree <- data[,c("LotFrontage","LotArea","GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","GarageYrBlt","GarageArea","YearRemodAdd", "Estado")]



set.seed(123)
trainRowsNumber<-sample(1:nrow(datosFiltertree),porciento*nrow(datosFiltertree))
train<-datosFiltertree[trainRowsNumber,]
test<-datosFiltertree[-trainRowsNumber,]

modeloRF1<-randomForest(train$Estado~.,train)
prediccionRF1<-predict(modeloRF1,newdata = test)
testCompleto<-test
testCompleto$predRF<-prediccionR…







