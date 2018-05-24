#Cargamos la base de datos
BD<-read.csv("C:/Users/Rodrigo Barran Zubar/Documents/DATANK/yellow_tripdata_2016-01.csv",header=TRUE,sep=",")


#Limpiamos los datos atipicos


# que la tarifa calculada por tiempo y distancia sea mayor a cero 
BDFiltrada<-BD[BD$fare_amount>=0,]

#Arreglamos RatecodeID que sea entre 1 y 6
BDFiltrada<-BDFiltrada[BDFiltrada$RatecodeID<7,]

#La propina debe de ser positiva
BDFiltrada<-BDFiltrada[BDFiltrada$tip_amount>=0,]

#cargos por banco deben de ser positivos
BDFiltrada<-BDFiltrada[BDFiltrada$mta_tax>=0,]

#Nos dimos cuenta que la BD medio mal y tienes cosas muy ilogicas 
#filtramos distancia y total a pagar
BDFiltrada<-BDFiltrada[BDFiltrada$trip_distance<100 | BDFiltrada$total_amount>100 ,]

#la distancia debe ser mayor a cero si no ¿¿¿a donde lo llevo???
BDFiltrada<-BDFiltrada[BDFiltrada$trip_distance>0,]

#teniamos datos con long y lat en e mar o en Europa por lo que restringimos la distancia
BDFiltrada<-BDFiltrada[BDFiltrada$pickup_latitude>30 & BDFiltrada$dropoff_latitude>30,]
BDFiltrada<-BDFiltrada[BDFiltrada$pickup_longitude<(-60) & BDFiltrada$dropoff_longitude<(-60),]

dim(BD)-dim(BDFiltrada)

#visualizar la BD (simpre se traba) NO CORRER O ESPERAREMOS MUCHO TIEMPO 
View(BD)

# visulizar los nombres de las columnas
names(BDFiltrada)

#para obtener los mismos valores de las variables random
set.seed(1)

#cortamos nunestra base de datos para que no se tarde en todo
Indicadores<-sample(dim(BDFiltrada)[1],100000)
BDEntrenamiento<-BDFiltrada[Indicadores,]

#checamos los primeros datos
BDEntrenamiento[1:5,1:5]

#crear la BD de testeo, quitando los que usamos para la base de entrenamiento.
BDTesteo<-BDFiltrada[-Indicadores,]
IndicadorTesteo<-sample(dim(BDTesteo)[1],1000)
BDTesteo<-BDTesteo[IndicadorTesteo,]

#checamos los primeros datos 
BDTesteo[1:5,1:5]

names(BDEntrenamiento)

#Cambiamos los valores de VendorID por factores
BDEntrenamiento$VendorID<-as.factor(BDEntrenamiento$VendorID)
BDTesteo$VendorID<-as.factor(BDTesteo$VendorID)

#Cambiamos los valores de RatecodeID por factores
BDEntrenamiento$RatecodeID<-as.factor(BDEntrenamiento$RatecodeID)
BDTesteo$RatecodeID<-as.factor(BDTesteo$RatecodeID)

#Cambiamos los valores de RatecodeID por factores
BDEntrenamiento$payment_type<-as.factor(BDEntrenamiento$payment_type)
BDTesteo$payment_type<-as.factor(BDTesteo$payment_type)


## PRIMER MODELO LINEAL 

#Modelo lineal para calcular total amount usando fare_amount
modelolineal<-lm(total_amount~fare_amount,data = BDEntrenamiento)
#Vemos los valores del modelo lineal
summary(modelolineal)

#damos los valores que usaremos para calcular y comparar usando el modelolineal
total_amount<-BDTesteo$total_amount
fare_amount<-BDTesteo$fare_amount

#Los datos que usaremos para calcular
Q<-cbind(total_amount,fare_amount)
Q<-as.data.frame(Q)

#predecimos usando el algoritmo de modelo lineal
predecidos<-predict(modelolineal,newdata=Q)
 
#los datos que usaremos para comparar
originales<-BDTesteo$total_amount

#checamos que tipo de datos son 
class(originales)

#Usamos view para ver si se parecen los datos 
View(cbind(predecidos,originales))

#calulamos que tan diferentes son los datos
errorlineal<-function(a,b){
                c<-(a-b)^2
                return(c)}

error<-errorlineal(predecidos,originales)
plot(error)
class(error)

View(error)

#OTRA APROXIMACION LINEAL, CALCULAMOS TOTAL_AMOUNT CON FARE_AMOUNT, TIP_AMOUNT Y RATECODEID


modelolineal2<-lm(total_amount~fare_amount+tip_amount,data = BDEntrenamiento)

summary(modelolineal2)


QQ<-cbind(BDTesteo$total_amount,BDTesteo$fare_amount,BDTesteo$tip_amount)
QQ<-as.data.frame(QQ)

predecidos2<-predict(modelolineal2,newdata=QQ)

originales2<-BDTesteo$total_amount


View(cbind(predecidos2,originales2))


# APROXIMACION LINEAL, CALCULAMOS TOTAL_AMOUNT CON DISTANCIA


modelolineal3<-lm(trip_distance~total_amount,data = BDEntrenamiento)

summary(modelolineal3)

QQQ<-cbind(BDTesteo$trip_distance,BDTesteo$total_amount)
QQQ<-as.data.frame(QQQ)


predecidos3<-predict(modelolineal3,newdata=QQQ)

originales3<-BDTesteo$trip_distance


View(cbind(predecidos3,originales3))


boxplot(total_amount,main = "Cantidad de Dinero")
hist(total_amount,main = "Cantidad de Dinero")


################################

## LOGISTICA
install.packages("nnet")
library(nnet)


#primera regresión para calcular RatecodeID

modelologistico1<-multinom(RatecodeID~fare_amount+mta_tax+tip_amount,data = BDEntrenamiento)
summary(modelologistico1)

nuevos<-predict(modelologistico1, newdata = BDTesteo)

View(cbind(nuevos,BDTesteo$RatecodeID))

#segunda regresión logisitca para calcular payment_type
modelologistico2<-multinom(payment_type~total_amount,data = BDEntrenamiento)
summary(modelologistico2)

nuevos2<-predict(modelologistico2, newdata = BDTesteo)

View(cbind(nuevos2,BDTesteo$payment_type))



##################
#  SQL

install.packages("sqldf")
library(sqldf)

SQL1<-sqldf("select(total_amount) as Cantidad_Total from BDEntrenamiento")
View(SQL1)

SQL2<-sqldf("select count(total_amount) as Viajes_Totales from BDEntrenamiento group by RateCodeID")
View(SQL2)
porcentajes<-SQL2/colSums(SQL2)
porcentajes
d<-barplot(porcentajes$Viajes_Totales,main = "Proporcion de Viajes por tipo de Tarifa",col = rainbow(5),ylim = c(0,1))
axis(1, at=d, labels=c(" Standard rate","JFK","Newark","Nassau or Westchester","Negotiated fare"))

SQL3<-sqldf("select count(total_amount) as Total_Cantidad from BDEntrenamiento group by Payment_type")
View(SQL3)
porcentajes2<-SQL3/colSums(SQL3)
porcentajes2
dd<-barplot(porcentajes2$Total_Cantidad,xlab = "",main = "Proporcion de Dinero por tipo de Tarifa",col = rainbow(5),ylim = c(0,1))
axis(1, at=d,labels=c("Credit card","Cash"," No charge"," Dispute","Unknown"))



################
# Box Plot 

rengodeprecios<- BDEntrenamiento[,19]
boxplot(rangodeprecios, main = "Rango de precios",col = 5)



rangodedistancias<-BDEntrenamiento[,5]
boxplot(rangodedistancias, main="Rango de Distancias",col= 6)


#############
# Correlaciones 
BDPeque<-BDEntrenamiento[,c(4,5,13:17,19)]

install.packages("corrplot")
library(corrplot)

MC<- round(cor(BDPeque),5)
View(MC)
corrplot(MC)
coplot(trip_distance ~ total_amount | RatecodeID, data = BDEntrenamiento)

###############
#Componentes principales
library(FactoMineR)
BDPCA<-cbind(BDEntrenamiento$VendorID,BDEntrenamiento$passenger_count,BDEntrenamiento$tip_amount, BDEntrenamiento$fare_amount)
PCA(BDPCA,quali.sup = 1,scale.unit = TRUE)




BDMatlab<-BDEntrenamiento[,c(6,7,10,11)]
write.csv(BDMatlab,"BDMatlab.csv",row.names=FALSE)
colums