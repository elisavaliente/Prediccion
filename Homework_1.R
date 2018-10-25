Data=read.csv("/Users/Elisa/Desktop/Data\ Science\ /Predicción/Homework_1/Fondos.csv", sep = ';', dec = ',')

head(Data)

View(Data)


#quitamos columnas con texto para poder realizar las regresiones sin problema. 
Data_notxt= Data[,c(-1,-2,-3,-5,-6,-17,-18,-19,-27,-28,-30,-31)]
View(Data_notxt)

#creo que variable dependiente. 
depvar = Data$rent_1
#creamos las variables independientes 
indepvar <- cbind(1, Data_notxt[,2:16])

head(depvar)
head.matrix(indepvar)

#Vamos a realizar el modelo lineal con todas las variables para ver su nivel de significación para así elegir las variables de nuestro modelo 

#Hacemos la regresion con todos los datos de la tabla Data_notxt para así poder ver que variables tienen un p-value low para ver que varibles hay que usar en nuestro modelo.
regresion1=lm(depvar~.,data=Data_notxt, na.action = 'na.exclude')
summary(regresion1)

#como vemos en el summary las variables que tienen un menor valor de p-value son las rent_6_meses y rent_en_el_anio. Y hay otras cuatro con un valor de 0.5
#vamos a rechazar todas las que no tienen asterísco y vamos  aver cuales deberíamos coger para que nuestro modelo sea correcto. 
#Hacemos otra regresión con los valore squ eme gustan: en este caso las dos variables del ***.

regresion2<- lm(depvar ~ rent_3_meses + rent_6_meses + 
                  rent_en_el_anio + Volatilidad_3, data = Data_notxt, na.action = 'na.exclude')
summary(regresion2)

AIC(regresion1,regresion2)
BIC(regresion1,regresion2)

regresion3 <- lm(depvar ~ rent_6_meses + rent_en_el_anio + Volatilidad_3, 
                 data = Data_notxt, na.action = 'na.exclude')
summary(regresion3)

AIC(regresion1,regresion3)
BIC(regresion1,regresion3)

library(car)

qqPlot(regresion1, labels=row.names(Data_notxt), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

#Backward Stepwise.usamos todas las regresiones para ver cuanto aporta cada uno todo mediceq ue me quede con rent_6_meses y rent_en_el_anio. 

stepAIC(regresion1, direction="backward")

#Forward regresion 1 y 3 
library(MASS)
library(leaps)
regfit.fwd=regsubsets(depvar~.,data=Data_notxt,method ="forward")
summary (regfit.fwd )

regfit.fwd=regsubsets(depvar~Data_notxt$rent_6_meses + Data_notxt$rent_en_el_anio + Data_notxt$Volatilidad_3,Data_notxt,method ="forward")
summary (regfit.fwd)


#Backward regresion 1 y 3 

stepAIC(regresion1, direction="backward")

regfit.bwd=regsubsets(depvar~.,data = Data_notxt,method ="backward")
summary (regfit.bwd )

regfit.bwd=regsubsets(depvar~Data_notxt$rent_6_meses + Data_notxt$rent_en_el_anio + Data_notxt$Volatilidad_3,Data_notxt,method ="backward")
summary (regfit.bwd )

#Combinación de los dos

stepAIC(regresion3, direction="both")

stepAIC(regresion2, direction="both")

