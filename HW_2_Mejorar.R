# LIBRERIAS ##
library(dplyr)
library(ggplot2)
library(dplyr)
library(MASS)
library(boot)
library(verification)
library(ISLR)
library(tidyverse)
library(gmodels)
library(plyr)
library(caTools)
library(e1071)
library(ROCR)
library(caret)


## Primero leemos el archivo ##
datos <- read.csv("~/Desktop/Data Science /Prediccion/Homework_2/Homework_2/Loans.csv", sep = ",", dec = ".", header = TRUE, skip = 1)

summary(datos)


# Primero voy a limpiar los datos porque hay muchos NA y muchos 0s. 
loans <- datos[, c(-1, -2, -11, -18, -19, -20, -23, -24, -29, -30, -36, -43, -44, -45, -50, -51, -54, -55, -56, -79, -80, -87, -89, -99, -101, -102, -107)]
loans <- loans[!apply(loans == "", 1, all),] ##borrar filas sin nada
#hacemos primero unos dibujos oara ver un poco el panorama 
#no hay ningun NA desmsurado asi que guay 
var.has.na <- lapply(loans, function(x){any(is.na(x))})
numero_nas <- which( var.has.na == TRUE )	
numero_nas

#quito los NAs
#loans <- loans[complete.cases(loans),]
loans <- na.omit(loans)


# Pasamos a factor todo 
loans[sapply(loans, is.character)] <- lapply(loans[sapply(loans, is.character)],as.factor)
loans[sapply(loans, is.numeric)] <- lapply(loans[sapply(loans, is.numeric)],as.factor)
loans[sapply(loans, is.integer)] <- lapply(loans[sapply(loans, is.integer)],as.factor)
loans[sapply(loans, is.double)] <- lapply(loans[sapply(loans, is.double)],as.factor)


## GRÁFICAS ##
loans_plot <- ggplot(data=loans, aes(loans$loan_amnt)) + 
  geom_histogram(breaks=seq(0, 35000, by=1000), 
                 col="blue", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="aliceblue", high="royalblue3")+
  labs(title="Loan Amount", x="Amount", y="Number of Loans")
loans_plot
#podemos observas que la mayoría de los loans tienen una cantidad inferior a 20,000 y que el mayor número de loans se encuentra alrededor de 10,000 de cantidad. 

box_status <- ggplot(loans, aes(loan_status, loan_amnt)) + 
  geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) +
  labs(list(title = "Loan amount by status", x = "Loan Status", y = "Loan Amount"))
box_status

# Queremos predecir el loan_status para ver saber si un cliente va a default o no. Según el articulo se ve que hay una clara relación entre credit grade (grade), debt to income ratio (dti), y la revolving line use (revol_util).
ggplot(loans,aes(loan_status, loan_amnt))+ geom_boxplot()
# habia relacion a parte de con estas 4 con: hom_ownership, tot_cur_bal.

#### MODELO ####

## TRAIN Y TEST ##
n=nrow(loans)
id_train <- sample(1:n , 0.90*n)
id_train
loans.train = loans[id_train,]
loans.test = loans[-id_train,]
nrow(loans.train)
ncol(loans.train)
nrow(loans.test)

# regresión logística
loans_glm <-  glm(loan_status~., family="binomial",data=loans.train)
summary(loans_glm)

loans_glm1 <-  glm(loan_status~grade+revol_util+tot_cur_bal+home_ownership+loan_amnt, family="binomial",data=loans.train)
summary(loans_glm1)

# Predecimos
loans_prediccion <- predict(loans_glm1, loans.test, type = "response")
summary(loans_prediccion)

## AUC
pred <- prediction(loans_prediccion,loans.test$loan_status)
auc1 <- performance(pred, "auc")
print(auc1@y.values[[1]])  # me da un 0.5474472. Tenemos mala roc curve 

# Curva ROC
roc.plot(loans.test$loan_status == "1", loans_prediccion)

#mejor auc area under de curve. Mejor el de 1 peor el de 0.5

#cut off. meter cut off a mano. 

# Confusion Matrix

confusionMatrix(round(loans_prediccion), loans.test$loan_status)















