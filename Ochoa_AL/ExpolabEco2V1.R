#Exposicion Laboratorio 
#Econometria II

setwd("D:/02 Maestria CEE/04 sem3/04 Eco2/Lab Expo/ExpoW1-4")

# load necessary packages for importing the function
library(RCurl)

# import the function
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

install.packages("stargazer")

# Cargamos la libreria y la base de datos
library("wooldridge")
library("stargazer")

###############################################

# Ejercicio 8.1
data("sleep75")
head(sleep75)
help("sleep75")

# estimate simple linear model
reg1 <- lm( sleep~ totwrk+educ+ age + agesq+ yngkid + male, 
          data=sleep75) #señalar base
#Muestra resultados
summary(reg1)

sleep75$U1<- (residuals(reg1))^2
reg2 <- lm( U1~ male, 
            data=sleep75) #señalar base
summary(reg2)
stargazer(reg2, type="text", title = "Resultados", align = TRUE)


###################################################

#Ejercicio 8.2
data("hprice1")
head(hprice1)
help("hprice1")

# estimate simple linear model
reg3 <- lm( price~ lotsize+ sqrft +bdrms, 
            data=hprice1) #señalar base
#Muestra resultados
summary(reg3)
Nrobust_seR3 <- as.vector(summary(reg3,robust = F)$coefficients[,"Std. Error"])

summary(reg3, robust= T)
# save robust standard errors
robust_seR3 <- as.vector(summary(reg3,robust = T)$coefficients[,"Std. Error"])

# print stargazer output with robust standard errors
stargazer(reg3,type = "text",se = list(Nrobust_seR3, robust_seR3))

stargazer(reg3, reg3, se=list(NULL, robust_seR3),
          column.labels=c("default","robust"), align=TRUE)

##Inciso ii)

# estimate simple linear model
reg4 <- lm( lprice~ llotsize+ lsqrft +bdrms, 
            data=hprice1) #señalar base
#Muestra resultados
summary(reg4)
Nrobust_seR4 <- as.vector(summary(reg4,robust = F)$coefficients[,"Std. Error"])

summary(reg4, robust= T)
# save robust standard errors
robust_seR4 <- as.vector(summary(reg4,robust = T)$coefficients[,"Std. Error"])

stargazer(reg4, reg4, se=list(NULL, robust_seR4),
          column.labels=c("default","robust"), align=TRUE)

######################

#Ejercicio 8.3

#aplicar prueba White a ec 8.18 (Inciso ii)

# estimar MCO simple
reg5 <- lm( lprice~ llotsize+ lsqrft +bdrms, 
            data=hprice1) #señalar base
#Muestra resultados
summary(reg5)

#Generar residuales

hprice1$U1<- (residuals(reg5))^2

# Segunda opci�n prueba White, usar regresion MCO
#a)regresion ugorro^2=b0+b1*Ygorro+b2*Ygorro^2
resR5_sq = reg5$residuals^2
fittedR5 = reg5$fitted.values
fittedR5_sq = fittedR5^2
reg_res = lm(resR5_sq ~ fittedR5 + fittedR5_sq)
summary(reg_res)
#extraemos R^2 sin ajuste
rsq = summary(reg_res)$r.squared
#Generamos n
estad_chi<-rsq*nrow(hprice1) 
estad_chi

p_v<-1-pchisq(estad_chi,2)
p_v
alpha8_3=0.15

if(abs(p_v) >alpha8_3 ){sprintf("No se rechaza H0:Homocedasticidad con %s de nivel de significancia", alpha8_3)} else {
  sprintf("Se rechaza H0:Homocedasticidad con %s de nivel de significancia", alpha8_3)
}

stargazer(reg5, reg_res, type="text", title = "Resultados", align = TRUE)

##################################

#Ejercicio 8.4

data("vote1")
head(vote1)
help("vote1")

#Inciso a)
# explicamos %votosA = bo +b1*%votospresidA+ b2*dummdemocrata + b3*log(gastocamA) + b4*log(gastocamB)
reg7 <- lm(voteA ~ prtystrA + democA + lexpendA + lexpendB, data = vote1)
summary(reg7)

vote1$U7<-(residuals(reg7))

# Residuales explicados por variables independientes

# explicamos REsiduales MCO = bo +b1*%votospresidA+ b2*dummdemocrata + b3*log(gastocamA) + b4*log(gastocamB)
reg8 <- lm(U7 ~ prtystrA + democA + lexpendA + lexpendB, data = vote1)
summary(reg8)

rsq8_4a = summary(reg8)$r.squared
sprintf("El R2 sin ajuste tiene el valor de  %s", rsq8_4a )

stargazer(reg7, reg8,title= "Resultados", align = T)

#Inciso b)

#Calcular prueba Breusch-Pagan version F

#Tomamos los residuales de la regresion anterior
vote1$U8<-(residuals(reg8))^2

# explicamos ResidualesMCO^2 = bo +b1*%votospresidA+ b2*dummdemocrata + b3*log(gastocamA) + b4*log(gastocamB)
reg9 <- lm(U8  ~ prtystrA + democA + lexpendA + lexpendB, data = vote1)
summary(reg9)
rsq8_4b = summary(reg9)$r.squared
sprintf("El R2 sin ajuste de la nueva regresi�n tiene el valor de  %s", rsq8_4b )

#Generar tabla de modelo original y modelo auxiliar
stargazer(reg7, reg9,title= "Resultados", align = T)

#Formar F estadistico
k<-4 #Numero de variables explicativas
n<-nrow(vote1)

estad_F<- (rsq8_4b/k)/((1-rsq8_4b)/(n-k-1))
sprintf("El estadistico F para la prueba es %s", estad_F )

p_v8_4<- (1 - pf(estad_F,k,n-k-1))
sprintf("El p-value  es %s", p_v8_4 )

alpha8_4a=0.10
alpha8_4b=0.05

if(abs(p_v8_4) >alpha8_4a ){sprintf("No se rechaza H0:Homocedasticidad con %s de nivel de significancia", alpha8_4a)} else {
  sprintf("Se rechaza H0:Homocedasticidad con %s de nivel de significancia", alpha8_4a)
}

if(abs(p_v8_4) >alpha8_4b ){sprintf("No se rechaza H0:Homocedasticidad con %s de nivel de significancia", alpha8_4b)} else {
  sprintf("Se rechaza H0:Homocedasticidad con %s de nivel de significancia", alpha8_4b)
}

#Inciso iii)
#Caso especial de la prueba White

#recordemos que ya tenemos la regresion MCO
#generemos valores ajustados de voteA (voteAgorro) y residuales al cuadrado

vote1$voteAgorro<-(fitted.values(reg7))
vote1$voteAgorro2<-(fitted.values(reg7))^2

#Regresion UMCO^2= bo + b1*voteAgorro + b2*voteAgorro^2

vote1$U7sq<-(residuals(reg7))^2

reg10 <- lm( U7sq~ voteAgorro + voteAgorro2 , data = vote1)
summary(reg10)
rsq8_4W <- summary(reg10)$r.squared

#Generar tabla de modelo original y modelo auxiliar
stargazer(reg7, reg10,title= "Resultados", align = T)

#Formar F estadistico

#Leer parametros de estad�stico F
nW<-nrow(vote1)
kW<-2 #variables explicativas (Ygorro y  Ygorro2)
estad_F_W<- (rsq8_4W/kW)/((1-rsq8_4W)/(nW-kW-1))
sprintf("El estadistico F para la prueba es %s", estad_F_W)

p_v8_4W<- (1 - pf(estad_F_W,kW,nW-kW-1))
sprintf("El p-value del estad�stico F es %s ", p_v8_4W)

alpha8_4a=0.10
alpha8_4b=0.05

if(abs(p_v8_4W) >alpha8_4a ){sprintf("No se rechaza H0:Homocedasticidad con %s de nivel de significancia", alpha8_4a)} else {
  sprintf("Se rechaza H0:Homocedasticidad con %s de nivel de significancia", alpha8_4a)
}

if(abs(p_v8_4W) >alpha8_4b ){sprintf("No se rechaza H0:Homocedasticidad con %s de nivel de significancia", alpha8_4b)} else {
  sprintf("Se rechaza H0:Homocedasticidad con %s de nivel de significancia", alpha8_4b)
}
