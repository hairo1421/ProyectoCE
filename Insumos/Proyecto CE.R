####################################
# Librerias
####################################

library(magrittr)
library(ggplot2)
library(tidyverse)
library(vars)
library(forecast)
library(urca)
library(pander)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(GGally)
library(orcutt)
library(tseries)
library(ggfortify)
library(imputeTS)
library(seasonal)

###############################################################
###############################################################
##
##      Parte 1. Procesamiento y Estadísticos
##
###############################################################
###############################################################

####################################
# Cargar datos
####################################
getwd()
setwd("C:/Users/h_air/Documents/Semestre 3/Cómputo Estadístico/Cómputo Estadístico Proyecto/Datos")



# Cargar precios del petroleo
precios <- read.csv("precios.csv")
# el formato es factor, se debe pasar a numerico
precios$Mar.
# almacena nombre de variables
nombres <- precios %>% colnames()
# clase distinta a factor
x <- precios %>% unclass()
# dimensión
precios %>% dim
# se realiza loop para obtener formato de los datos
Precios <- matrix(0L, dim(precios)[1], dim(precios)[2])
# variable años
Precios[,1] <- x$AÃ.O
# loop
for( i in 2:13){
  Precios[ , i] <- gsub(",", "", x[[i]] %>% as.character() ) %>% as.numeric() %>% as.matrix()
  
}
# dataframe
Precios %<>% as.data.frame() 
# se edita nombre de variables
colnames(Precios) <- nombres


# Cargar reservas de petroleo internacionañ
reservas <- read.csv("reservas.csv") %>% as.data.frame()



# Formato de sere de tiempo
St_precios <- gather(Precios,   "mes", "precios", 2:13)


St_precios <- St_precios[order(St_precios$AÃ.O),]  


St_reserva <- gather(reservas, "mes", "reservas", 2:13)

St_reserva <- St_reserva[order(St_reserva$AÃ.O),] 


St_precios <- ts(St_precios[,3], start = c(2007,1), frequency = 12)

St_reserva <- ts(St_reserva[,3], start = c(1996,1), frequency = 12)


# remover valores atipicos
St_reserva[St_reserva > 50000] <- 9911

# Imputando datos Kalman Filter suavizador
#St_reserva <- na_kalman(St_reserva, model = "auto.arima")

St_precios <- na_kalman(St_precios, model = "auto.arima")



###############################################################
###############################################################
##
##      Parte 2. Ejercicio 1
##
###############################################################
###############################################################

# 1. Grafique las series de tiempo y explique brevemente el comportamiento observado de cada una.

# Nota, ver hechos en el contexto económico

####################################
# Visualización 
####################################

# Series
#####################
x11()
cbind("Precios Petroleo Venezuela" = St_precios,
      "Reserva Petrole Internacionañ"=St_reserva) %>%
  autoplot(facets=TRUE) + theme_bw(base_family = "serif")

ggsave("imagen1.png", width  = 20, height = 10)
# Precio Petroleo Venezuela
##############################
St_precios %>%
  autoplot() + ggtitle("Precio Petróleo Venezuela") + theme_bw(base_family = "serif") + xlab("")

ggsave("imagen2.png", width  = 20, height = 10)
# 2008-2009: cambio estructural, crisis global
# 2010-2014: periodo de bonanza, recuperación; flustuaciones tendencia a la alza
# 2015 - 2016: desaceleración
# 2017 : recuperación

St_reserva %>%
  autoplot() + ggtitle("Reserva Petróleo Internacional") + theme_bw(base_family = "serif") + xlab("")

ggsave("imagen3.png", width  = 20, height = 10)


St_reserva %>%
  changepoint:: cpt.meanvar() %>%  # Identify change points
  autoplot() + ggtitle("Precio Petróleo Venezuela") + theme_bw(base_family = "serif") + xlab("")



ggsave("imagen4.png", width  = 20, height = 10)

St_precios %>%
  changepoint:: cpt.meanvar() %>%  # Identify change points
  autoplot() + ggtitle("Precio Petróleo Venezuela") + theme_bw(base_family = "serif") + xlab("")


ggsave("imagen5.png", width  = 20, height = 10)
# en general incremento con drift del 1996-2010
# incrementos de 1996-1998
# decremento de 1998 al 2000
# incremento del 2001 al 2003
# baja y sube hasta el 2010
# cambio de tedencia a partir del 2010
# tendencia a la baja con fluctuación


###############################################################
###############################################################
##
##      Parte 3. Ejercicio 2
##
###############################################################
###############################################################


# 2. Si observa estacionalidad y/o tendencia en las series, aplique las técnicas vistas en el
# para eliminación de tendencia y estacionalidad a cada una de las series (en el caso de ser necesario).
# Luego grafique la tendencia, la estacionalidad y la serie con la tendencia y/o estacionalidad eliminadas y explique los resultados.


# Estacionalidad # Test que indica estacionalidad

######################################

library("seastests")
set.seed(16)

# Is seasonal:
#  (the WO-test is used to assess the seasonality of a time series)

# Wo: 
#  Webel-Ollech overall seasonality test that combines results from different seasonality tests.
# Combines the results of the QS-test and the kwman-test, both calculated on the residuals of an automatic non-seasonal ARIMA model. 
# If the p-value of the QS-test is below 0.01 or the p-value of the kwman-test is below 0.002, the WO-test will classify the corresponding time series as seasonal.


# Precios
################################################################
isSeasonal(St_precios) # False
summary(wo(St_precios)) #
#Test used:  WO 

#Test statistic:  0 
#P-value:  1 1 0.9609284 

#The WO - test does not identify  seasonality

# Reservas
################################################################
isSeasonal(St_reserva) # TRUE
summary(wo(St_reserva)) 
#Test used:  WO 

#Test statistic:  1 
#P-value:  1.965873e-06 1.876367e-06 0.01746343 

#The WO - test identifies seasonality
################################################################

# Referencia: https://cran.r-project.org/web/packages/seastests/vignettes/seastests-vignette.html



# Estacionalizando Reserva de Petroleo
#########################################

# seas: 
# Seasonal Adjustment with X-13ARIMA-SEATS,  to perform a seasonal adjustment that works well in most circumstances.




St_reserva_s <- seas(St_reserva)
St_reserva_s$data %>% colnames()

# Se observa que ya no es estacional
################################################################
St_reserva_s$data[ ,3] %>% isSeasonal()
summary(wo(St_reserva_s$data[ ,3])) 
#Test used:  WO 

#Test statistic:  0 
#P-value:  1 1 0.7661409 

#The WO - test does not identify  seasonality
################################################################



# Componentes de la serie
seasonal <- St_reserva_s$data[ ,2]
seasonaladj_reserva <- St_reserva_s$data[ ,3]
trend <- St_reserva_s$data[ ,4]
irregular <- St_reserva_s$data[ ,5]


# Plot original y seasonal
#########################################################################
x11()

temp1 <- cbind(St_reserva, seasonaladj_reserva, trend)
colnames(temp1) <- c("Original", "Ajustada", " Tendencia")
 
temp1 %>% autoplot() +
  ggtitle("X-13ARIMA-SEATS decomposition Reservas") + theme_bw(base_family = "serif") +
   scale_color_manual(values=c("black", "blue", "green2")) +
ggsave("imagen6.png", width  = 20, height = 10)



x11()

cbind("Original" = St_reserva,
      "Ajustada"=seasonaladj_reserva,
      "Seasonal" = seasonal,
      "Trend" = trend,
      "Irregular" = irregular) %>%
  autoplot(facets=TRUE) + theme_bw(base_family = "serif")  +
  ggtitle("X13 decomposition Reservas")
ggsave("imagen7.png", width  = 20, height = 10)



# Estacionariedad
######################################


# Es lo mismo
(St_precios*1000000) %>% log()  %>% diff %>%  ts(start = c(2007, 2), frequency = 12) %>% autoplot() 
  


x11()
(St_precios) %>% log()  %>% diff %>%  ts(start = c(2007, 2), frequency = 12) %>% autoplot() + 
   ggtitle("Diferencias del logaritmo de los Precio Petróleo Venezuela") + theme_bw(base_family = "serif") + xlab("")
ggsave("imagen8.png", width  = 20, height = 10)


(seasonaladj_reserva) %>% log()  %>% diff %>%  ts(start = c(1996, 1), frequency = 12) %>% autoplot() +
   ggtitle("Diferencia del logaritmo de las Reserva Petróleo") + theme_bw(base_family = "serif") + xlab("")
ggsave("imagen9.png", width  = 20, height = 10)

# Pruebas de raíz Unitaria
#########################################################################



raizUnitaria <- function(x, n, m, prueba){
  set.seed(1)
  
  
  
  if(prueba == "DF"){
    
    matriz_none <- matrix(0L, n, m)
    matriz_drift <- matrix(0L, n, m)
    matriz_trend <- matrix(0L, n, m)
    
    a1 <-  ur.df( x , lags =10, type='none', selectlags =  "BIC")
    a2 <- ur.df( x , lags =10, type='drift', selectlags =  "BIC")
    a3 <- ur.df( x , lags =10, type='trend', selectlags =  "BIC")
    i <- 1
      # Drift
    matriz_none[i,1:(m-1)] <- a1@cval
    matriz_none[i,m] <- a1@teststat
      
      # Trend
    matriz_drift[i,1:(m-1)] <- a2@cval[1,]
    matriz_drift[i,m] <- a2@teststat[1]
      
      # Trend
    matriz_trend[i,1:(m-1)] <- a3@cval[1,]
    matriz_trend[i,m] <- a3@teststat[1]
      
      
    
    
    rechazo1 <- (matriz_none[,m] < matriz_none[,1:(m-1)]) %>% sum()
    rechazo_none <- cbind((matriz_none[,m] < matriz_none[,1:(m-1)]) %>% as.data.frame(), rechazo1)
    
    rechazo2 <- (matriz_drift[,m] < matriz_drift[,1:(m-1)]) %>% sum()
    rechazo_drift <- cbind((matriz_drift[,m] < matriz_drift[,1:(m-1)]) %>% as.data.frame(), rechazo2)
    
    rechazo3 <- (matriz_trend[,m] < matriz_trend[,1:(m-1)]) %>% sum()
    rechazo_trend <- cbind((matriz_trend[,m] < matriz_trend[,1:(m-1)]) %>% as.data.frame(), rechazo3)
    
    nombres <- x %>% colnames()
    
    rownames(matriz_none) <- nombres
    rownames(matriz_drift) <- nombres
    rownames(matriz_trend) <- nombres
    rownames(rechazo_none) <- nombres
    rownames(rechazo_drift) <- nombres
    rownames(rechazo_trend) <- nombres
    
    return(list(none = matriz_none, drift = matriz_drift, trend = matriz_trend , none_t = rechazo_none, drift_t = rechazo_drift,
                trend_t = rechazo_trend ))
    
    
  }else if (prueba == "PP"){
    
    matriz_drift <- matrix(0L, n, m)
    matriz_trend <- matrix(0L, n, m)
    
    
      
      a1 <-  ur.pp(  x, type='Z-tau' , model='constant' , lags='long' )
      a2 <- ur.pp(  x, type='Z-tau' , model='trend' , lags='long' )
      
      
      
      # drift
      matriz_drift[,1:(m-1)] <- a1@cval
      matriz_drift[,m] <- a2@teststat
      
      # Trend
      matriz_trend[,1:(m-1)] <- a2@cval[1,]
      matriz_trend[,m] <- a2@teststat[1]
      
      
    
    
    
    
    rechazo2 <- (matriz_drift[,m] < matriz_drift[,1:(m-1)]) %>% sum()
    rechazo_drift <- cbind((matriz_drift[,m] < matriz_drift[,1:(m-1)]) %>% as.data.frame(), rechazo2)
    
    rechazo3 <- (matriz_trend[,m] < matriz_trend[,1:(m-1)]) %>% sum()
    rechazo_trend <- cbind((matriz_trend[,m] < matriz_trend[,1:(m-1)]) %>% as.data.frame(), rechazo3)
    
    nombres <- x %>% colnames()
    
    rownames(matriz_drift) <- nombres
    rownames(matriz_trend) <- nombres
    rownames(rechazo_drift) <- nombres
    rownames(rechazo_trend) <- nombres
    
    return(list( drift = matriz_drift, trend = matriz_trend ,  drift_t = rechazo_drift,
                 trend_t = rechazo_trend ))
    
  }else if(prueba == "KPSS"){
    
    
    matriz_mu <- matrix(0L, n, m+1)
    matriz_tau <- matrix(0L, n, m+1)
    
    
      a1 <-  ur.kpss(x, type ="mu" , lags = "long", use.lag = NULL)
      a2 <- ur.kpss(x, type =  "tau", lags =  "long",  use.lag = NULL)
      
      
      # mu
      
      matriz_mu[,1:(m)] <- a1@cval
      matriz_mu[,m+1] <- a1@teststat
      
      # tau
      matriz_tau[,1:(m)] <- a2@cval[1,]
      matriz_tau[,m+1] <- a2@teststat[1]
      
      
    
    
    rechazo1 <- (matriz_mu[,m+1] > matriz_mu[,1:(m)]) %>% sum()
    rechazo_mu <- cbind((matriz_mu[,m+1] > matriz_mu[,1:(m)]) %>% as.data.frame(), rechazo1)
    
    rechazo2 <- (matriz_tau[,m+1] > matriz_tau[,1:(m)]) %>% sum()
    rechazo_tau <- cbind((matriz_tau[,m+1] > matriz_tau[,1:(m)]) %>% as.data.frame(), rechazo2)
    
    nombres <- x %>% colnames()
    
    rownames(matriz_mu) <- nombres
    rownames(matriz_tau) <- nombres
    rownames(rechazo_mu) <- nombres
    rownames(rechazo_tau) <- nombres
    
    
    
    return(list( mu = matriz_mu, tau = matriz_tau , mu_t = rechazo_mu, tau_t = rechazo_tau))
    
  } else if(prueba == "ZA"){
    
    
    matriz_both <- matrix(0L, n, m)
    matriz_intercept <- matrix(0L, n, m)
    matriz_trend <- matrix(0L, n, m)
    
    
      a1 <- ur.za(x, model = "both", lag=3)
      a2 <- ur.za(x, model = "intercept", lag=3)
      a3 <- ur.za(x,  model = "trend", lag=3)
      
      # Drift
      matriz_both[,1:(m-1)] <- a1@cval
      matriz_both[,m] <- a1@teststat
      
      # Trend
      matriz_intercept[,1:(m-1)] <- a2@cval
      matriz_intercept[,m] <- a2@teststat
      
      # Trend
      matriz_trend[,1:(m-1)] <- a3@cval
      matriz_trend[,m] <- a3@teststat
      
      
    
    
    rechazo1 <- (matriz_both[,m] < matriz_both[,1:(m-1)]) %>% sum()
    rechazo_both <- cbind((matriz_both[,m] < matriz_both[,1:(m-1)]) %>% as.data.frame(), rechazo1)
    
    rechazo2 <- (matriz_intercept[,m] < matriz_intercept[,1:(m-1)]) %>% sum()
    rechazo_intercept <- cbind((matriz_intercept[,m] < matriz_intercept[,1:(m-1)]) %>% as.data.frame(), rechazo2)
    
    rechazo3 <- (matriz_trend[,m] < matriz_trend[,1:(m-1)]) %>% sum()
    rechazo_trend <- cbind((matriz_trend[,m] < matriz_trend[,1:(m-1)]) %>% as.data.frame(), rechazo3)
    
    nombres <- x %>% colnames()
    
    rownames(matriz_both) <- nombres
    rownames(matriz_intercept) <- nombres
    rownames(matriz_trend) <- nombres
    rownames(rechazo_both) <- nombres
    rownames(rechazo_intercept) <- nombres
    rownames(rechazo_trend) <- nombres
    
    return(list(both = matriz_both, intercept = matriz_intercept, trend = matriz_trend , both_t = rechazo_both, intercept_t = rechazo_intercept,
                trend_t = rechazo_trend ))
  }
  
}





# Niveles
#####################################

m <- 4
n <- 1

DF_p <- raizUnitaria(St_precios, n, m, "DF")
PP_p <- raizUnitaria(St_precios, n, m, "PP")
KPSS_p <- raizUnitaria(St_precios, n, m, "KPSS")
ZA_p <- raizUnitaria(St_precios, n, m, "ZA")







DF_r <- raizUnitaria(seasonaladj_reserva, n, m, "DF")
PP_r <- raizUnitaria(seasonaladj_reserva, n, m, "PP")
KPSS_r <- raizUnitaria(seasonaladj_reserva, n, m, "KPSS")
ZA_r <- raizUnitaria(seasonaladj_reserva, n, m, "ZA")




# Diferencia
########################


DF_dp <- raizUnitaria(difSt_precios, n, m, "DF")
PP_dp <- raizUnitaria(difSt_precios, n, m, "PP")
KPSS_dp <- raizUnitaria(difSt_precios, n, m, "KPSS")
ZA_dp <- raizUnitaria(difSt_precios, n, m, "ZA")

DF_dr <- raizUnitaria(dif_reserva, n, m, "DF")
PP_dr <- raizUnitaria(dif_reserva, n, m, "PP")
KPSS_dr <- raizUnitaria(dif_reserva, n, m, "KPSS")
ZA_dr <- raizUnitaria(dif_reserva, n, m, "ZA")



###############################################################
###############################################################
##
##      Parte 3. Ejercicio 3
##
###############################################################
###############################################################

# 3. Calcule y grafica  la ACF y PACF para cada una de las series y explique los resultados.


difSt_precios <- (St_precios) %>% log()  %>% diff %>%  ts(start = c(2007, 2), frequency = 12)
dif_reserva <- seasonaladj_reserva %>% log()  %>% diff %>%  ts(start = c(1996, 2), frequency = 12)




# Reservas Internacionales Primeras diferencias
#############################################################
p1 <- dif_reserva  %>%  ggAcf()   + theme_bw(base_family = "serif") + xlab("")  + ggtitle((" "))
p2 <- dif_reserva  %>%  ggPacf()   + theme_bw(base_family = "serif") + xlab("")  + ggtitle((" "))
p3 <- dif_reserva  %>%  autoplot()   + theme_bw(base_family = "serif") + xlab("")  +
  ggtitle(("Diferencia del logaritmo de las Reserva Petróleo"))

g1 <- arrangeGrob(p3,                             # First row with one plot spaning over 2 columns
                  arrangeGrob(p1, p2, ncol = 2), # Second row with 2 plots in 2 different columns
                  nrow = 2) #generates g

ggsave(file="imagen10.png", width  = 20, height = 10, g1) #saves g



# Reservas internacionales segundad diferencias
############################################################
p4 <- dif_reserva %>% diff %>%  ggAcf()   + theme_bw(base_family = "serif") + xlab("")  + ggtitle((" "))
p5 <- dif_reserva %>% diff %>%  ggPacf()   + theme_bw(base_family = "serif") + xlab("")  + ggtitle((" "))
p6 <- dif_reserva  %>% diff %>%  autoplot()   + theme_bw(base_family = "serif") + xlab("")  +
  ggtitle(("Segunda diferencia del logaritmo de las Reserva Petróleo"))




g2 <- arrangeGrob(p6,                             # First row with one plot spaning over 2 columns
                  arrangeGrob(p4, p5, ncol = 2), # Second row with 2 plots in 2 different columns
                  nrow = 2) #generates g

ggsave(file="imagen11.png", width  = 20, height = 10, g2) #saves g

# Precios 
##########################################################


p7 <- difSt_precios  %>%  ggAcf()   + theme_bw(base_family = "serif") + xlab("")  + ggtitle((" "))
p8 <- difSt_precios %>%  ggPacf()   + theme_bw(base_family = "serif") + xlab("")  + ggtitle((" "))
p9 <- difSt_precios   %>%  autoplot()   + theme_bw(base_family = "serif") + xlab("")  +
  ggtitle(("Diferencias del logaritmo de los Precio Petróleo Venezuela"))



g3 <- arrangeGrob(p9,                             # First row with one plot spaning over 2 columns
                  arrangeGrob(p7, p8, ncol = 2), # Second row with 2 plots in 2 different columns
                  nrow = 2) #generates g

ggsave(file="imagen12.png", width  = 20, height = 10, g3) #saves g


###############################################################
###############################################################
##
##      Parte 4. Ejercicio 4 NOta shyni
##
###############################################################
###############################################################

# 4. Ajuste un modelo AR(p), MA(q) o ARMA(p,q) a cada una de las series segun sea el
# caso. No es necesario ajustar los 3 modelos, solo aquel que corresponda y que se deduce
# de las ACF y PACF del punto anterior.

#  Reservas
##########################################
modelo_reserva <- auto.arima(dif_reserva) # 0,1,5
modelo_reserva %>% summary
modelo_reserva <- arima(dif_reserva, c(0,1,5)) 


modelo_reserva2 <- auto.arima(dif_reserva %>% diff) # 0,0,5
modelo_reserva2 <- arima(dif_reserva %>% diff, c(0,0,5)) 
# Precios
##########################################
modelo_precios <- auto.arima(difSt_precios) # 1,0,0
modelo_precios <- arima(difSt_precios, c(1,0,0))

# MIOS
##########################################
modelo_reserva_1 <- arima(dif_reserva, c(4,0,2)) 
modelo_reserva_2 <- arima(dif_reserva %>% diff, c(0,0,4)) 




##############################################
# ARCH EFECT
##############################################


#  Next, observe the squared residual plot. If there are clusters of volatility, 
# ARCH/GARCH should be used to model the volatility of the series to reflect more recent 
# changes and fluctuations in the series

# Finally, ACF & PACF of squared residuals will help confirm if the residuals (noise term) 
# are not independent and can be predicted. SI hay autocorrelación está

# Visual ver si hay volatilidad

# Reserva Modelo 1
####################################

res2_reserva <- modelo_reserva$residuals^2

a1 <- res2_reserva %>% autoplot() + theme_bw(base_family = "serif") + xlab("")  +
  ggtitle(("Reserva Internacional ARIMA(0,1,5)"))
a2 <- ggAcf(res2_reserva) + theme_bw() + xlab("")  +
  ggtitle((""))
a3 <- ggPacf(res2_reserva) + theme_bw() + xlab("") +
  ggtitle((""))


g4 <- arrangeGrob(a1,                             
                  arrangeGrob(a2, a3, ncol = 2), 
                  nrow = 2)

ggsave(file="imagen13.png", width  = 20, height = 10, g4) 


# Reserva Modelo 2
####################################
# cbind(modelo_reserva$residuals^2, modelo_reserva2$residuals^2) %>%  autoplot()

res2_reserva2 <- modelo_reserva2$residuals^2
a4 <- res2_reserva2 %>% autoplot() + theme_bw(base_family = "serif") + xlab("")  +
  ggtitle(("Reserva Internacional  MA(5)"))
a5 <- ggAcf(res2_reserva2) + theme_bw() + xlab("")  +
  ggtitle((" "))
a6 <- ggPacf(res2_reserva2) + theme_bw() + xlab("") +
  ggtitle((" "))


g5 <- arrangeGrob(a4,                             
                  arrangeGrob(a5, a6, ncol = 2), 
                  nrow = 2)

ggsave(file="imagen14.png", width  = 20, height = 10, g5) 


# Precios  Modelo 1
####################################

res2_precios3 <- modelo_precios$residuals^2
a7 <- res2_precios3 %>% autoplot() + theme_bw(base_family = "serif") + xlab("")  +
  ggtitle(("Precio Petróleo Venezuela AR(1,0,0)"))
a8 <- ggAcf(res2_precios3) + theme_bw() + xlab("")  +
  ggtitle((""))

a9 <- ggPacf(res2_precios3) + theme_bw() + xlab("") +
  ggtitle((""))



g6 <- arrangeGrob(a7,                             
                  arrangeGrob(a8, a9, ncol = 2), 
                  nrow = 2)

ggsave(file="imagen15.png", width  = 20, height = 10, g6) 


# Portmanteu Q test
# ARCH heteroscedasticity test for residuals  Ho alternative: heteroscedastic
# Se quiere no rechazar
# Ho =  no heterocedasticidad
# Ha = Heterocedasticidad

# Lagrange multiple test deben ser significantes para rechazar


library(aTSA)

# Hacer los Plots bonitos
# Reserva modelo 1
###########################################
modelo_reserva <- arima(dif_reserva, c(0,1,5)) # 0,1,5


arch_reserva1 <- arch.test(modelo_reserva,output=TRUE)

# No hay efecto arch


#Portmanteau-Q test:
#  order    PQ p.value
##[1,]     4 0.443   0.979
#[2,]     8 0.631   1.000
#[3,]    12 0.848   1.000
#[4,]    16 1.069   1.000
#[5,]    20 1.231   1.000
#[6,]    24 1.801   1.000
#Lagrange-Multiplier test: 
#  order   LM p.value
#[1,]     4 1841       0
#[2,]     8  882       0
#[3,]    12  556       0
#[4,]    16  385       0
#[5,]    20  296       0
#[6,]    24  229       0





# Reserva modelo 2
###########################################
modelo_reserva2 <- arima(dif_reserva %>% diff, c(0,0,5)) # 0,0,5

arch_reserva2 <- arch.test(modelo_reserva2,output=TRUE)


# No hay efecto arch


#Portmanteau-Q test: 
# order    PQ p.value
#[1,]     4 0.640   0.958
#[2,]     8 0.862   0.999
#[3,]    12 1.035   1.000
#[4,]    16 1.270   1.000
#[5,]    20 1.439   1.000
#[6,]    24 1.896   1.000
#Lagrange-Multiplier test: 
# order   LM p.value
#[1,]     4 1667       0
#[2,]     8  793       0
#[3,]    12  502       0
#[4,]    16  350       0
#[5,]    20  270       0
#[6,]    24  212       0

# Precios modelo 1
##########################################
modelo_precios <- arima(difSt_precios, c(1,0,0)) # 1,0,0
arch_precios1 <- arch.test(modelo_precios,output=TRUE)

# hay efecto arch

#Portmanteau-Q test: 
#  order   PQ  p.value
#[1,]     4 43.1 9.80e-09
#[2,]     8 62.0 1.90e-10
#[3,]    12 62.5 7.76e-09
#[4,]    16 64.0 1.10e-07
#[5,]    20 69.9 1.92e-07
#[6,]    24 70.3 1.94e-06
#Lagrange-Multiplier test: 
#  order    LM  p.value
#[1,]     4 35.13 1.14e-07
#[2,]     8  9.52 2.17e-01
#[3,]    12  5.34 9.14e-01
#[4,]    16  3.57 9.99e-01
#[5,]    20  2.26 1.00e+00
#[6,]    24  1.86 1.00e+00



# https://rpubs.com/ludare2001/299667
# https://talksonmarkets.files.wordpress.com/2012/09/time-series-analysis-with-arima-e28093-arch013.pdf


##############################################
# Autocorrelación EFECT
##############################################

# Reserva modelo 1 
################################################
Lj_reserva <- checkresiduals(modelo_reserva)
#Ljung-Box test

#data:  Residuals from ARIMA(0,1,5)
#Q* = 19.789, df = 19, p-value = 0.4074

#Model df: 5.   Total lags used: 24

#Lj_reserva2 <- checkresiduals(modelo_reserva2)

b1 <- gghistogram(modelo_reserva$residuals, add.normal = TRUE) + theme_bw(base_family = "serif") + xlab("")  +
  ggtitle(("Residuales Reserva Internacional ARIMA(0,1,5)"))

Lj_reserva2 <- checkresiduals(modelo_reserva2)
#Ljung-Box test

#data:  Residuals from ARIMA(0,0,5) with non-zero mean
#Q* = 18.494, df = 18, p-value = 0.4236

#Model df: 6.   Total lags used: 24


b2 <- gghistogram(modelo_reserva2$residuals, add.normal = TRUE) + theme_bw(base_family = "serif") + xlab("")  +
  ggtitle(("Residuales Reserva Internacional ARIMA(0,0,5)"))




Lj_precio <- checkresiduals(modelo_precios)

b3 <- gghistogram(modelo_precios$residuals, add.normal = TRUE) + theme_bw(base_family = "serif") + xlab("")  +
  ggtitle(("Residuales Precios Petróleo ARIMA(0,1,5)"))


f1 <- arrangeGrob(b1, b3, ncol = 2, 
                  nrow = 1)

ggsave(file="imagen16.png", width  = 20, height = 10, f1) 


f2 <- arrangeGrob(b2, b3, ncol = 2, 
                  nrow = 1)

ggsave(file="imagen17.png", width  = 20, height = 10, f2) 


##############################################
# Normalidad EFECT
##############################################

# Ho: Normalidad
# Ha: No normal

shapiro.test(modelo_reserva$residuals)
shapiro.test(modelo_reserva2$residuals)
shapiro.test(modelo_precios$residuals)



###############################################################
###############################################################
##
##      Parte 5. Ejercicio 5
##
###############################################################
###############################################################


# Reservas
############################
spc_reserva <- spectrum(dif_reserva, log="no")


spec_res <- data.frame(freq = spc_reserva$freq, spec = spc_reserva$spec)
names(spec_res) <- c("Frecuency", "Spectrum")



ggplot(data = subset(spec_res)) + geom_path(aes(x = Frecuency, y =  Spectrum  ))  + theme_bw(base_family = "serif")   +
  ggtitle(("Periodogram Reserva Petróleo"))

ggsave(file="imagen18.png", width  = 20, height = 10) 



# espectro a periodo
###################################
delta <- 1/12
specx <- spc_reserva$freq/delta
specy <- 2*spc_reserva$spec
spec_res_t <- data.frame(freq = specx, spec = specy)
names(spec_res_t) <- c("Periodo", "Spectrum")

ggplot(data = subset(spec_res_t)) + geom_path(aes(x = Periodo, y =  Spectrum  ))  + theme_bw(base_family = "serif")   +
  ggtitle(("Periodogram Reserva Petróleo"))

ggsave(file="imagen20.png", width  = 20, height = 10) 



# Precios
###########################333
spc_precios <- spectrum(difSt_precios, log="no")



spec_pre <- data.frame(freq = spc_precios$freq, spec = spc_precios$spec)
names(spec_pre) <- c("Frecuency", "Spectrum")



ggplot(data = subset(spec_pre)) + geom_path(aes(x = Frecuency, y =  Spectrum  ))  + theme_bw(base_family = "serif")  +
  ggtitle(("Periodogram Precios Petróleo"))


ggsave(file="imagen19.png", width  = 20, height = 10) 




# espectro a precios
######################################################
delta <- 1/12
specx <- spc_precios$freq/delta
specy <- 2*spc_precios$spec
spec_pre_t <- data.frame(freq = specx, spec = specy)
names(spec_pre_t) <- c("Periodo", "Spectrum")

ggplot(data = subset(spec_pre_t)) + geom_path(aes(x = Periodo, y =  Spectrum  ))  + theme_bw(base_family = "serif")   +
  ggtitle(("Periodogram Reserva Petróleo"))

ggsave(file="imagen21.png", width  = 20, height = 10) 



################################################################################
################################################################################
################################################################################

# Sample periodogram: 
spc_reserva <- spectrum(dif_reserva, log="no")
spc_precios <- spectrum(difSt_precios, log="no")

 

# Smoothed sample periodogram
spectrum(dif_reserva, kernel("daniell"), log="no")
spectrum(difSt_precios, kernel("daniell"), log="no")


# Smoothed sample periodogram
spectrum(dif_reserva, kernel("modified.daniell"), log="no")
spectrum(difSt_precios, kernel("modified.daniell"), log="no")


# We find maximum and corresponding period:

max(spc_reserva$spec)
max(spc_precios$spec)


spc_reserva$freq[which.max(spc_reserva$spec)]
spc_precios$freq[which.max(spc_precios$spec)]

1/spc_reserva$freq[which.max(spc_reserva$spec)]
1/spc_precios$freq[which.max(spc_precios$spec)]


# Zoom
plot(spc_reserva$freq[1:50], spc_reserva$spec[1:50], type="l") 
plot(spc_precios$freq[1:50], spc_precios$spec[1:50], type="l") 



#  estimation using AR models

# We model data as an AR process
# Spectrum is then estimated as the spectrum of that AR process
# Sunspots data 

sp2_reserva<-spectrum(dif_reserva, method="ar", log="no")
sp2_precios<-spectrum(difSt_precios, method="ar", log="no")





# Como en la otra referencia

# espectro a densidad
sp_reserva <- spectrum(dif_reserva, log="no", spans=c(2,2), plot=TRUE)
sp_precios <- spectrum(difSt_precios, log="no", spans=c(2,2), plot=TRUE)


################################################################################
################################################################################
################################################################################


###########################
# Coherence
###########################
library("biwavelet")

# Reserva and Precios
##############################################
dif_reserva2 <- window(dif_reserva , star =    c(2007,2), frecuency = 12)
DATE <- 1:length(dif_reserva2)
DATE <- 1:length(difSt_precios)
t1 = cbind(DATE, dif_reserva2) %>% as.matrix()
t2 = cbind(DATE, difSt_precios) %>% as.matrix()


nrands = 1000 # más
wtc.AB = wtc(t1, t2, nrands = nrands)

# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE, main =  "Reservas Internacionales vs Precios Venezuela")
#Wavelet Coherence:

# Adding grid lines
n = length(t1[, 1])
abline(v = seq(10, n, 10), h = 1:16, col = "brown", lty = 1, lwd = 1)

# Defining x labels
axis(side = 3, at = c(seq(0, n, 12)), labels = c(seq(2007, 2017, 1)))


ggsave(file="imagen22.png", width  = 20, height = 10) 

#Interpretation: Pendiente ggplot
  
#Time is displayed on the horizontal axis, while the vertical axis shows the frequency (the lower the frequency, the higher the scale). Regions in time-frequency space where the two time series co-vary are located by the wavelet coherence.

#Warmer colors (red) represent regions with significant interrelation, while colder colors (blue) signify lower dependence between the series. Cold regions beyond the significant areas represent time and frequencies with no dependence in the series.

#An arrow in the wavelet coherence plots represents the lead/lag phase relations between the examined series. A zero phase difference means that the two time series move together on a particular scale. Arrows point to the right (left) when the time series are in phase (anti-phase).

#When the two series are in phase, it indicates that they move in the same direction, and anti-phase means that they move in the opposite direction. Arrows pointing to the right-down or left-up indicate that the first variable is leading, while arrows pointing to the right-up or left-down show that the second variable is leading.







###############################################################
###############################################################
##
##      Parte 6. Ejercicio 6
##
###############################################################
###############################################################

######## Cross correlation

c1 <- ggCcf(dif_reserva2,difSt_precios ,  lag.max = 10, type = "correlation",
      plot = TRUE)  + theme_bw(base_family = "serif")  +
  ggtitle(("Correlación Cruzada Reserva respecto Precios")) 

ggsave(file="imagen23.png", width  = 20, height = 10) 

c2 <- ggCcf(difSt_precios , dif_reserva2, lag.max = 10, type = "correlation",
      plot = TRUE)  + theme_bw(base_family = "serif")  +
  ggtitle(("Correlación Cruzada Precios respecto Reserva")) 

ggsave(file="imagen24.png", width  = 20, height = 10) 




colnames(serie) <- c("Reservas", "Precios")
c3 <- serie %>% autoplot + theme_bw(base_family = "serif")  +
  ggtitle(("Reserva de Petróleo Internacional vs Precios del Petróle de Venezuela"))  +
  scale_color_manual(values=c("black", "red3")) 



d1<- arrangeGrob(c3,                             # First row with one plot spaning over 2 columns
                  arrangeGrob(c1, c2, ncol = 2), # Second row with 2 plots in 2 different columns
                  nrow = 2) #generates g

ggsave(file="imagen25.png", width  = 20, height = 10, d1) #saves g


###############################################################
###############################################################
##
##       Ejercicio 6 Mili
##
##############################################################

# La serie diferenciada desde 2007 de precios y reserva

library(forecast)
x_reserva <- window(dif_reserva, start=c(2007, 2), frequency=12)  
y_precios <- difSt_precios

# Se unieron las series desde el 2007
serie <- cbind(x_reserva, y_precios)
serie2 <- cbind(y_precios, x_reserva)
x11()
ts.plot(serie, xlab="Tiempo",col=c(1,2))           #serie 1

ts.plot(serie2, xlab="Tiempo", col=c(1,2))         #serie 2

library(vars)

VARselect(serie,lag.max=10,type="none")
modelo<- VAR(serie, p=3, type=c("none"))
modelo

summary(modelo, equation="x_reserva")          
summary(modelo, equation="y_precios")
plot(modelo)                                       #serie1

#####

VARselect(serie2,lag.max=10,type="none")           #serie2
modelo2<- VAR(serie2, p=3, type=c("none"))
modelo2

summary(modelo2, equation="x_reserva")          
summary(modelo2, equation="y_precios")
plot(modelo2)



# Diagnostico


roots(modelo)  # es estable


# autocorrelación

var.serial <- serial.test(modelo)   # correlación serial residuales son independientes
var.serial


x11()
#plot(var.serial, names="x_reserva")
#plot(var.serial, names="y_precios")

# heterocedasticidad

var.arch <- vars::arch.test(modelo, lags.multi = 5 ,multivariate.only = TRUE) 
var.arch # rechazo, si hay efecto arch



var.norm <- normality.test(modelo)  #H_o: Normalidad
var.norm # no normalidad





## Causalidad
# H0: No grenger causality
grangertest(y_precios ~ x_reserva, order=3, data=serie) # No rechazo
grangertest(x_reserva ~ y_precios , order=3, data=serie) # rechazo
# Precios causan reservas en sentido de granger



# Impulso respuesta
modelo.irf1<-irf(modelo,impulse="y_precios", response="x_reserva")
modelo.irf2<-irf(modelo,impulse="x_reserva", response="y_precios")
modelo.irf3<-irf(modelo,impulse="y_precios", response="y_precios")
modelo.irf4<-irf(modelo,impulse="x_reserva", response="x_reserva")
x11()
plot(modelo.irf1) #
plot(modelo.irf2)
plot(modelo.irf3) #
plot(modelo.irf4)



modelo.irf <- irf(modelo)
# Precos -> Reervas y Precios -> Precios
number_ticks <- function(n) {function(limits) pretty(limits, n)}
lags <- c(1:11)
irf1<-data.frame(modelo.irf$irf$y_precios[,1],modelo.irf$Lower$y_precios[,1],
                 modelo.irf$Upper$y_precios[,1], lags)
irf2<-data.frame(modelo.irf$irf$y_precios[,2],modelo.irf$Lower$y_precios[,2],
                 modelo.irf$Upper$y_precios[,2])

P_Reserva <- ggplot(data = irf1,aes(lags,modelo.irf.irf.y_precios...1.)) +
  geom_line(aes(y =  modelo.irf.Upper.y_precios...1.), colour = 'lightblue2') +
  geom_line(aes(y = modelo.irf.Lower.y_precios...1.), colour = 'lightblue')+
  geom_line(aes(y = modelo.irf.irf.y_precios...1.))+
  geom_ribbon(aes(x=lags, ymax=modelo.irf.Upper.y_precios...1., ymin=modelo.irf.Lower.y_precios...1.), fill="lightblue", alpha=.1) +
  xlab("") + ylab("Reserva") + ggtitle("Orthogonal Impulse Response from Reserva and Impulse from Precios") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black') + theme_bw(base_family = "serif")  + geom_hline(yintercept=0, color ="red3")


P_Precios <- ggplot(data = irf2,aes(lags,modelo.irf.irf.y_precios...2.)) +
  geom_line(aes(y =  modelo.irf.Upper.y_precios...2.), colour = 'lightblue2') +
  geom_line(aes(y = modelo.irf.Lower.y_precios...2.), colour = 'lightblue')+
  geom_line(aes(y = modelo.irf.irf.y_precios...2.))+
  geom_ribbon(aes(x=lags, ymax=modelo.irf.Upper.y_precios...2., ymin=modelo.irf.Lower.y_precios...2.), fill="lightblue", alpha=.1) +
  xlab("") + ylab("Precios") + ggtitle("Orthogonal Impulse Response from Precios and impulse form Precios") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black') + theme_bw(base_family = "serif")  + geom_hline(yintercept=0, color ="red3")



# Reservas -> Precios y Recervas -> Recervas
irf3<-data.frame(modelo.irf$irf$x_reserva[,1],modelo.irf$Lower$x_reserva[,1],
                 modelo.irf$Upper$x_reserva[,1], lags)
irf4<-data.frame(modelo.irf$irf$x_reserva[,2],modelo.irf$Lower$x_reserva[,2],
                 modelo.irf$Upper$x_reserva[,2])

R_reserva <- ggplot(data = irf3,aes(lags,irf3$modelo.irf.irf.x_reserva...1.)) +
  geom_line(aes(y =  irf3$modelo.irf.Upper.x_reserva...1.), colour = 'lightblue2') +
  geom_line(aes(y = irf3$modelo.irf.Lower.x_reserva...1.), colour = 'lightblue')+
  geom_line(aes(y = irf3$modelo.irf.irf.x_reserva...1.))+
  geom_ribbon(aes(x=lags, ymax=irf3$modelo.irf.Upper.x_reserva...1., ymin=irf3$modelo.irf.Lower.x_reserva...1.), fill="lightblue", alpha=.1) +
  xlab("") + ylab("Reserva") + ggtitle("Orthogonal Impulse Response from Reserva impulse form Reserva") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black') + theme_bw(base_family = "serif")  + geom_hline(yintercept=0, color ="red3")



R_Precios <- ggplot(data = irf4,aes(lags,irf4$modelo.irf.irf.x_reserva...2.)) +
  geom_line(aes(y =  irf4$modelo.irf.Upper.x_reserva...2.), colour = 'lightblue2') +
  geom_line(aes(y = irf4$modelo.irf.Lower.x_reserva...2.), colour = 'lightblue')+
  geom_line(aes(y = irf4$modelo.irf.irf.x_reserva...2.)) +
  geom_ribbon(aes(x=lags, ymax=irf4$modelo.irf.Upper.x_reserva...2., ymin=irf4$modelo.irf.Lower.x_reserva...2.), fill="lightblue", alpha=.1) +
  xlab("") + ylab("Reserva") + ggtitle("Orthogonal Impulse Response from Reserva impulse form Reserva") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black') + theme_bw(base_family = "serif")  + geom_hline(yintercept=0, color ="red3")


z1<- arrangeGrob(P_Reserva, P_Precios, R_reserva, R_Precios, ncol = 2, # Second row with 2 plots in 2 different columns
                 nrow = 2) #generates g

ggsave(file="imagen26.png", width  = 20, height = 10, z1) #saves g


plot(fevd(modelo)) 

ggsave(file="imagen27.png", width  = 20, height = 10) #saves g

# Referencia:
# https://stackoverflow.com/questions/45625506/grid-arrange-ggplot2-on-impulse-response-function-irf

# Precios afectan a reservas como precios pasan primero luego se mueven las reservas
# ver que tanto aporta venezuela