#==========================================
#       INVESTIGACION ESTADISTICA
#            CÓDIGO EN R
#     Autora: Lozada Maria del Cielo
#==========================================

# Instalación de paquetes y librerías
# install.packages("corrplot")
# install.packages("xlsx")
library(corrplot)
library('forecast')
library('tseries')
library(xlsx)
library(readxl)

# Cargar la Serie: Producción de plata.
z=read_excel("C:/Cielo/8vo ciclo/Investigación estadística/Proyecto/Serie_plata.xlsx",range = "B1:B271")

# Separación del train y test
z_train = as.data.frame(z[1:252,])
z_test = as.data.frame(z[253:270,])


# Series del train y test
TC = ts(z_train,start = c(2001,1),frequency = 12)
plot.ts(TC, col = "blue",lwd = 2, main = "Producción de plata")
TC_test = ts(z_test,start = c(2022,1),frequency = 12)
length(TC_test)
plot.ts(TC_test, col = "blue",lwd = 2, main = "Producción de plata")



# Gráfica de la serie producción de plata 2001-2023 
Z_total = ts(z,start = c(2001,1),frequency = 12)
plot.ts(Z_total, col = "orange",lwd = 2, main = "Producción de plata",xlab = "Periodo", ylab ="Plata")
abline(v = 2020, col = "red", lty = 2)
abline(v = 2021, col = "red", lty = 2)
lines(ts(z[229:241,],start = c(2020,1), end = c(2020,12), frequency = 12),col = "brown",lwd = 2.3)


# Gráficas del autocorrelograma y autocorrelograma parcial
par(mfrow=c(1,2))
acf(TC,main = "Correlograma de la serie original", col = "blue",lag = 50)
pacf(TC,main = "Correlograma parcial de la serie original",col = "magenta")
ggAcf(TC, lag = 50)
dev.off()

# Ajustamos el modelo ARIMA a la serie sin corrercción
modeloarima <- auto.arima(TC)
summary(modeloarima)
predicciones <- forecast(object = modeloarima,h=17)
plot(modeloarima$fitted, col = "blue")
lines(TC,col = "red")
accuracy(forecast(object = modeloarima,h=18), TC_test)


# Residuales
tsdisplay(residuals(modeloarima), lag.max=50, main='(2,1,2) Model Residuals')



## Creando las series por mes
D=read_excel("C:/Cielo/8vo ciclo/Investigación estadística/Proyecto/Serie_plata.xlsx",range = "B1:B229")
D = as.data.frame(D)
PLATA = ts(D,start = c(2001,1),frequency = 12)
plot.ts(PLATA, col = "blue",lwd = 2, title = "Tipo de cambio de dolar en el Perú")

class(PLATA)
M = as.vector(D[1])$Produciccion_Ag

N = matrix(0, ncol = 12, nrow = 19)
for (i in 1:12){
  N[,i]=M[seq(i, dim(D[1])[1], by = 12)]
}

Names = c("Enero", "Febrero","Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
colnames(N) <- Names

# Matriz de observaciones por mes
N
par(mfrow=c(1,2))
######
# GRÁFICAS
#####
par(mfrow=c(3,3))
ENERO = ts(N[,1], start = c(2001), frequency=1)
FEBRERO = ts(N[,2], start = c(2001), frequency=1)
MARZO = ts(N[,3], start = c(2001), frequency=1)
ABRIL = ts(N[,4], start = c(2001), frequency=1)
MAYO = ts(N[,5], start = c(2001), frequency=1)
JUNIO = ts(N[,6], start = c(2001), frequency=1)
JULIO = ts(N[,7], start = c(2001), frequency=1)
AGOSTO = ts(N[,8], start = c(2001), frequency=1)
SEPTIEMBRE = ts(N[,9], start = c(2001), frequency=1)
OCTUBRE = ts(N[,10], start = c(2001), frequency=1)
NOVIEMBRE = ts(N[,11], start = c(2001), frequency=1)
DICIEMBRE = ts(N[,12], start = c(2001), frequency=1)

# Supongamos que tienes un vector con los nombres de los meses
meses <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")

# Plot principal
plot.ts(ENERO, col = "green", lwd = 2, main = "Producción de plata por mes (2001-20019)", xlab = "Periodo", ylab = "Producción de plata", ylim = c(180000, 400100))

# Líneas para cada mes
lines(FEBRERO, col = "orange", lwd = 2)
lines(MARZO, col = "pink", lwd = 2)
lines(ABRIL, col = "purple", lwd = 2)
lines(MAYO, col = "skyblue", lwd = 2)
lines(JUNIO, col = "blue", lwd = 2)
lines(JULIO, col = "brown", lwd = 2)
lines(AGOSTO, col = "darkgreen", lwd = 2)
lines(SEPTIEMBRE, col = "yellow", lwd = 2)
lines(OCTUBRE, col = "red", lwd = 2)
lines(NOVIEMBRE, col = "gray", lwd = 2)
lines(DICIEMBRE, col = "black", lwd = 2)

# Ajuste fino de la posición vertical de la leyenda
par(xpd = FALSE)
legend("topright", legend = meses, col = c("green", "orange", "pink", "purple", "skyblue", "blue", "brown", "darkgreen", "yellow", "red", "gray", "black"), lwd = 2, title = "Meses", bty = "n", xjust = 1, yjust = 1)
par(xpd = FALSE)
plot.ts(FEBRERO, col = "white",lwd = 2, main = "FEBRERO 2001 - 2019",xlab = "Periodo", ylab ="Producción de plata")

plot.ts(MARZO, col = "pink",lwd = 2, main = "MARZO 2001 - 2019",xlab = "Periodo", ylab ="Producción de plata")

plot.ts(ABRIL, col = "purple",lwd = 2, main = "ABRIL 2001 - 2019",xlab = "Periodo", ylab ="Producción de plata")

OCTUBRE = ts(N[,10], start = c(2001), frequency=1)
plot.ts(OCTUBRE, col = "blue",lwd = 2, main = "OCTUBRE 2001 - 2019",xlab = "Periodo", ylab ="Producción de plata")
NOVIEMBRE = ts(N[,11], start = c(2001), frequency=1)
plot.ts(NOVIEMBRE, col = "skyblue",lwd = 2, main = "NOVIEMBRE 2001 - 2019",xlab = "Periodo", ylab ="Producción de plata")
DICIEMBRE = ts(N[,12], start = c(2001), frequency=1)
plot.ts(DICIEMBRE, col = "pink",lwd = 2, main = "DICIEMBRE 2001 - 2019",xlab = "Periodo", ylab ="Producción de plata")

# Matriz de correlación
matriz_cor <- cor(N)
# Visualizar la matriz de correlación
library(RColorBrewer)


mis_colores <- c("brown", "orange", "pink", "#800080", "#D3D3D3")
# Visualizar la matriz de correlación con la paleta seleccionada
corrplot(matriz_cor, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", col = mis_colores)


########################################################################
#######################  Haciendo PCA manual ###########################
########################################################################

#  N es mi matriz con las variables ('enero',..., 'diciembre')
Z1 = N%*%eigen(cor(N))$vectors
Z1[,1] # Se toma la primera columna ya que está es la que tiene la mayor varianza
CP1 = ts(as.data.frame(Z1[,1]) ,start = c(2001),frequency = 1)
plot.ts(CP1, col = "blue",lwd = 2, main = "Serie de la primera componente")

# Ajustamos el modelo ARIMA a la primera componente
modeloarima2<- auto.arima(CP1)
summary(modeloarima2)
plot(modeloarima2$fitted)
lines(CP1)
# Hacemos la predcción para esa componente
Dato_20201 <- forecast(object = modeloarima2,h=1, level=c(0.95))
PC_forecast <- Dato_20201$mean[1]

# A las demás componentes se les aplica la media como predicción para no perder información
ComponeFalta_forecast <- apply(Z1[,2:12], MARGIN = 2, FUN = mean)
# se juntan las componentes con sus predicciones
Z1_moreforecast <- rbind(Z1,c(PC_forecast,ComponeFalta_forecast))

#RECONSTRUCCION DE LA MATRIZ X
X_new = Z1_moreforecast %*% t(eigen(cor(N))$vectors)
# Valores a reemplazar en el periodo de quiebre estrucctural
X_new[20,]

#############################
# Datos añadiendo el periodo 2020 estimado
X = c()
for(i in 1:20){
  X = c(X,X_new[i,])
}
#############################

# Uniendo los datos correguidos con el periodo 2021
T1 = as.vector(z[241:270,])$Produciccion_Ag
CP2_more_PERI_2021 = c(X,T1)
CP2_more_PERI_2021 = as.data.frame(CP2_more_PERI_2021)
#Guardar la serie corregida en el archivo csv "CP2_more_PERI_2021"
setwd("C:/Cielo/8vo ciclo/Investigación estadística/Proyecto")
write.csv(CP2_more_PERI_2021, "CP2_more_PERI_2021.csv", row.names = FALSE)
############################################
  
  
CP2 = ts(as.data.frame(CP2_more_PERI_2021) ,start = c(2001,1),frequency = 12)

#################################################################
CP2_a = ts(as.data.frame(CP2_more_PERI_2021[169:270,]) ,start = c(2015,1),frequency = 12)
# Comparación gráfica (con quiebre vs sin quiebre)


plot.ts(CP2_a, col = "orange",lwd = 2, main = "Producción de plata",xlab = "Periodo", ylab ="Plata",ylim = c(100000,410000))
TC_prueb = ts(as.data.frame(z[1:240,]),start = c(2001,1),frequency = 12)

abline(v = 2020, col = "red", lty = 2)
abline(v = 2020+ 11/12, col = "red", lty = 2)
lines(ts(z[229:241,],start = c(2020,1), end = c(2020,12), frequency = 12),col = "orange",lwd = 2)
lines(ts(CP2[229:241,],start = c(2020,1), end = c(2020,12), frequency = 12),col = "brown",lwd = 3)
##################################################################

z_train = as.data.frame(z[1:252,])
z_test = as.data.frame(z[253:270,])

CP2_train = ts(CP2[1:252,],start = c(2001,1),frequency = 12)
plot.ts(CP2_train, col = "blue",lwd = 2, main = "Producción de plata en el Perú")
CP2_test = ts(CP2[253:270,],start = c(2022,1),frequency = 12)

par(mfrow=c(1,2))
acf(CP2,main = "Correlograma", col = "orange",lag = 50)
pacf(CP2,main = "Correlograma parcial",col = "purple")
ggAcf(TC, lag = 50)
dev.off()


modeloarima3<- auto.arima(CP2_train)
summary(modeloarima3)
plot(modeloarima3$fitted, type='o',col='red')
lines(CP2_train)
tsdisplay(residuals(modeloarima3), lag.max=20, main='(2,1,2) Model Residuals')
#MODELO SARIMA CORREGIDO
accuracy(forecast(object = modeloarima3,h=18), CP2_test)
residuals(modeloarima3)


Test <- as.vector(CP2_test)
predicciones <- as.vector(forecast(object = modeloarima3,h=18)[4]$mean)
length(predicciones)
df = as.data.frame(predicciones)
setwd("C:/Cielo/8vo ciclo/Investigación estadística/Proyecto")
write.csv(df, "predicciones.csv", row.names = FALSE)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
############   Residuales   ##############
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##

Residuales_test <- Test - predicciones
length(Residuales_test)
Residuales_train <- as.vector(residuals(modeloarima3))
length(Residuales_train)
Residuales = c(Residuales_train,Residuales_test)
length(Residuales)
df = as.data.frame(Residuales)
setwd("C:/Cielo/8vo ciclo/Investigación estadística/Proyecto")
write.csv(df, "Residuales.csv", row.names = FALSE)


