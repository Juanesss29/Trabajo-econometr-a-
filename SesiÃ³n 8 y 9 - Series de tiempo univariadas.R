#=========================================================================================================#
#                         Unidad de Informática - Facultad de Ciencias Económicas
#                             Curso Libre Econometría Aplicada en R | 2023-1
#                               Sesión 8 y 9: Series de tiempo univariadas
#                                   Monitor: Jaime Andrés Fierro Aponte                                 
#=========================================================================================================#

#==================================================#
# Script creado por: Jaime Andrés Fierro Aponte    #
# Documentación y soporte: jaafierroap@unal.edu.co #
#==================================================#

#=================#
#### Contenido ####
#=================#

# 1. Paquetes
# 2. Box-Jenkins: 1. Identificación
#    2.1. Datos
#    2.2. Serie de tiempo
#    2.3. Gráficos
#    2.4. Significancia de las autocorrelaciones
#         2.4.1. ACF y PACF
#         2.4.2. Prueba Ljung-Box
#    2.5. ¿Es una serie estacionaria?
#    2.6. Volver estacionaria una serie: Determinar orden d  
#    2.7. Determinar los órdenes máximos (p, q) para el modelo
# 3. Box-Jenkins: 2. Estimación ARIMA
#    3.1. Estimación de un modelo ARIMA
#    3.2. Elección del modelo
#    3.3. Estimación del modelo elegido
# 4. Box-Jenkins: 3. Verificación y diagnóstico de los residuos
# 5. Box-Jenkins: 4. Pronóstico
#    5.1. Pronóstico con predict()
#    5.2. Pronóstico con forecast()
#    5.3. Real vs ajustado
# 6. Autoarima
# ---------- FIN DEL MODELO ------------------ #
# 7. Algunos códigos útiles
#    7.1. Filtro Hodrick-Prescott
#    7.2. Desestacionalización con ARIMA-SEATs
#    7.3. Desagregación temporal

#==============================#
# Limpiar el entorno
rm(list = ls())
# Limpiar la consola: ctrl+L
#==============================#




#===================#
#### 1. Paquetes ####
#===================#

pacman::p_load(
  readxl,     # Para importar archivos de Excel.
  tseries,    # Una de las opciones para realizar la prueba ADF.
  urca,       # Para las pruebas de raíz unitaria.
  lmtest,     # Para las pruebas de significancia estadística individual.
  ggfortify,  # Para graficar series de tiempo.
  forecast,   # Para hacer pronósticos.
  tsoutliers, # Para manejar valores atípicos.
  mFilter,    # Para aplicar el filtro Hodrick-Prescott.
  seasonal,   # Para desestacionalizar series de tiempo con ARIMA SEATs.
  openxlsx,   # Para exportar archivos Excel.
  tempdisagg  # Para hacer desagregación temporal.
  )


#=========================================#
#### 2. Box-Jenkins: 1. Identificación ####
#=========================================#

# Se estimará el modelo apropiado ARIMA para la serie

#### 2.1. Datos ####

setwd(r'(C:\Users\USUARIO\OneDrive\Documentos\Unal\UIFCE\2023-1\Curso Libre Econometría Aplicada en R\Sesión 8 y 9 - Series de tiempo)')
consumo = read_excel('CONS_SHARE.xlsx')
View(consumo)
class(consumo)


#### 2.2. Serie de tiempo ####

cs = ts(consumo$CONS_SH, start = c(1946, 4), frequency = 4)
# El primer argumento es la columna de la base de datos que contiene los datos a modelar.
# El segundo argumento es el parámetro 'start =', en el cual dentro de un vector se indica el período inicial.
# El tercer argumento es el parámetro 'frequency =', en el cual se indica la periodicidad (1: Mensual, 4: Trimestral, 6: semestral)

# Tipo de objeto
class(cs)

# Cuándo comienza la serie
start(cs)

# Cuándo finaliza la serie
end(cs)

# La periodicidad de los datos
frequency(cs)

# Estadística descriptiva
summary(cs)

# Ver la serie
View(cs) # Objetos ts no pueden verse como los dataframe
cs

#### 2.3. Gráficos #####

# 2.3.1. Gráfico de la serie de tiempo ----

# Usando plot base

# Graficar la serie temporal
plot(cs, main = 'CONSUMO COMO % DEL PIB')

# Incluir línea de tendencia
fit = lm(cs ~ time(cs))
abline(fit, col = 'blue')

# Usando ggplot2

# ggfortify es un paquete para trabajar con ggplot2 sobre series temporales 
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html

autoplot(cs, main = 'CONSUMO COMO % DEL PIB') +
  geom_abline(intercept = fit$coefficients[1],
              slope = fit$coefficients[2], col = 'blue')

# 3.3.2. Gráfico de caja para cada ciclo ----

# Con plot base

# Hacer diagrama de caja por trimestre

(ciclo=cycle(cs)) # El ciclo de la serie
boxplot(cs ~ ciclo, col = cm.colors(4)) # La paleta de colores con cm.colors
boxplot(cs ~ ciclo, col = rainbow(4)) # Con otra paleta de colores

# Con ggplot2

cs %>% 
  cycle() %>% 
  factor(.) %>% 
  data.frame(ciclo = ., cs) %>% 
  ggplot() + 
  aes(x = ciclo, 
      y = cs,
      col = ciclo) +   # Colorear sólo los bordes. Comentar esta línea si fill descomentada
      # fill = ciclo) + # Colorear por dentro. Comentar esta línea si col descomentada
  geom_boxplot() + 
  theme(legend.position = 'none')


# 3.3.3. Descomposición de la serie ----

# Descomponer la serie (componente tendencial, estacional y aleatorio)
descompos = decompose(cs)

# Gráfico con plot base
plot(descompos)

# Gráfico ggplot2
autoplot(descompos) +
  theme_bw()


#### 2.4. Significancia de las autocorrelaciones ####

# 2.4.1. ACF y PACF ----

# ACF
acf(cs) # Decaimiento lento a cero indica que la serie no es estacionaria 
pacf(cs)
ts.plot(cs) # Evidentemente, no es estacionaria


# 2.4.2. Prueba Ljung-Box ----

# H0: Autocorrelaciones no significativas.

Box.test(cs, lag = 8, type = c('Ljung-Box'))


#### 2.5. ¿Es una serie estacionaria? ####

# Prueba Dickey-Fuller (H0:No estacionaria)

adf.test(cs, alternative = 'stationary') # Del paquete tseries

# Prueba de raíz unitaria Dickey Fuller Aumentado del paquete urca

# La función ur.df() computa el test aumentado de Dickey-Fuller. 

# Para los términos determinísticos -> H0: No significativos.
# Para la raíz unitaria -> H0: Existe al menos una raiz unitaria, por lo que no hay estacionariedad.

# Evaluar significancia de tendencia e intercepto y raíz unitaria
dft = ur.df(cs, type = 'trend', lags = 4)
# El parámetro 'type =' indica la parte 'aumentada' de la prueba. 
# Evalúa la significancia de los términos determinísticos Puede tener tres 
# valores:
#         * 'trend' para evaluar la significancia de la tendencia y del 
#            intercepto en el modelo, y si tiene raíz unitaria.
#         * 'drift' para evaluar la significancia del intercepto en el modelo, 
#            y si tiene raíz unitaria.
#         * 'none' para evaluar si existe raíz unitaria

summary(dft)
# phi3: Evalúa la significancia de la tendencia. 
#       Si el valor calculado es mayor al valor crítico, se rechaza H0 y la 
#       tendencia es significativa.
# phi2: Evalúa la significancia del intercepto.
#       Si el valor calculado es mayor al valor crítico, se rechaza H0 y el 
#       intercepto es significativo.
# tau3: Evalúa la existencia de raíz unitaria.
#       Si el valor calculado es menor al valor crítico, se rechaza H0 y no hay 
#       raíz unitaria, hay estacionariedad.

# Evaluar significancia del intercepto y raíz unitaria
dfd = ur.df(cs, type = 'drift', lags = 4)
summary(dfd)

# Evaluar raíz unitaria sin términos determinísticos
dfn = ur.df(cs, type = 'none', lags = 4)
summary(dfn)

# Prueba Phillip-Perron

pptest = ur.pp(cs, model = c('trend'), type = c('Z-tau'))
summary(pptest)


# Prueba KPSS

kpss = ur.kpss(cs, type = c('tau'))
summary(kpss)


#### 2.6. Volver estacionaria una serie: Determinar orden (d) ####

# Para estabilizar varianzas, se utiliza el logaritmo de la serie 
lcs = log(cs)
autoplot(lcs,
         main = 'Logaritmo del Consumo')

# Para remover el componente tendencial y estacional, se toma le serie transformada en primeras diferencias 
dlcs = diff(lcs)
autoplot(dlcs,
         main = 'Logaritmo del consumo en primeras diferencias')


# Prueba Dickey-Fuller (H0:No estacionaria)

adf.test(dlcs, alternative = 'stationary') # Del paquete tseries


# Prueba de raíz unitaria Dickey Fuller Aumentado del paquete urca


# Evaluar significancia de tendencia e intercepto y raíz unitaria
dft = ur.df(dlcs, type = 'trend', lags = 4)
summary(dft)
# phi3: Evalúa la significancia de la tendencia. 
# Si el valor calculado es mayor al valor crítico, se rechaza H0 y la tendencia es significativa.
# phi2: Evalúa la significancia del intercepto.
# Si el valor calculado es mayor al valor crítico, se rechaza H0 y el intercepto es significativo.
# tau3: Evalúa la existencia de raíz unitaria.
# Si el valor calculado es menor al valor crítico, se rechaza H0 y no hay raíz unitaria, hay estacionariedad.

# Evaluar significancia del intercepto y raíz unitaria
dfd = ur.df(dlcs, type = 'drift', lags = 4)
summary(dfd)

# Evaluar raíz unitaria sin términos determinísticos
dfn = ur.df(dlcs, type = 'none', lags = 4)
summary(dfn)

# Prueba Phillip-Perron

pptest = ur.pp(dlcs, model = c('trend'), type = c('Z-tau'))
summary(pptest)

# Prueba KPSS

kpss = ur.kpss(dlcs, type = c('tau'), use.lag = 4)
summary(kpss)



#### 2.7. Determinar los órdenes máximos (p, q) del modelo ####

par(mfrow = c(2, 1))

acf(dlcs,
    main = 'Significancia rezagos Diff Log Consumo', 
    sub = 'ACF')

pacf(dlcs,
     main = 'Significancia parcial rezagos Diff Log Consumo', 
     sub = 'PACF')

par(mfrow = c(1, 1))





#===========================================#
#### 3. Box-Jenkins: 2. Estimación ARIMA ####
#===========================================#

#### 3.1. Elección del modelo por AIC ####

# ARIMA (2,1,2) de serie
arima212 = arima(x = lcs, order = c(2,1,2)); summary(arima212)

# ARIMA (2,1,4) de serie
arima214 = arima(x = lcs, order = c(2,1,4)); summary(arima214)

# ARIMA (4,1,2) de serie
arima412 = arima(x = lcs, order = c(4,1,2)); summary(arima412)

# ARIMA (4,1,4) de serie
arima414 = arima(x = lcs, order = c(4,1,4)); summary(arima414)


rbind(arima212$aic, 
      arima214$aic, 
      arima412$aic, 
      arima414$aic)


#### 3.2. Estimación iterativa del modelo ####

mar = 4
mma = 4
results = data.frame()
for (i in 0:mar) {
  for (j in 0:mma)  {
    fitp = arima(lcs, order = c(i, 1, j))
    results = rbind(results,c(i, j, AIC(fitp), BIC(fitp))) 
  }
}
colnames(results) = c('p', 'q', 'AIC', 'BIC')
results

# Determinamos cuál es el valor más bajo en AIC y BIC y en cuál fila está
results %>% 
  slice_min(AIC, n = 5)

results %>% 
  slice_min(BIC, n = 5)

# Función propia

best_arima = function(serie, p_max, q_max, d_order, information_criterium='AIC'){

'
Función creada por: Jaime Fierro
Para documentación: jaafierroap@unal.edu.co
Dependencias: Ninguna
Versión: 0.0.0
Fecha: 21/06/2021

Esta función sugiere un modelo ARIMA(p,d, q) con base en un criterio de información 
definido por el usuario (AIC o BIC). Además, devulve los valores mínimos de cada
criterio de información, con el número de órdenes para el ARIMA asociados.
También devuelve los 5 mejores modelos según el criterio indicado, para que el 
usuario pueda hacer un análisis más amplio y contextual.

input: 
      - serie: Serie tipo ts.
      - p_max: Orden p máximo a evaluar (obtenido el ACF).
      - q_max: Orden q máximo a evaluar (obtenido del PACF).
      - d_order: Orden de integración d (obtenido de las pruebas de raíz unitaria).
      - information_criterium: Criterio de información (AIC o BIC) por el cual optimizar el modelo.
                               Valor por defecto 'AIC'.

output: Lista con los siguientes elementos:
       - Best model: String indicando el mejor modelo según el criterio de información indicado.
       - Best five models: Dataframe con los mejores 5 modelos según el criterio de información indicado.
       - Minimum AIC: Lista con el menor valor del AIC y el número del modelo correspondiente.
       - Minimum BIC: Lista con el menor valor del BIC y el número del modelo correspondiente.
       - All models: Dataframe con todos los órdenes iterados y sus correspondientes criterios de información.
'
  
  # Para desactivar las alertas.
  options(warn = -1)
  
  # Estimación de varios modelos iterando en varios órdenes p y q
  results_iter = data.frame()  # Dataframe vacío en el que se guardará la información de los modelos
  
  for (p in 0:p_max){   # Inicia el ciclo para iterar sobre un orden p.
    
    for (q in 0:q_max){ # Inicia el ciclo para iterar sobre un orden q.
      
      model_fitted = arima(serie, order = c(p, d_order, q)) # Estimación de un ARIMA según los órdenes del bucle
      results_iter = rbind(results_iter, c(p, d_order, q, AIC(model_fitted), BIC(model_fitted))) # Almacenamiento de la información en un dataframe
      
    } # Fin del bucle iterando en q
    
  } # Fin del bucle iterando en p
  
  colnames(results_iter) = c('p', 'd', 'q', 'AIC', 'BIC') # Nombres de las columnas del nuevo dataframe
  
  # Declaración de datos de interés
  idx_lower_IC = which.min(results_iter[, information_criterium]) # Número del modelo con el menor criterio de información
  best_p = results_iter[idx_lower_IC, 'p'] # Orden p del mejor modelo
  best_q = results_iter[idx_lower_IC, 'q'] # Orden q del mejor modelo
  best_IC = min(results_iter[, information_criterium]) # Valor del menor criterio de información
  
  # Configuración de la salida
  best_model = paste('Best fitted model is ARIMA(',
                     best_p, ', ',
                     d_order, ', ',
                     best_q, 
                     ') according to ',
                     information_criterium, '.',
                     sep='')
  
  # Declarando los mejores 5 modelos según criterio de información
  best_five_models = head(results_iter[order(results_iter[, information_criterium]), ], 5)
  
  # Activar nuevamente las alertas
  options(warn = getOption('warn'))
  
  return(list(
    'Best model' = best_model,
    'Best five models' = best_five_models,
    'Minimum AIC' = list('AIC' = min(results_iter[, 'AIC']),
                         'Number of model' = which.min(results_iter[, 'AIC'])),
    'Minimum BIC' = list('BIC' = min(results_iter[, 'BIC']),
                         'Number of model' = which.min(results_iter[, 'BIC'])),
    'All models' = results_iter)
  )
  
}

mejores_modelos = best_arima(serie = lcs,
                             p_max = 4, 
                             q_max = 4,
                             d_order = 1,
                             information_criterium = 'BIC')
mejores_modelos
mejores_modelos$`Best model`
mejores_modelos$`Best five models`
mejores_modelos$`Minimum AIC`
mejores_modelos$`Minimum BIC`
mejores_modelos$`All models`


#### 3.3. Estimación del modelo ####

# Estimamos el modelo con menor AIC y BIC
modelo = arima(x = lcs, order = c(2, 1, 2))

coeftest(modelo) # Coeficientes y la significancia

AIC(modelo)




#======================================================================#
#### 4. Box-Jenkins: 3. Verificación y diagnóstico de los residuos #####
#======================================================================#


# Haciendo uso de tsdiag (graphics)
x11()
tsdiag(modelo)

# Haciendo uso de ggtsdiag (ggfortify) (residuos estandarizados, acf residuos y LjungBox)
windows()
ggtsdiag(modelo, gof.lag = 8)



# Creación a mano de la validación

par(mfrow = c(3, 1))

plot(modelo$resid, 
     main = 'Residuales de la serie',
     xlab = 'Residuales', 
     ylab = 'Fecha')
abline(a=0, b=0, col='red', lty = 2)

acf(modelo$resid, 
    ylim=c(-0.2, 1), 
    main = 'ACF de Residuales')

lb_df = data.frame()
for (lag_number in 1:8) {
  
  lb_test = Box.test(resid(modelo), lag = lag_number,  type = 'Ljung')
  print(lb_test)
  pval = lb_test$p.value
  
  lb_df = rbind(lb_df, pval)
  colnames(lb_df) = 'Ljung-Box - p.value'
  
}
plot(x = rownames(lb_df), 
     y = lb_df$`Ljung-Box - p.value`,
     pch = 19,
     ylim = c(0, 1),
     main = 'p-values de la prueba Ljung-Box',
     xlab = 'Rezagos',
     ylab = 'Ljung-Box p-value')
abline(a = 0.05, b = 0, lty = 2, col = 'blue')

par(mfrow = c(1, 1))


# Normalidad de los errores
# H0: Distribución normal
jarque.bera.test(modelo$resid)   # Paquete tseries
hist(modelo$residuals)           # Paquete graphics (nativo de R)
car::qqPlot(modelo$residuals)    # Paquete car
psych::skew(modelo$residuals)    # Paquete psych
psych::kurtosi(modelo$residuals) # Paquete psych


# ¿La no normalidad se debe a la estructura del modelo?

residuales = residuals(modelo)

# Esta función colapsa los polinomios del modelo ARIMA en dos polinomios: 
# el producto del polinomio autoregresivo y el producto del polinomio de media móvil.
coef = coefs2poly(modelo)

# Se usa la función locate.outliers(residuales, coefs2poly)
locate.outliers(residuales, coef)
# Cuando el tipo de valor atípico es TC o LS, es muy difícil corregir la no
# normalidad, pero podemos guiarnos del histograma y/o QQ-Plot para saber si
# estamos cerca de una distribución normal o no.
# Si el valor atípico es tipo AO, habría que corregirlo , porque estos producen
# colas pesadas.



#======================================#
#### 5. Box-Jenkins: 4. Pronóstico #####
#======================================#

#### 5.1. Pronóstico con predict() ####

# Usaremos modelo.

pronostico = predict(modelo, n.ahead = 8)
# Pronosticar 8 trimestres futuros
valores_futuros = pronostico$pred #predicciones en log
prediccion_nivel = exp(valores_futuros)

# Realizar gráfico de series de tiempo
ts.plot(cs, prediccion_nivel,
        col=c('black', 'blue'),lty = c(1, 3))

# Añadir margen de error
error1 = prediccion_nivel + qnorm(0.95, 0, 1) * exp(pronostico$se)
error2 = prediccion_nivel - qnorm(0.95, 0, 1) * exp(pronostico$se)
ts.plot(cs, 
        prediccion_nivel, 
        error1, 
        error2, 
        col=c('black', 'blue','red', 'red'), 
        lty=c(1, 2, 2, 2))


#### 5.2. Pronóstico con forecast ####

prediccion_forecast = forecast(modelo, level = 0.95, h = 8)
autoplot(prediccion_forecast)


#### 5.3. Real vs Ajustado ####

ts.plot(cs, ts(exp(prediccion_forecast$fitted), start = c(1946, 3), freq = 4),
        col = c('black', 'red'))


#====================#
#### 6. Autoarima ####
#====================#

modelo_autoarima = auto.arima(lcs)
autoplot(forecast(auto.arima(lcs), h = 8))


#=================================================================================#
#=================================================================================#
#=========================== FIN DEL MODELO ======================================#
#=================================================================================#
#=================================================================================#


#=================================#
#### 7. Algunos códigos útiles ####
#=================================#

#### 7.1. Filtro Hodrick-Prescott ####

# Con el paquete mFilter
# lambda para datos mensuales: 14400
# lambda para datos trimestrales: 1600
# lambda para datos anuales: 100

h_p_f = hpfilter(cs, type = 'lambda', freq = 1600)
plot(h_p_f)
plot(h_p_f$trend)
plot(h_p_f$cycle)
h_p_f$trend


#### 7.2. Desestacionalización con ARIMA-SEATs ####
# https://cran.r-project.org/web/packages/seasonal/seasonal.pdf
# http://www.seasonal.website/seasonal.html


# Con el paquete seasonal

# Desestacionalización sin Efectos Calendario
cs_seas = seas(cs)
View(cs_seas)
cs_seas[['data']]


# Desestacionalizacon con Efectos Calendario

cs_seas_ce = seas(cs , x11 = '', 
                  xreg = genhol(easter, start = 0, end = 0, frequency = 4 , center = 'calendar'), 
                  regression.usertype = 'holiday')
cs_seas_ce[['data']]

cs_SA = cs_seas_ce[['data']][,3]; cs_SA

autoplot(cbind(cs, cs_SA),
         main = 'CS y CS Desestacionalizado', size = 0.7) +
  scale_x_continuous('Tiempo') + 
  scale_y_continuous('Consumo') +
  scale_color_manual(values = c('#3C12E6', '#F21111')) +
  theme(legend.position = 'right')

cs_seas_df = data.frame(cbind(as.character(consumo$DATE), cs_seas_ce[['data']][3]))
colnames(cs_seas_df) = c('Fecha', 'CS_SA')
View(cs_seas_df)

write.xlsx(cs_seas_df, 'Consumo desestacionalizado.xlsx')
getwd()


#### 7.3. Desagregación temporal ####

# Con el paquete tempdisagg
# https://cran.r-project.org/web/packages/tempdisagg/index.html

ZA_WB = read_excel('Sudáfrica anual WB.xlsx', sheet = 'U_ZA')
View(ZA_WB)

za = ts(ZA_WB$U_ZA, start = c(1991), frequency = 1)
length(za)
autoplot(za, main = 'Desempleo en Sudáfrica anual')

zaq = td(za ~ 1, 
         conversion = 'mean', 
         to = 'quarterly', 
         method = 'chow-lin-maxlog')
za_q = predict(zaq)
za_q
length(za_q)
autoplot(za_q, main = 'Desempleo en Sudáfrica trimetralizado')

inicio = as.Date('1991-04-1')
fin = as.Date('2021-01-01')
fecha = seq(from = inicio, to = fin, by = '3 months')

fecha = seq(from = inicio, to = fin, by = 'quarter') - 1

length(fecha)

za_trim = cbind(Trimestre = as.character(fecha), U_ZA = za_q)
View(za_trim)
write.xlsx(za_trim,'Sudáfrica trimestralizado.xlsx')


