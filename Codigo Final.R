
#Cargamos librerías que vamos a utilizar

library(dplyr)
library(tidyr)
install.packages("VIM") 
library(VIM)
library(ggplot2)
library(plotly)
library(tidyverse)
install.packages("gridExtra")
library(gridExtra)
install.packages("DescTools")
library(DescTools)
install.packages("vcd")
library(vcd)
library(writexl)
install.packages("FactorMineR")
library(FactoMineR)
install.packages("factoextra")
library(factoextra)
library(gplots)
library(corrplot)
library(cluster)



#___________________________________________________________________________________________________________________________________
#1 Preparacion de los datos 

#Cargamos la base de datos 
#https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset/data

ruta <-  "C:/Users/Paula/OneDrive/Matematicas y estadistia-UCM/Matematicas y estadistica/4º/2º Cuatri/ADAT/Trabajo/Sleep Health and lifestyle Dataset/Sleep_health_and_lifestyle_dataset.csv"
sleep=read.table(ruta,header=T, sep=",")
head(sleep)


#1.1 número de variables, número de individuos

nrow(sleep)
ncol(sleep)

#1.2 Codificamos las variables para que este en el formato correcto

summary(sleep)

sleep$Gender <- as.factor(sleep$Gender)
sleep$Occupation <- as.factor(sleep$Occupation)
sleep$Quality.of.Sleep <- as.factor(sleep$Quality.of.Sleep)
sleep$Stress.Level <- as.factor(sleep$Stress.Level)
sleep$BMI.Category <- as.factor(sleep$BMI.Category)
# Dividir la columna Blood.Pressure en dos columnas separadas
sleep <- sleep %>%
  separate(Blood.Pressure, into = c("Systolic", "Diastolic"), sep = "/", convert = TRUE)

sleep$Sleep.Disorder <-  as.factor(sleep$Sleep.Disorder)
summary(sleep)


#1.3 Número de NAs

sum(sapply(sleep, is.nan))
sum(sapply(sleep, is.na))
sum(sapply(sleep, is.infinite))


#No hay NAs entonces se los añadimos 
# Establecer una semilla para reproducibilidad
set.seed(123)

# Porcentaje de datos que queremos que sean missing en cada columna
porcentaje_missing <- 0.03

# Función para introducir datos faltantes de forma aleatoria en una columna
introducir_missing <- function(columna) {
  # Obtener una muestra aleatoria de índices para introducir missing values
  indices_missing <- sample(1:length(columna), size = round(length(columna) * porcentaje_missing))
  # Introducir missing values en los índices seleccionados
  columna[indices_missing] <- NA
  return(columna)
}

# Aplicar la función a cada columna del conjunto de datos
sleep_missing <- as.data.frame(lapply(sleep, introducir_missing))

# Verificar que se hayan añadido datos faltantes
summary(sleep_missing)

#Contamos los NAs

sum(sapply(sleep_missing, is.nan))
sum(sapply(sleep_missing, is.na))
sum(sapply(sleep_missing, is.infinite))
total_missing_percentage <- (sum(sapply(sleep_missing, is.na)))/(nrow(sleep_missing)*ncol(sleep_missing))*100
total_missing_percentage


#1.4 Imputamos los missing por KNN

install.packages("VIM") #computar solo si no lo hemos instalado previamente
library(VIM)
sleep_imp <- sleep_missing
sleep_imp <- kNN(sleep_imp, k=7)
summary(sleep_imp)



#1.5 Hacer una comparación de las variables originales vs las variables imputadas.

#Gender
par(mfrow = c(1, 2))
plot(sleep$Gender, col = c("pink", "blue"), main = "Original", xlab = "Gender", ylab = "Frequency")
plot(sleep_imp$Gender, col = c("pink", "blue"), main = "Imputed", xlab = "Gender", ylab = "Frequency")


# Comparación con medidas estadísticas
summary(sleep$Gender)
summary(sleep_imp$Gender)


#Occupation

# Comparación visual con gráficos de dispersión
par(mfrow = c(1, 2))
par(mfrow = c(1, 2))
plot(sleep$Occupation, col = rainbow(length(unique(sleep_imp$Occupation))), main = "Original", xlab = "Occupation", ylab = "Frequency")
plot(sleep_imp$Occupation, col = rainbow(length(unique(sleep_imp$Occupation))), main = "Imputed", xlab = "Occupation", ylab = "Frequency")


# Comparación con medidas estadísticas
summary(sleep$Occupation)
summary(sleep_imp$Occupation)

#Quality of sleep

# Comparación visual con gráficos de dispersión
par(mfrow = c(1, 2))
plot(sleep$Quality.of.Sleep, col = "skyblue", main = "Original", xlab = "Quality of sleep", ylab = "Frequency")
plot(sleep_imp$Quality.of.Sleep, col = "skyblue", main = "Imputed", xlab = "Quality of sleep", ylab = "Frequency")

# Comparación con medidas estadísticas
summary(sleep$Quality.of.Sleep)
summary(sleep_imp$Quality.of.Sleep)


#Stress Level
par(mfrow = c(1, 2))
plot(sleep$Stress.Level,col=rev(heat.colors(length(table(sleep$Stress.Level)))), main = "Original", xlab = "Stress Level", ylab = "Frequency")
plot(sleep_imp$Stress.Level, col=rev(heat.colors(length(table(sleep_imp$Stress.Level)))),main = "Imputed", xlab = "Stress Level", ylab = "Frequency")

# Comparación con medidas estadísticas
summary(sleep$Stress.Level)
summary(sleep_imp$Stress.Level)

#Age
par(mfrow = c(1, 2))
plot(sleep$Age, col = "lightpink" ,main = "Original", xlab = "Frequency", ylab = "Age")
plot(sleep_imp$Age, col = "lightpink", main = "Imputed", xlab = "Frequency", ylab = "Age")


# Comparación con histogramas
par(mfrow = c(1, 2))
hist(sleep$Age,col = "lightpink" , main = "Original", xlab = "Value", ylab = "Frequency")
hist(sleep_imp$Age,col = "lightpink" , main = "Imputed", xlab = "Value", ylab = "Frequency")

# Comparación con medidas estadísticas
summary(sleep$Age)
summary(sleep_imp$Age)


#Daily Steps

par(mfrow = c(1, 2))
plot(sleep$Daily.Steps,  col = "lightgreen", main = "Original", xlab = "Frequency", ylab = "Daily Steps")
plot(sleep_imp$Daily.Steps,  col = "lightgreen", main 
     = "Imputed", xlab = "Frequency", ylab = "Daily Steps")



# Comparación con histogramas
par(mfrow = c(1, 2))
hist(sleep$Daily.Steps,  col = "lightgreen", main = "Original", xlab = "Daily Steps", ylab = "Frequency")
hist(sleep_imp$Daily.Steps,  col = "lightgreen", main = "Imputed", xlab = "Daily Steps", ylab = "Frequency")


# Comparación con medidas estadísticas
summary(sleep$Daily.Steps)
summary(sleep_imp$Daily.Steps)


#Sleep Duration

par(mfrow = c(1, 2))
plot(sleep$Sleep.Duration,col='orange', main = "Original", xlab = "Frequency", ylab = "Sleep Duration ")
plot(sleep_imp$Sleep.Duration, col='orange',main = "Imputed", xlab = "Frequency", ylab = "Sleep Duration ")


# Comparación con histogramas
par(mfrow = c(1, 2))
hist(sleep$Sleep.Duration, col='orange',main = "Original", xlab = "Sleep Duration", ylab = "Frequency")
hist(sleep_imp$Sleep.Duration, col='orange',main = "Imputed", xlab = "Sleep Duration", ylab = "Frequency")


# Comparación con medidas estadísticas
summary(sleep$Sleep.Duration)
summary(sleep_imp$Sleep.Duration)


#Physical Activity Level

par(mfrow = c(1, 2))
plot(sleep$Physical.Activity.Level, col = "aquamarine", main = "Original", xlab = "Frequency", ylab = "Physical Activity Level")
plot(sleep_imp$Physical.Activity.Level, col = "aquamarine", main = "Imputed", xlab = "Frequency", ylab = "Physical Activity Level")

# Comparación con histogramas
par(mfrow = c(1, 2))
hist(sleep$Physical.Activity.Level, col = "aquamarine", main = "Original", xlab = "Physical Activity Level", ylab = "Frequency")
hist(sleep_imp$Physical.Activity.Level, col = "aquamarine", main = "Imputed", xlab = "Physical Activity Level", ylab = "Frequency")

# Comparación con medidas estadísticas
summary(sleep$Physical.Activity.Level)
summary(sleep_imp$Physical.Activity.Level)



#_________________________________________________________________________________________________________________________
# 2. Análisis exploratorio de datos ,representaciones descriptivas

# 2.1 Variables Cualitativas Nominales:

# 2.1.1 Gender

#Tabla de frecuencias
frecuencia_genero <- table(sleep_imp$Gender)
frecuencia_genero

# Calcular porcentaje de frecuencia para cada categoría de género
porcentaje_frecuencia <- prop.table(frecuencia_genero) * 100

# Crear el gráfico de pastel
pie(frecuencia_genero, 
    main = "Distribución de género", 
    labels = paste(names(frecuencia_genero), "\n", round(porcentaje_frecuencia, 1), "%"), 
    col = c("pink", "blue"), # Colores para cada categoría
    cex = 1.2, # Tamaño del texto
    clockwise = TRUE, # Orden de las secciones del pastel (en el sentido de las agujas del reloj)
    radius = 1) # Tamaño del pastel (1 = tamaño por defecto)


#2.1.2 Occupation 

#Tabla de frecuencia
table(sleep_imp$Occupation)

#Gráfico de barras
barplot(table(sleep_imp$Occupation), 
        main = "Distribución de ocupación", 
        xlab = "Ocupación", 
        ylab = "Frecuencia",
        col = rainbow(length(unique(sleep_imp$Occupation))), # Colores arcoiris
        las = 2, # Rotación de etiquetas en el eje x
        cex.names = 0.8) # Tamaño de las etiquetas en el eje x

#Pie chart 
# Calcular la tabla de frecuencias de ocupación
frecuencia_ocupacion <- table(sleep_imp$Occupation)

# Calcular porcentaje de frecuencia para cada categoría de ocupación
porcentaje_frecuencia_ocupacion <- prop.table(frecuencia_ocupacion) * 100

# Ajustar el tamaño del dispositivo gráfico
par(mar = c(0.2,0.2,0.2,0.2))  # Ajustar los márgenes
# Crear el gráfico de pastel
pie(frecuencia_ocupacion, 
    main = "Distribución de ocupación", 
    labels = paste(names(frecuencia_ocupacion), "\n", round(porcentaje_frecuencia_ocupacion, 1), "%"), 
    col = rainbow(length(unique(sleep_imp$Occupation))), # Colores arcoiris
    cex = 0.45) # Tamaño del texto


par(mar = c(4,4,4,4))


#2.2 Variables Cualitativas ordinales: 

#2.2.1 Quality of Sleep:

#Tabla de frecuencia
summary(sleep_imp$Quality.of.Sleep)

#Representación gráfica

# Crear el gráfico de barras con mejor estética
barplot(table(sleep_imp$Quality.of.Sleep), 
        main = "Distribución de calidad del sueño", 
        xlab = "Calidad del sueño", 
        ylab = "Frecuencia",
        col = "skyblue", # Color de las barras
        border = "black") # Color del borde de las barras



#2.2.2 Stress Level:

# Calcular la tabla de frecuencias del nivel de estrés
frecuencia_stress <- table(sleep_imp$Stress.Level)
frecuencia_stress

# Crear el gráfico de barras con mejoras estéticas
barplot(frecuencia_stress, 
        main = "Distribución de nivel de estrés", 
        xlab = "Nivel de estrés", 
        ylab = "Frecuencia",
        col = rev(heat.colors(length(frecuencia_stress))), # Colores tipo heat para las barras
        border = "black", # Color del borde de las barras
        ylim = c(0, max(frecuencia_stress) * 1.1), # Ajuste de límites del eje y
        space = 0.5, # Espacio entre las barras
        las = 1, # Orientación de las etiquetas en el eje x
        cex.names = 0.8) # Tamaño de las etiquetas en el eje x



#2.3 Variables Cuantitativas Discretas:

#2.3.1 Age (Edad):

#Medidas descriptivas:
summary(sleep_imp$Age)

#Histograma:

hist(sleep_imp$Age, 
     main = "Histograma de Edad", 
     xlab = "Edad", 
     ylab = "Frecuencia",
     col = "lightpink",  # Color de las barras
     border = "white", # Color del borde de las barras
     las = 1)  # Orientación de las etiquetas en el eje x

#2.3.2 Daily Steps

#Medidas descriptivas:
summary(sleep_imp$Daily.Steps)

# Crear el gráfico de caja
boxplot(sleep_imp$Daily.Steps, 
        main = "Boxplot de Pasos Diarios", 
        ylab = "Pasos diarios",
        col = "lightgreen",  # Color de las cajas
        border = "darkgreen")  # Color del borde de las cajas

#2.4 Variables Cuantitativas Continuas:

#2.4.1 Sleep Duration (hours)

#Medidas descriptivas:
summary(sleep_imp$Sleep.Duration)

#Representación gráfica:

# Instalar y cargar la librería ggplot2 si no está instalada
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Crear el gráfico de violín
ggplot(data = sleep_imp, aes(x = "", y = Sleep.Duration)) +
  geom_violin(fill = "orange", color = "darkorange", alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", color = "darkorange", alpha = 0.7) +
  labs(title = "Distribución de Duración del Sueño", y = "Duración del sueño (horas)", x = "") +
  theme_minimal()



#2.4.2 Physical Activity Level (minutes/day)

#Medidas descriptivas:
summary(sleep_imp$Physical.Activity.Level)

#Representación gráfica:

# Instalar y cargar la librería ggplot2 si no está instalada
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Crear el gráfico de densidad
ggplot(data = sleep_imp, aes(x = Physical.Activity.Level)) +
  geom_density(fill = "coral", color = "coral3", alpha = 0.7) +
  labs(title = "Densidad de Nivel de Actividad Física", x = "Nivel de Actividad Física (minutos/día)", y = "Densidad") +
  theme_minimal()

# Crear el gráfico de caja
boxplot(sleep_imp$Physical.Activity.Level, 
        main = "Distribución de Nivel de Actividad Física", 
        ylab = "Nivel de Actividad Física (minutos/día)",
        col = "aquamarine",  # Color de las cajas
        border = "aquamarine4")  # Color del borde de las cajas


#gráfico interactivo Ocupation y stress


library(plotly)
library(dplyr)

sleep_imp %>%
  
  filter() %>%
  plot_ly(x = ~Occupation, y = ~Stress.Level, color = ~Gender, size = ~Age, type = "scatter", mode = "markers") %>%
  add_markers(alpha = 0.5) %>%
  layout(title = "",
         xaxis = list(title = "Ocupación"),
         yaxis = list(title = "Niveles de estrés"),
         legend = list(title = "Gender"))

#gráfico interactivo Ocupation y Sleep Duration
sleep_imp %>%
  
  filter() %>%
  plot_ly(x = ~Occupation, y = ~Sleep.Duration, color = ~Gender,colors = c("orange", "purple"), size = ~Age, type = "scatter", mode = "markers") %>%
  add_markers(alpha = 0.5) %>%
  layout(title = "",
         xaxis = list(title = "Ocupación"),
         yaxis = list(title = "Duración del sueño"),
         legend = list(title = "Gender"))



#gráfico interactivo Stress y Sleep Duration
sleep_imp %>%
  
  filter() %>%
  plot_ly(x = ~Stress.Level, y = ~Sleep.Duration, color = ~Gender,colors = c("lightgreen", "lightblue"), size = ~Age, type = "scatter", mode = "markers") %>%
  add_markers(alpha = 0.5) %>%
  layout(title = "",
         xaxis = list(title = "Nivel de estrés"),
         yaxis = list(title = "Duración del sueño"),
         legend = list(title = "Gender"))


#gráfico 

install.packages("gridExtra")
library(gridExtra)
library(ggplot2)

p1 <- ggplot(sleep_imp, aes(x = Age, fill = Gender)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  theme_classic() +
  ggtitle("Edad")



p2 <- ggplot(sleep_imp, aes(x = Occupation, fill = Gender)) +
  geom_bar(position = "dodge") +
  theme_classic() +
  ggtitle("Ocupación")

p3 <- ggplot(sleep_imp, aes(x = Sleep.Disorder, fill = Gender)) +
  geom_bar(position = "dodge") +
  theme_classic() +
  ggtitle("Trastorno del sueño")

# Combinamos los gráficos
grid.arrange(p1, p2, p3, ncol = 2)




#_________________________________________________________________________________________________________________________
# 3. Tablas de contingencia e independencia 

#3.1 Tablas de contingencia
#Tabla de contingencia entre Género y Ocupación
con1 <- xtabs(~Gender+Occupation, data=sleep_imp)
con1
addmargins(con1)

chisq.test(con1, correct=F)
chisq.test(con1)
fisher.test(con1,simulate.p.value=TRUE)
library(MASS)
loglm( ~ 1 + 2, data = con1) 

# Visualización con un gráfico de barras apiladas
barplot(con1, 
        main = "Distribución de Género por Ocupación", 
        xlab = "Ocupación", 
        ylab = "Frecuencia",
        col = c("lightgreen", "lightyellow"),  # Colores para cada género
        legend = rownames(con1),
        beside = TRUE)  # Barras apiladas una al lado de la otra



# Tabla de contingencia entre Género y Calidad del Sueño
con2 <- xtabs(~Gender+ Quality.of.Sleep, data=sleep_imp)
con2
addmargins(con2)
chisq.test(con2, correct=F)
fisher.test(con2,simulate.p.value=TRUE)
library(MASS)
loglm( ~ 1 + 2, data = con2)


# Gráfico de barras apiladas
barplot(con2, 
        main = "Calidad del Sueño por Género", 
        xlab = "Género", 
        ylab = "Frecuencia",
        col = c("skyblue", "salmon"),
        legend = rownames(con2))



#Tabla de Contingencia entre Género y Nivel de Estrés
con3<- xtabs(~Gender+ Stress.Level, data=sleep_imp)
con3
addmargins(con3)

chisq.test(con3, correct=F)
chisq.test(con3)
fisher.test(con3,simulate.p.value=TRUE)
library(MASS)
loglm( ~ 1 + 2, data = con3)
#gráfico 
barplot(con3, 
        main = "Distribución de Género por Nivel de Estrés", 
        xlab = "Nivel de Estrés", 
        ylab = "Frecuencia",
        col = c("deeppink2", "darkblue"),  # Colores para cada género
        legend = rownames(con3),
        beside = TRUE)  # Barras apiladas una al lado de la otra



#Tabla de Contingencia entre Género y Edad
con4 <- xtabs(~Gender + Age, data=sleep_imp)
con4
addmargins(con4)

chisq.test(con4, correct=F)
chisq.test(con4)
fisher.test(con4,simulate.p.value=TRUE)
library(MASS)
loglm( ~ 1 + 2, data = con4)


# Gráfico de densidad por género
ggplot(sleep_imp, aes(x = Age, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de Edad por Género",
       x = "Edad",
       y = "Densidad") +
  theme_minimal()



#Tabla de Contingencia entre Género y Número de pasos
con5 <- xtabs(~Gender + Daily.Steps, data=sleep_imp)
con5
addmargins(con5)

chisq.test(con5, correct=F)
chisq.test(con5)
fisher.test(con5,simulate.p.value=TRUE)
library(MASS)
loglm( ~ 1 + 2, data = con5)


# Gráfico de densidad por género con color diferente
ggplot(sleep_imp, aes(x = Daily.Steps, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de Pasos Diarios por Género",
       x = "Pasos Diarios",
       y = "Densidad") +
  scale_fill_manual(values = c("red", "blue")) +  # Cambio de color
  theme_minimal()


#Tabla de Contingencia entre Género y Duracion del sueño
con6 <- xtabs(~Gender + Sleep.Duration , data=sleep_imp)
con6
addmargins(con6)

chisq.test(con6, correct=F)
chisq.test(con6)
fisher.test(con6,simulate.p.value=TRUE)
library(MASS)
loglm( ~ 1 + 2, data = con6)


# Gráfico de densidad suavizada por género con banda de confianza
ggplot(sleep_imp, aes(x = Sleep.Duration, fill = Gender)) +
  geom_density(alpha = 0.5, color = "black") +
  labs(title = "Distribución de Duración del Sueño por Género",
       x = "Duración del Sueño",
       y = "Densidad") +
  geom_vline(aes(xintercept = mean(Sleep.Duration)), color = "red", linetype = "dashed", size = 1) +  # Línea vertical para la media
  geom_text(aes(x = mean(Sleep.Duration), label = round(mean(Sleep.Duration), 2)), y = 0.02, vjust = -1, color = "red", size = 4) +  # Etiqueta para la media
  geom_rug(alpha = 0.2) +  # Marcas en el eje x para mostrar la distribución de los datos
  theme_minimal()


#Tabla de Contingencia entre Género y Nivel de actividad física
con7 <- xtabs(~Gender + Physical.Activity.Level, data=sleep_imp)
con7
addmargins(con7)

chisq.test(con7, correct=F)
chisq.test(con7)
fisher.test(con7,simulate.p.value=TRUE)
library(MASS)
loglm( ~ 1 + 2, data = con7)


# Definir colores personalizados
colores <- c("orange", "purple")  # Por ejemplo, rosa y azul claro

# Gráfico de barras para la distribución de nivel de actividad física por género con colores personalizados
ggplot(sleep_imp, aes(x = factor(Physical.Activity.Level), fill = Gender)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(title = "Distribución de Nivel de Actividad Física por Género",
       x = "Nivel de Actividad Física",
       y = "Frecuencia") +
  scale_fill_manual(values = colores) +  # Aplicar colores personalizados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x para mejor legibilidad


# Tabla de contingencia entre Ocupación y Nivel de Estrés
con8 <- xtabs(~Stress.Level + Occupation, data=sleep_imp)
con8
addmargins(con8)

chisq.test(con8, correct=F)
fisher.test(con8,simulate.p.value=TRUE)
par(mar = c(3,3,3,3)) 
library(MASS)
loglm( ~ 1 + 2, data = con8)


# Gráfico de barras 
barplot(con8, 
        main = "Nivel de Estrés por Ocupación", 
        xlab = "Ocupación", 
        ylab = "Frecuencia",
        col = rev(heat.colors(length(unique(sleep_imp$Stress.Level)))), # Corrección aquí
        legend = rownames(con8))
# Ajustar los márgenes de la leyenda
layout(matrix(c(1, 1, 1, 2), ncol = 1), heights = c(5, 1))

# Tabla de contingencia entre Calidad del Sueño y Nivel de Estrés
con9 <- xtabs(~ Quality.of.Sleep + Stress.Level, data = sleep_imp)
con9
addmargins(con9)

chisq.test(con9, correct = FALSE)
fisher.test(con9, simulate.p.value = TRUE)
library(MASS)
loglm( ~ 1 + 2, data = con9)

# Gráfico de barras apiladas
barplot(con9, 
        main = "Calidad del Sueño por Nivel de Estrés", 
        xlab = "Calidad del Sueño", 
        ylab = "Frecuencia",
        col = rev(heat.colors(length(unique(sleep_imp$Stress.Level)))),  # Colores para cada nivel de estrés
        legend = rownames(con9),
        beside = TRUE)  # Barras apiladas una al lado de la otra
# Ajustar los márgenes de la leyenda
layout(matrix(c(1, 1, 1, 2), ncol = 1), heights = c(5, 1))


# Tabla de contingencia entre Ocupación y Duración del Sueño
con10 <- xtabs(~ Occupation + Sleep.Duration, data = sleep_imp)
con10
addmargins(con10)

chisq.test(con10, correct = FALSE)
fisher.test(con10, simulate.p.value = TRUE)
library(MASS)
loglm( ~ 1 + 2, data = con10)


# Calcular la duración promedio del sueño para cada ocupación
avg_sleep_duration <- tapply(sleep_imp$Sleep.Duration, sleep_imp$Occupation, mean)

# Ordenar las ocupaciones por duración promedio del sueño
sorted_avg_sleep_duration <- avg_sleep_duration[order(avg_sleep_duration)]

# Crear el gráfico de barras agrupadas
barplot(sorted_avg_sleep_duration, 
        main = "Duración Promedio del Sueño por Ocupación",
        xlab = "Ocupación",
        ylab = "Duración Promedio del Sueño (horas)",
        col = heat.colors(length(sorted_avg_sleep_duration)), 
        las = 2,  # Orientación de las etiquetas del eje x
        cex.names = 0.8)  # Tamaño de las etiquetas del eje x



# 3.1. Medidas de independencia: 
# 3.1.1 Coeficiente Ji-cuadrado y 3.1.2 Prueba exacta de Fisher 

#comprobamos si rechazamos o aceptamos 
#H0 : Las variables son independientes.
#H1 : Las variables no son independientes.

p.values_chiq <- data.frame (chisq.test(con1,correct = F)[3], chisq.test(con2, correct=F)[3], chisq.test(con3, correct=F)[3],
                             chisq.test(con4, correct=F)[3],chisq.test(con5, correct=F)[3],chisq.test(con6, correct=F)[3],
                             chisq.test(con7, correct=F)[3],chisq.test(con8, correct=F)[3],chisq.test(con9, correct=F)[3],
                             chisq.test(con10, correct=F)[3])

p.values_chiq

#rechazamos o aceptamos
rechazar_h0 <- apply (p.values_chiq,2, function(x) any(x < 0.05)) #me duevuelve true si rechazo
rechazar_h0


#p valor de fisher
p.values_fisher <- data.frame (fisher.test(con1,simulate.p.value=TRUE)[1], fisher.test(con2,simulate.p.value=TRUE)[1], fisher.test(con2,simulate.p.value=TRUE)[1],
                               fisher.test(con4,simulate.p.value=TRUE)[1],fisher.test(con5,simulate.p.value=TRUE)[1],fisher.test(con6,simulate.p.value=TRUE)[1],fisher.test(con7,simulate.p.value=TRUE)[1],
                               fisher.test(con8,simulate.p.value=TRUE)[1],fisher.test(con9,simulate.p.value=TRUE)[1],fisher.test(con10,simulate.p.value=TRUE)[1])
p.values_fisher

rechazar_h0_fisher <- apply (p.values_fisher,2, function(x) any(x < 0.05)) #me duevuelve true si rechazo
rechazar_h0_fisher



#3.2.Medidas de Asociación: Escala nominal
#3.2.1. Coeficiente de contingencia C de Pearson: grado de relación o dependencia entre dos caracteres en la tabla de contingencia

install.packages("DescTools")
library(DescTools)
ContCoef(con1)

Contingencia_C_Pearson <- data.frame(ContCoef(con1))        

Contingencia_C_Pearson




#3.2.2 V de Cramer:la asociación entre dos atributos nominales no dicotómicos
install.packages("vcd")
library(vcd)
# Calcular V de Cramer para cada tabla de contingencia
V_Cramer <- data.frame(assocstats(con1)$crame)

V_Cramer


#3.3 Medidas de Asociación: Escala Ordinal
#3.3.1 Coeficiente gamma de Goodman y Kruskal
#Si las dos variables son independientes, el estimador gamma tiende a aproximarse a cero.
library(DescTools)
gamma_con9 <- GoodmanKruskalGamma(con9,conf.level=0.95)
gamma_con9

#3.3.2. Coeficiente D de Somers
#medida de la fuerza y la dirección de la asociación entre una variable dependiente ordinal y una variable independiente ordinal.
library(DescTools)
# D de Somers C|R
SomersDelta(con9, direction="column", conf.level=0.95)
# D de Somers R|C
SomersDelta(con9, direction="row", conf.level=0.95)


#3.3.3. Coeficiente τb de Kendall
#τb de Kendall es una medida no paramétrica de la correlación para variables ordinales o de rangos que tiene en consideración los empates
library (DescTools)
KendallTauB(con9, conf.level=0.95)

# Verificar si hay empates en las filas
empates_filas <- any(apply(con9, 1, function(x) any(duplicated(x))))
cat("¿Hay empates en las filas de con9?", ifelse(empates_filas, "Sí", "No"), "\n")

# Verificar si hay empates en las columnas
empates_columnas <- any(apply(con9, 2, function(x) any(duplicated(x))))
cat("¿Hay empates en las columnas de con9?", ifelse(empates_columnas, "Sí", "No"), "\n")


#3.3.4. Coeficiente de Concordancia τc de Kendall
#medida no paramétrica de asociación para variables ordinales que ignora los empates.
library(DescTools)
StuartTauC(con9, conf.level=0.95)

#3.4. Relación entre dos variables cuantitativas 
#3.4.1. Coeficiente de Correlacción de Pearson


# Seleccionar solo las variables cuantitativas
variables <- sleep_imp[, c("Age", "Daily.Steps", "Sleep.Duration", "Physical.Activity.Level")]

# Calcular la matriz de correlación de Pearson
correlation_matrix <- cor(variables)


# Mostrar la matriz de correlación
print(correlation_matrix)


# Convertir la matriz de correlación en un data frame
correlation_df <- as.data.frame(correlation_matrix)

plot(correlation_df,col="blue")


# Lo combertimos en un excel 
library(writexl)
# Escribir el data frame en un archivo Excel
write_xlsx(correlation_df, "correlation_matrix.xlsx")

# Obtener el directorio de trabajo actual
current_directory <- getwd()

# Verificar la ubicación del archivo
file_path <- file.path(current_directory, "correlation_matrix.xlsx")

# Imprimir la ubicación del archivo
print(file_path)

# Las correlaciones más altas son las de Daily.Steps and Physical.Activity.level, hacemos un estudio más profundo de ellas.

# Calculamos la correlacion entre hombres y mujeres 
cor_Steps_Activity <- cor(sleep_imp$Daily.Steps,sleep_imp$Physical.Activity.Level)

cor_hombres_Steps_Activity <- cor(sleep_imp$Daily.Steps[sleep_imp$Gender == "Male"], sleep_imp$Physical.Activity.Level[sleep_imp$Gender == "Male"])

cor_mujeres_Steps_Activity <- cor(sleep_imp$Daily.Steps[sleep_imp$Gender == "Female"], sleep_imp$Physical.Activity.Level[sleep_imp$Gender == "Female"])

Steps_Activity <- data.frame(
  All = cor_Steps_Activity,
  Men = cor_hombres_Steps_Activity,
  Women = cor_mujeres_Steps_Activity,
  row.names = c("Correlation Steps and Physical Activity")
)
Steps_Activity

# Las correlaciones más bajas son las de Daily.Steps and Sleep.Duration, hacemos un estudio más profundo de ellas.

cor_Steps_Sleep <-  cor(sleep_imp$Daily.Steps,sleep_imp$Sleep.Duration)

cor_hombres_Steps_Sleep <- cor(sleep_imp$Daily.Steps[sleep_imp$Gender == "Male"], sleep_imp$Sleep.Duration[sleep_imp$Gender == "Male"])

cor_mujeres_Steps_Sleep <- cor(sleep_imp$Daily.Steps[sleep_imp$Gender == "Female"], sleep_imp$Sleep.Duration[sleep_imp$Gender == "Female"])

Steps_Sleep <- data.frame(
  All = cor_Steps_Sleep,
  Men = cor_hombres_Steps_Sleep,
  Women = cor_mujeres_Steps_Sleep,
  row.names = c("Correlation Steps and Sleep Duration")
)
Steps_Sleep


# También nos interesa la correlación entre Sleep.Duration y Physical.Activity.Level

cor_Sleep_Activity <-  cor(sleep_imp$Sleep.Duration,sleep_imp$Physical.Activity.Level)

cor_hombres_Sleep_Activity <- cor(sleep_imp$Sleep.Duration[sleep_imp$Gender == "Male"], sleep_imp$Physical.Activity.Level[sleep_imp$Gender == "Male"])

cor_mujeres_Sleep_Activity <- cor(sleep_imp$Sleep.Duration[sleep_imp$Gender == "Female"], sleep_imp$Physical.Activity.Level[sleep_imp$Gender == "Female"])

Sleep_Activity <- data.frame(
  All = cor_Sleep_Activity,
  Men = cor_hombres_Sleep_Activity,
  Women = cor_mujeres_Sleep_Activity,
  row.names = c("Correlation Sleep Duration and Activity")
)
Sleep_Activity



#3.4.2.la correlacion de Spearman 
# Seleccionar solo las variables cuantitativas
variables <- sleep_imp[, c("Age", "Daily.Steps", "Sleep.Duration", "Physical.Activity.Level")]

# Calcular el coeficiente de correlación de Spearman
cor_spearman <- cor(variables, method = "spearman")

# Mostrar la matriz de correlación de Spearman
print(cor_spearman)

# Convertir la matriz de correlación en un data frame
correlation_spearman_df <- as.data.frame(cor_spearman)

plot(correlation_spearman_df,col="red")


# Lo combertimos en un excel 
library(writexl)
# Escribir el data frame en un archivo Excel
write_xlsx(correlation_spearman_df, "correlation_Spearman_matrix.xlsx")

# Obtener el directorio de trabajo actual
current_directory <- getwd()

# Verificar la ubicación del archivo
file_path <- file.path(current_directory, "correlation_Spearman_matrix.xlsx")

# Imprimir la ubicación del archivo
print(file_path)



#__________________________________________________________________________________________________
#4. Análisis de Componentes Principales (PCA)

# Seleccionar las variables cuantitativas de interés
data_pca <- sleep_imp[, c("Age", "Sleep.Duration", "Physical.Activity.Level", "Daily.Steps")]

#4.1 comprobar con la matriz de correlaciones
cor(data_pca)

#4.2. Media y varianza de la expresión de cada gen (muestra de los 10 primeros). 
apply(X = data_pca, MARGIN = 2, FUN = mean)
apply(X = data_pca, MARGIN = 2, FUN = var)

#4.3. Cálculo de componentes principales
#Es importante estandarizar las variables (genes) para que tengan desviación estándar 
#igual a 1 antes de aplicar PCA
# Estandarizar los datos: Estandariza las variables para que tengan la misma escala (media = 0 y desviación estándar = 1).

data_scaled <- scale(data_pca)
pca <- prcomp(data_scaled, scale = TRUE)

names(pca_nci)

summary(pca)
pca$center

# Muestra de los componentes
pca$rotation

#4.4. ¿Cuántas componenetes principales distintas?
dim(pca$rotation)


#4.5.Varianza explicada por cadad componente prinicpal.
#La varianza explicada por cada componente principal (correspondiente a los eigenvalores) 
#la varianza explicada es mayor en la primera componente que en las subsiguientes.

install.packages("FactorMineR")
library(FactoMineR)

pca2 <- PCA(X = data_pca, scale.unit = TRUE, ncp = 4, graph = FALSE)
print(pca2)
pca2$eig


#4.6 Representacion grafica
install.packages("factoextra")
library(factoextra)
#aporte indiovidual
fviz_pca_ind(pca, col.ind="cos2", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel=TRUE)


#Aporte variables
fviz_pca_var(pca, col.var="contrib", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel=TRUE)


#4.7 Proporción de varianza explicada y acumulada
PVE <- 100*pca$sdev^2/sum(pca$sdev^2)
PVE

#representarlo gráficamente
par(mfrow = c(1,2))

plot(PVE, type = "o", 
     ylab = "PVE", 
     xlab = "Componente principal", 
     col = "blue")

plot(cumsum(PVE), type = "o", 
     ylab = "PVE acumulada", 
     xlab = "Componente principal", 
     col = "brown3")


#Selección del numero de componenetes principales.
fviz_screeplot(pca_nci, addlabels = TRUE, ylim = c(0, 50)) 

#Mostrar solo aquellas con mayor contribución.
fviz_contrib(pca_nci, choice = "var", axes = 1, top = 4)


#Con la función get_pca_var() del paquete factoextra podemos extraer los resultados 
#de las variables a partir de un objeto pca
var <- get_pca_var(pca)
var
var$coord
var$cos2
var$contrib

#Biplot.
#El biplot es una forma más con la que podemos visualizar el resultado de un PCA. 
biplot(pca, scale = 0, col = c("dodgerblue3", "deeppink3")) 




#_________________________________________________________________________________________________________
#5 Analisis de correspondencias
library(FactoMineR)
library(factoextra)
library(gplots)

# Seleccionar las variables categóricas de interés
data_mca <- sleep_imp[, c("Gender", "Occupation", "Quality.of.Sleep", "Stress.Level")]

# Crear la matriz para el balloonplot
mca_table <- table(data_mca$Gender, data_mca$Occupation, data_mca$Quality.of.Sleep, data_mca$Stress.Level)
data <- as.table(as.matrix(mca_table))

# Generar el gráfico de tipo balloonplot
balloonplot(t(data), main ="Análisis de Correspondencia Múltiple", 
            xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

#Chissq test
chisq_Gender.Occupation <- chisq.test(sleep_imp$Gender,sleep_imp$Occupation)
chisq_Gender.Occupation

chisq_Gender.QualityofSleep <- chisq.test(sleep_imp$Gender,sleep_imp$Quality.of.Sleep)
chisq_Gender.QualityofSleep

chisq_Gender.StressLevel <- chisq.test(sleep_imp$Gender,sleep_imp$Stress.Level)
chisq_Gender.StressLevel

chisq_Occupation.QualityofSleep <- chisq.test(sleep_imp$Occupation,sleep_imp$Quality.of.Sleep)
chisq_Occupation.QualityofSleep

chisq_Occupation.StressLevel <- chisq.test(sleep_imp$Occupation,sleep_imp$Stress.Level)
chisq_Occupation.StressLevel

chisq_QualityofSleep.StressLevel <- chisq.test(sleep_imp$Quality.of.Sleep,sleep_imp$Stress.Level)
chisq_QualityofSleep.StressLevel

chisq_ <- data.frame(chisq_Gender.Occupation$p.value,chisq_Gender.QualityofSleep$p.value,
                     chisq_Gender.StressLevel$p.value,chisq_Occupation.QualityofSleep$p.value,
                     chisq_Gender.StressLevel$p.value,chisq_QualityofSleep.StressLevel$p.value)

# Realizar el análisis de correspondencia múltiple

mca <- MCA(data_mca, graph = FALSE)


# Resumen de los resultados
print(mca)

# Varianza explicada por cada dimensión
print(mca$eig)

#scree plot  fviz_eig() or fviz_screeplot() [factoextra package].
fviz_screeplot(mca, addlabels = TRUE, ylim = c(0, 50))

#diagrama de dispersion con una linea discontinua roja que especifica el autovalor medio:

fviz_screeplot(mca) +
  geom_hline(yintercept=33.33, linetype=2, color="red")

# Gráfico de los individuos
fviz_mca_ind(mca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# Gráfico de las variables
fviz_mca_var(mca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)


# Selección del número de dimensiones
fviz_screeplot(mca, addlabels = TRUE, ylim = c(0, 50))

# Contribución de las variables a las dimensiones
fviz_contrib(mca, choice = "var", axes = 1:2, top = 4)


# Biplot
fviz_mca_biplot(mca, repel = TRUE)

# Gráfico por individuos
ind <- get_mca_ind(mca)

# Coordenadas de los puntos de individuos
head(ind$coord)

# Cos2: calidad en el mapa factorial
head(ind$cos2)

# Contribuciones a las dimensiones
head(ind$contrib)

# Visualizar solo los puntos de individuos
fviz_mca_ind(mca, repel = TRUE)

# Personalizar el gráfico de individuos
fviz_mca_ind(mca, col.ind = "steelblue", shape.ind = 15)

# Calidad de la representación de los individuos
head(ind$cos2, 4)
fviz_mca_ind(mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)
fviz_mca_ind(mca, alpha.ind = "cos2")
library("corrplot")
corrplot(ind$cos2, is.corr = FALSE)
fviz_cos2(mca, choice = "ind", axes = 1:2)

# Contribución de los individuos a las dimensiones
head(ind$contrib)
corrplot(ind$contrib, is.corr = FALSE)
fviz_contrib(mca, choice = "ind", axes = 1, top = 10)
fviz_contrib(mca, choice = "ind", axes = 2, top = 10)
fviz_contrib(mca, choice = "ind", axes = 1:2, top = 10)
fviz_mca_ind(mca, col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_mca_ind(mca, alpha.ind = "contrib", repel = TRUE)

# Gráfico de las variables
var <- get_mca_var(mca)
head(var$coord)
head(var$cos2)
head(var$contrib)

# Gráficos: calidad y contribución de las variables
fviz_mca_var(mca)
fviz_mca_var(mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_cos2(mca, choice = "var", axes = 1:2)
fviz_contrib(mca, choice = "var", axes = 1:2)

# Tipos de Biplot
fviz_mca_biplot(mca, repel = TRUE)
fviz_mca_biplot(mca, map = "ind.sup", arrow = c(TRUE, TRUE), repel = TRUE)
fviz_mca_biplot(mca, map = "var", arrow = c(TRUE, FALSE), repel = TRUE)

# Descripción de las dimensiones
res.desc <- dimdesc(mca, axes = c(1, 2))
head(res.desc[[1]]$ind, 4)
res.desc[[1]]$var
res.desc[[2]]$ind
res.desc[[2]]$var


#__________________________________________________________________________________________________________
#Análisis cluster 

#6.1 Analisis cluster no jerarquico 

# Seleccionar variables

variables.cluster <- sleep_imp[, c("Age","Occupation", "Sleep.Duration", "Quality.of.Sleep", "Physical.Activity.Level", "Stress.Level", "Daily.Steps")]

# Normalizar variables
scalar <- variables.cluster[,c("Sleep.Duration",  "Physical.Activity.Level")]
scaled_data <- scale(scalar)

#elección del número de clsuters
# Método de la silueta
# Calcula el valor de la silueta para diferentes valores de k y busca el valor máximo.

library(cluster)
library(factoextra)

set.seed(20)
wcss <- vector()
for(i in 1:20){
  wcss[i] <- sum(kmeans(scaled_data, i,nstart=20)$withinss)
}
wcss
library(ggplot2)
ggplot() +
  geom_point(aes(x = 1:20, y = wcss), color = "blue") +
  geom_line(aes(x = 1:20, y = wcss), color = "blue") +
  ggtitle("Metodo del Codo") +
  xlab("Cantidad de Centroides k") +
  ylab("WCSS")


#Se observa que el número óptimo clusters es K=6
set.seed(20)
km.out.6<-kmeans(scaled_data,6,nstart=20)
km.out.6$centers
km.out.6$ifault
grupos <- km.out.6$cluster


par(mfrow=c(1,1))
plot(data_scaled,col=(km.out.6$cluster+1) ,
     main="Resultados del grupamiento K-Means con K = 6",
     xlab="",ylab="",pch=20,cex=2)


## Tabular los resultados.
dif=data.frame(variables.cluster,grupos)

table(dif$grupos,dif$Stress.Level)
table(dif$grupos,dif$Occupation)
table(dif$grupos,dif$Age)

visualizar_tablas <- function(tabla, titulo) {
  barplot(tabla, beside = TRUE, legend.text = TRUE, main = titulo, col = rainbow(nrow(tabla)))
}

# Visualizar la relación entre grupos y niveles de estrés
visualizar_tablas(table(grupos, dif$Stress.Level), "Relación entre Grupos y Niveles de Estrés")
visualizar_tablas(table(dif$grupos,dif$Occupation),"Relación entre Grupos y Occupación")
visualizar_tablas(table(dif$grupos,dif$Age),"Relación entre Grupos y Edad")


#6.2 Analisis cluster jerarquico 

library(tidyverse)
library(FactoMineR)

variables.cluster <- sleep_imp[, c("Age","Occupation", "Sleep.Duration", "Quality.of.Sleep", "Physical.Activity.Level", "Stress.Level", "Daily.Steps")]

# Normalizar variables
scalar <- variables.cluster[,c("Sleep.Duration",  "Physical.Activity.Level")]
scaled_data <- scale(scalar)

# Calcular la matriz de distancias
dist_matrix <- dist(data)

# Realizar el análisis de clústeres jerárquico
hc_complete <- hclust(dist_matrix, method = "complete")
hc_average <- hclust(dist_matrix, method = "average")
hc_single <- hclust(dist_matrix, method = "single")

# Visualizar los resultados
# Vinculación completa
fviz_dend(hc_complete, k = 4,
          cex = 0.5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          rect = TRUE,
          main = "Vinculación completa")

# Vinculación promedio
fviz_dend(hc_average, k = 4,
          cex = 0.5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE,
          rect = TRUE,
          main = "Vinculación promedio")

# Vinculación simple
fviz_dend(hc_single, k = 4,
          cex = 0.5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          rect = TRUE,
          main = "Vinculación simple")





