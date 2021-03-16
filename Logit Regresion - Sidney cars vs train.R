# Predicting Commuter Transportation Choices (R)

library(lattice) # multivariate data visualization

load("correlation_heat_map.RData") # from R utility programs

# read data from comma-delimited text file... create data frame object
sydney <- read.csv("sydney.csv")
sydney

#cambiamos los nombres de las columnas
names(sydney) <-
  c("Car_Time", "Car_Cost", "Train_Time", "Train_Cost", "Choice")
sydney

#creamos el df a paritr del cual vamos a plotear
plotting_data_frame <- sydney[, 1:4]
plotting_data_frame

## Matriz de scatterplots con regresiones lineales simples de 
## la relacion entre varaibles

# Modelos y "lowess smooth fits" for variable pairs.
# TEORIA: https://www.maximaformacion.es/blog-dat/que-es-la-regresion-local-loess-o-lowess/

pdf(file = "fig_predicting_choice_scatter_plot_matrix.pdf",
    width = 8.5, height = 8.5)#creamos un pdf con el grafico de salida

pairs(plotting_data_frame, #pairs: produce una matriz de graficos tipo scatter
      panel = function(x, y) { #función usual de la forma function(x,y,...) a 
                               #ser usada para determinar el contenido de los páneles. 
                               #Por defecto es points, indicando que se graficarán
                               #los puntos de los pares de variables
        points(x, y, cex = 0.5)#cex: sirve para modificar el tamaño de los símbolos, 
                              #nombres de los ejes, marcas de los ejes y títulos)
        abline(lm(y ~ x), lty = "solid", col = "red") #añadimos la linea de la regresion lineal
        lines(lowess(x, y), col ="blue")# crea una linea "sauvizada" del modelo
      }
)
dev.off() #shuts down the specified (by default the current) device. If the current device is shut down and any other devices are open, the next open device is made current.


#Mapa de calor de la correlación para las varaibles explicatvas
pdf(file = "fig_predicting_choice_correlation_heat_map.pdf",
    width = 8.5, height = 8.5) #creamos el pdf de salida

#cambiamos los nombres de las columnas
sydney_cormat <-
  cor(sydney[, c("Car_Time", "Car_Cost", "Train_Time", "Train_Cost")])

#usamos la funcion predefinida que ya hicimos el load al principio
#para generar la amtriz de correlación
correlation_heat_map(sydney_cormat)
dev.off()

# Especificaciones y ajuste del modelo de regresión Logistico
sydney_model <- {Choice ~ Car_Time + Car_Cost + Train_Time + Train_Cost}
#definimos la varaible choice como target en función de las otras 4 varaibles (predictoras)

sydney$Choice <- as.factor(sydney$Choice) 
#pasamos la variable choice a tipo factor para que corra la regresión logistica

sydney_fit <- glm(sydney_model, family=binomial(logit), data=sydney)
# glm: se usa para ajustar modelos de regresión
# se especifica una descripion simbolica del predictor lineal y
# una descripción del error de la distribución
#en este caso la formula es sydney_model; family es la descipción del error de distribución, 
#en este caso binomial; y la data. es decir, donde se enceuntran las variables es el 
#data frame de origen: sydney.

print(summary(sydney_fit))

print(anova(sydney_fit, test="Chisq"))

# calculamos la predicción y propabilidad de tomar un tren

#Creamos la varaible: "Predict_Prob_TRAIN"
#usamos la función "predict.glm": obtiene predicciones y puede estimar el Erro Std de los predictores del modelo linear ajustado
#indicamos tipo "response" para que indique las probabilidades de predicción
sydney$Predict_Prob_TRAIN <- predict.glm(sydney_fit, type = "response")
sydney$Predict_Prob_TRAIN

#creamos pdf
pdf(file = "fig_predicting_choice_density_evaluation.pdf",
    width = 8.5, height = 8.5)

#creamos el plot de densidad
plotting_object <- densityplot( ~ Predict_Prob_TRAIN | Choice, #variable que predecimos
                                data = sydney, #fuente de datos
                                layout = c(1,2), #divisiión del panel en 1 fila y 2 columnas
                                aspect=1, col = "darkblue", 
                                plot.points = "rug", #en vez de puntos ahce una mini grafico de desnidad de barras en el eje x
                                xlab="Predicted Probability of Taking Train") #nombramos al eje x

#la función "densityplot" crea un grafico de densidad o histograma y agrega la densidad estilo Kernel
#
print(plotting_object)
dev.off()


# Predicción de auto vs tren con 0.5 error
# creamos la variable "Predict_choice"
# si "Predict_Prob_TRAIN" es mayor a 0.5 entonces "Predict_choice" es 2; caso contrario 1
sydney$Predict_Choice <- ifelse((sydney$Predict_Prob_TRAIN > 0.5), 2, 1)

#convertimos "Predict_choice" en un factor de dos etiquetas: car=1 y train=2
sydney$Predict_Choice <- factor(sydney$Predict_Choice,
                                levels = c(1, 2), labels = c("CAR", "TRAIN"))

#creamos matriz de cobnfusión
#usamos la función table para crear tabla con "Predict_choice" y "Choice"
confusion_matrix <- table(sydney$Predict_Choice, sydney$Choice)

cat("\nConfusion Matrix (rows = Predicted Choice, columns = Actual Choice\n")
print(confusion_matrix)

#calculamos el porcentaje de acierto
#para ello seleccionamos los CAR/CAR y los TRAIN/TRAIN de la matriz, que serian los aciertos
#dividimos por el total de predicciones
predictive_accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2])/
  sum(confusion_matrix)
cat("\nPercent Accuracy: ", round(predictive_accuracy * 100, digits = 1))

# Cuanto mas bajo deben estar los tickets de tren para incrementar 
# el uso de transporte publico en un 10%

#creamos un vector con el cosot resuesta para los tikets de tren
#generamos una secuencia desde el precio minimo de tren hasta el precio maximo 
#con un total de 1000 registros
train_cost_vector <-
  seq(min(sydney$Train_Cost), max(sydney$Train_Cost), length=1000)
train_cost_vector
  
#creamos un vector con los coeficientes del modelo ajustado
beta.vector <- sydney_fit$coefficients
beta.vector

#creamos un vector de 1000 registros
train_probability_vector <- numeric(1000)
train_probability_vector
# para cada registro
for (i in 1:1000) {
  X.vector <- c(1, mean(sydney$Car_Time), mean(sydney$Train_Time),
                mean(sydney$Car_Cost), train_cost_vector[i]) 
  #creamos el vector x con la media de cada variable de los datos en sydney
  train_probability_vector[i] <-
  # multiplicamos "%*%" cada valor de las dos amtrices creadas "X" y "beta"
  # obtenemos su logaritmo "exp"
  #y la dividimos por la multiplicación de las matrices +1
    exp(X.vector %*% beta.vector)/
    (1 + exp(X.vector %*% beta.vector))
}
train_probability_vector

# 150 de 333 usa el tren
# determina el precio requerido para el 55% de la comunidad que toma el tren
# es es la cuota deseada seteada por la administración Publica 

index <- 1 # seteamos el indice para la busqueda

while (train_probability_vector[index] > 0.55) index <- index + 1
train_probability_vector[index]

Solution_Price <- train_cost_vector[index]
Solution_Price
cat("\nSolution Price: ", Solution_Price)

Current_Mean_Price <- mean(sydney$Train_Cost)
Current_Mean_Price

# cuanto mas se deben bajar los precios?
#definimos una varaible que tenga los centavos que se necesitan bajar para lograr el objetivo
Cents_Lower <- ceiling(Current_Mean_Price - Solution_Price)
#ceiling: devuelve un vectro numerico con el entero mas bajo
cat("\nLower prices by ", Cents_Lower, "cents\n")

#creamos pdf
pdf(file = "fig_predicting_choice_ticket_price_solution.pdf",
    width = 8.5, height = 8.5)

plot(train_cost_vector, train_probability_vector,
     type="l",ylim=c(0,1.0), las = 1,
     xlab="Cost of Taking the Train (in cents)",
     ylab="Estimated Probability of Taking the Train")

# plotemoas un linea vertical con el precio promedio del ticket de tren
abline(v = Current_Mean_Price, col = "red", lty = "solid", lwd = 2)
abline(v = Solution_Price, col = "blue", lty = "dashed", lwd = 2)
legend("topright", legend = c("Current Mean Train Ticket Price",
                              paste("Solution Price (", Cents_Lower, " Cents Lower)", sep = "")),
       col = c("red", "blue"), pch = c(NA, NA), lwd = c(2, 2),
       border = "black", lty = c("solid", "dashed"), cex = 1.25)
dev.off()


# Suggestions for the student:
# How much lower must train fares be to encourage more than 60 percent
# of Sydney commuters to take the train? What about car costs? How much
# of a tax would public administrators have to impose in order to have
# a comparable effect to train ticket prices?
# Evaluate the logistic regression model in terms of its out-of-sample
# predictive accuracy (using multi-fold cross-validation, for example).
# Try alternative classification methods such as tree-structured
# classification and support vector machines. Compare their predictive
# performance to that of logistic regression in terms of percentage
# of accurate prediction and other measures of classification performance.