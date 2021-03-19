# Identificación de Targets de Clientes (R)

####-----Librerias-----####
install.packages("lattice")
install.packages("vcd")
install.packages("ROCR")
library(lattice) # multivariate data visualization
library(vcd) # data visualization for categorical variables
library(ROCR) # evaluation of binary classifiers


####-----Dataset-----####
# Creamos un data frame a aprtir del csv de base
bank <- read.csv("bank.csv", sep = ";", stringsAsFactors = FALSE)

# imprimimos la estructra del dataseet
print(str(bank))
#la mayoria de las variable son tipo "character"

# imprimimos las primeras filas
print(head(bank))

# imprimimos los nombres de las columnas
print(names(bank))


####-----Analisis univariado de "age"-----####
# imprimos el tipo de la varaible "age" 
print(class(bank$age))

#imprimimos los atributos de la variable "age"
print(attributes(bank$age))
#cuando arroja NULL significa que no fue definida con atributos especiales

# histograma de "age"
hist(bank$age)


####-----Tabla de frecuencia - variables categoricas-----####
# a traves de una tabla de frecuencia examinamos las variables categroicas o tipo factor
# la funcion "table permite usar un parametro que ese "useNA" el cual permite 
#indicarle que deseamos que muestre la cantidad de Nulos en la impresión de la 
# tabla si le indicamos el comando c("always")
print(table(bank$job , useNA = c("always"))) 
print(table(bank$marital , useNA = c("always")))
print(table(bank$education , useNA = c("always")))
print(table(bank$default , useNA = c("always")))
print(table(bank$housing , useNA = c("always")))
print(table(bank$loan , useNA = c("always")))


####-----analisis univariado de "Jobs"-----####
# dividimos la variable Job en tres categorias o tipos y la guardamos en la vairiable "jobtype"
# claramente la etiqueta "unknown" es data faltante

empleos_por_encima_de_la_media <- c("admin.","entrepreneur","management","self-employed")
empleos_por_debajo_de_la_media <- c("blue-collar","services","technician")

#ara crear la nueva variable "jobtype" primero utilizamos
# la función tipe replica el valor indicado cerando un ventar de la longitud indicada en lenght
# creamos la variable jobtype que por el momento para todos sus registros posee el valor 3
bank$jobtype <- rep(3, length = nrow(bank))
bank$jobtype

# indicamos que si la variable Job coincide con las categorias dentro de
# "empleos_por_encima_de_la_media" entonces e correpondera el valor 1
# entonces la variable "jobtype" por el momento poseera unicamente los valores 3 y 1
bank$jobtype <- ifelse((bank$job %in% empleos_por_encima_de_la_media), 1, bank$jobtype)
bank$jobtype

# idem anterior pero agregamos la otra cwetegoria creada, por lo tanto
# ya tenemos completa la varaible "jobtype" ya que el numero 3 quedara para los trabajos desconocidos
bank$jobtype <- ifelse((bank$job %in% empleos_por_debajo_de_la_media), 2, bank$jobtype)
bank$jobtype

# hacemos de la varaible "jobtype un variable tipo factor indicandole sus niveles o etiquetas
bank$jobtype <- factor(bank$jobtype, levels = c(1, 2, 3),
                       labels = c("encima de la media", "debajo de la media", "otros"))
bank$jobtype

# de la base de datos bank, tomamos lsa variables "job" y jobtype"
# para ver con que frecuencia parecen los empelos en cada categoria de jobtype
with(bank, table(job, jobtype, useNA = c("always")))


####-----Creación de variables tipo Factor-----####
bank$marital <- factor(bank$marital,
                       labels = c("Divorced", "Married", "Single"))

bank$education <- factor(bank$education,
                         labels = c("Primary", "Secondary", "Tertiary", "Unknown"))

bank$default <- factor(bank$default, labels = c("No", "Yes"))

bank$housing <- factor(bank$housing, labels = c("No", "Yes"))

bank$loan <- factor(bank$loan, labels = c("No", "Yes"))

bank$response <- factor(bank$response, labels = c("No", "Yes"))


####-----Creación de muestra-----####

# seleccionamos los casos que aun no han sido contactados por campañas de marketing
# es decir los que tienen valor 0 (cero) en la variable "previous"
# y de esos casos tomamos las variables que nos permitiran sar clasificadoras en el modelo

bankwork <- subset(bank, subset = (previous == 0),
                   select = c("response", "age", "jobtype", "marital", "education",
                              "default", "balance", "housing", "loan"))
#la funcion "subset" devuelve una muestra del data frame u objeto pasado en la función
# dentro de ella tenemos el parametro "subset" que permite indicar la condición
# de las observaciones que seleccionaremos dentro de la muestra


# examinamos estructura de la muestra
print(str(bankwork))
# observamos las primeras filas
print(head(bankwork))
# corremos un summary
print(summary(bankwork))



####-----Relacion de Age sobre acpetación-----####

# examinamos la relacion entre la edad y la respuesta a la promoción
# creamos pdf de salida
pdf(file = "fig_targeting_customers_age_lattice.pdf",
    width = 8.5, height = 8.5)

# Creamos un objeto de plot con un histograma
lattice_plot_object <- histogram(~age | response, #indicamos que la variable response" es dependiente de age
                                 data = bankwork, #indicamos data framde fuente
                                 type = "density", #tipo de plot
                                 xlab = "Age of Bank Client",
                                 layout = c(1,2)) #salida en dos paneles 1 fila  y 2 columnas

print(lattice_plot_object)
# la mayor cantidad de "si" se da entre los 30 y 50 años aprox
dev.off()
####-----Relacion de Educacion sobre acpetación-----####

# creamos tabla de frecuencia para educación
# la etiqueta "unknown" corresponde con datos no tomados

# con la muestra creamos una tabla de frecuencias de las columnas educacion en funcion respuesta, añadiendo cantidad nulos
with(bankwork, print(table(education, response, useNA = c("always"))))

# con el paquete "vcd" cremos un plot de tipo mosaico
# hacemos el pdf de salida
pdf(file = "fig_targeting_customers_education_mosaic.pdf",
    width = 8.5, height = 8.5)

mosaic( ~ response + # variable dependiente
          education, #varaible independiente
        data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer",
                                              education = "Education Level")),
        highlighting = "education", #resaltamos ese vector
        highlighting_fill = c("cornsilk","violet","purple","white",
                              "cornsilk","violet","purple","white"),
        rot_labels = c(left = 0, top = 0), #rotación de las etiquetas del plot
        #con esto las etiquetas "yes" y "no" del eje y quedanen forma horizontal y no vertical
        pos_labels = c("center","center"), #centra las etiquetas
        offset_labels = c(0.0,0.6))

dev.off()
####-----Relacion de jobtype sobre aceptación-----####

# tabla de frecuencias para jobtype
with(bankwork, print(table(jobtype, response, useNA = c("always"))))

# armamos pdf de salida
pdf(file = "fig_targeting_customers_jobtype_mosaic.pdf",
    width = 8.5, height = 8.5)

mosaic( ~ response + jobtype, data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer",
                                              jobtype = "Type of Job")),
        highlighting = "jobtype",
        highlighting_fill = c("cornsilk","violet","purple",
                              "cornsilk","violet","purple"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"), offset_labels = c(0.0,0.6))

dev.off()
####-----Relacion de Estado Civil sobre acpetación-----####

# tabla de frecuencias de MArital
# los que no estan dentro de soltero o casado se clasificaron como "divorced"
with(bankwork, print(table(marital, response, useNA = c("always"))))

#creamos pdf de salida
pdf(file = "fig_targeting_customers_marital_mosaic.pdf",
    width = 8.5, height = 8.5)

mosaic( ~ response + marital, data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer",
                                              marital = "Marital Status")),
        highlighting = "marital",
        highlighting_fill = c("cornsilk","violet","purple",
                              "cornsilk","violet","purple"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))

dev.off()

####-----Relacion de default sobre acpetación-----####

#tabla de frecuencias para "deafault"
with(bankwork, print(table(default, response, useNA = c("always"))))

pdf(file = "fig_targeting_customers_default_mosaic.pdf",
    width = 8.5, height = 8.5)

mosaic( ~ response + default, data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer",
                                              default = "Has credit in default?")),
        highlighting = "default",
        highlighting_fill = c("cornsilk","violet"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))

dev.off()

####-----Relacion de saldo promedio sobre aceptación-----####

pdf(file = "fig_targeting_customers_balance_lattice.pdf",
    width = 8.5, height = 8.5)

lattice_plot_object <- histogram(~balance | response, data = bankwork,
                                 type = "density",
                                 xlab = "Bank Client Average Yearly Balance (in dollars)",
                                 layout = c(1,2))

print(lattice_plot_object) 

dev.off()

####-----Relacion de pmo hipotecario sobre aceptación-----####

with(bankwork, print(table(housing, response, useNA = c("always"))))

pdf(file = "fig_targeting_customers_housing_mosaic.pdf",
    width = 8.5, height = 8.5)

mosaic( ~ response + housing, data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer",
                                              housing = "Has housing loan?")),
        highlighting = "housing",
        highlighting_fill = c("cornsilk","violet"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))

dev.off()

####-----Relacion de pmo personal sobre aceptación-----####

with(bankwork, print(table(loan, response, useNA = c("always"))))

pdf(file = "fig_targeting_customers_loan_mosaic.pdf",
    width = 8.5, height = 8.5)

mosaic( ~ response + loan, data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer",
                                              loan = "Has personal loan?")),
        highlighting = "loan",
        highlighting_fill = c("cornsilk","violet"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))

dev.off()

####-----boxplot multivariable sobre response-----####
ggplot(data = bankwork, aes(x = balance, y = response, colour = marital)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "bottom")

####-----Modelo predictivo-----####

# especificaciones de las variables predcitoras en función de la varaible target:
# "response"
bank_spec <- {response ~ age + jobtype + education + marital +
    default + balance + housing + loan}

# Modelo de Regresión Logistica.
## Es un método de regresión que permite estimar la probabilidad de una variable
## cualitativa binaria en función de una variable cuantitativa. Una de las 
## principales aplicaciones es la de clasificación binaria, en el que las 
## observaciones se clasifican en un grupo u otro dependiendo del valor que tome 
## la variable predictora.

## la diferencia con la regresion es lineal es que al ajustar por metodo de
## Minimos cuadrados (la diferencia al cuadrado de los valores reales vs los
## valores pedichos, haciendo que se magnifiquen las desviaciones más extremas.
## caunto mayor es el residuos, menor sera la calidad de predicción)

## lo que sucede es que al ser una recta (en regresion lineal) los valores extremos
## pueden tomar valor menores a 0 y mayores a 1, dificultando el analisis ya que 
## la condicion es que los valores de probabilidad sean entre 0 y 1

## la regresión logística transforma el valor devuelto por la regresión lineal (β0+β1X) 
## empleando una función cuyo resultado está siempre comprendido entre 0 y 1. La
## mas usada es la funcion sigmoide.

## la función sigmoide convertira en 0 de valores negativos muy grandes y en 1
## los valores positivos grandes. 1 / (1+(e^-x))

## en la función sigmoide reemplazamos a la x, por la función de regresión lineal.
## es decir (β0+β1X). Y a esto lo igualamos con Pr(Y=k|X=x),  el cual significa 
## la probabilidad de que Y sea igual a K verdaero (1: prediccion bien hecha)
## cuando el predictor X, toma el valor x.

##la versión logaritmica de esto sería "ln [p / (1 - p)]". Entonces quedaría
## "ln [p(Y=k|X=x) / (1 - p(Y=k|X=x))] = (β0+β1X)" 

## en una regresion logistica simple se modela la probabilidad de que la variable 
## respuesta Y pertenezca al nivel de referencia 1 (prediccion verdadera) 
## en función del valor que adquieran los predictores, mediante el uso de LOG of ODDs

## la razón de probabilidad de verdadero se definen como el ratio entre 
## la probabilidad de evento verdadero y probabilidad de evento falso. Es decir
## si la p() verdadera de un evento es 0.8, la p() falsa sera 0.2, entonces la razon de probabilidad
## sera de 0.8/0.2 = 4; que es lo mismo que decir que se esperan 4 eventos verdaderos
## por cada evento falso. si a ese valor le aplicamos el logaritmo se lo 
## conoce como """ODDs""".

## para interpretar el modelo simple podemos aclara que β1 indica el cambio en 
## el logaritmo de ODDs debido al incremento de una unidad de X


## REGRESION LOGÍSTICA MULTIPLE
## es identica a la logistica simple pero en vez aplicar logaritmo natural, aplicamos su inversa
## es decir, "e/(1+e)"
## Quedaría: p(Y)= [e^(β0+β1x1+β2x2+...+βixi)] / [1+e^(β0+β1x1+β2x2+...+βixi)]

bank_fit <- glm(bank_spec, # detallamos las variables
                family=binomial, #tipo de error a calcular
                data=bankwork) #base de datos fuente

print(summary(bank_fit))
# segun el modelo el logaritmo de ODDs de que un cliente responda si esta 
# negativamente relacionado con la variable maritalMarried segun un coeficiente de 
#-5.717e-01 y siendo significante (p-value = 0.000608 [menor a 0.05])

# generamos sus correspondientes intervalos de confianza
confint(bank_fit)

# analizamos la varianza
print(anova(bank_fit, test="Chisq"))
# la función "Anova" realiza una analisis de la varianza del modelo, 
# en este caso a través del metodo Chi Cuadrado

# calculamos la probabilidad de predecir que van a tomar el tren
# creamos la variable "Predict_Prob_Response"
bankwork$Predict_Prob_Response <- predict.glm(bank_fit, #detallamos el modelo
                                              type = "response") #detallamos variable a predecir
bankwork$Predict_Prob_Response
# con esto ultimo que calculamos, obtuvimos una probabilidad de respuesta positiva.
# esto quiere decir que cuanto mas cercano a 1 sea, mayor sera la posibilidad de 
# que la respuesta a la campaña sea positiva.

# creamos pdf de salida
pdf(file = "fig_targeting_customer_log_reg_density_evaluation.pdf",
    width = 8.5, height = 8.5)

plotting_object <- densityplot( ~ Predict_Prob_Response | response,
                                data = bankwork,
                                layout = c(1,2), aspect=1, col = "darkblue",
                                plot.points = "rug",
                                xlab="Predicted Probability of Responding to Offer")

print(plotting_object)

dev.off()

# predijimos la respuesta con un corte de 0.5
# no funciona ya que poseemos una baja tasa de respuesta
# al usar el 0.5 de corte predecimos que toda la base responde que NO.
# si nos fueramos al otro extremo targeteariamos dentro del "SI" a muchos clientes
# que optarian por no responder, por lo tanto sería falso.
# osea nosotros indicamos que si la probabilidad de acertar es mayor a 0.5 se 
# corresponde con el valor NO


# para todos las predicciones con valores mas altos que el corte, es decir, 0.5
# le asignamos el valor 2
# caso contrario 1
bankwork$Predict_Response <-
  ifelse((bankwork$Predict_Prob_Response > 0.5), 2, 1)
bankwork$Predict_Response

# convertimos a la nueva variable en factor con las etiquetas 
# "yes"para el valor 1; y 
# "no" para el valor 2
bankwork$Predict_Response <- factor(bankwork$Predict_Response,
                                    levels = c(1, 2), labels = c("NO", "YES"))
bankwork$Predict_Response

#construimos una tabla de confusión o frecuencias
confusion_matrix <- table(bankwork$Predict_Response, bankwork$response)
cat("\nConfusion Matrix (rows=Predicted Response, columns=Actual Choice\n")
#las filas son la respuesta segun predicción y las columnas la decicsion real
print(confusion_matrix)

# porcentaje de aciertos en la predicción
predictive_accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2])/
  sum(confusion_matrix)
predictive_accuracy
cat("\nPercent Accuracy: ", round(predictive_accuracy * 100, digits = 1))
#como vemos acertamos el 90% de las respuestas

# bajamos el numero de corte
# probamos con 0.1 en vez de 0.5
bankwork$Predict_Response <-
  ifelse((bankwork$Predict_Prob_Response > 0.1), 2, 1)
bankwork$Predict_Response

bankwork$Predict_Response <- factor(bankwork$Predict_Response,
                                    levels = c(1, 2), labels = c("NO", "YES"))

confusion_matrix <- table(bankwork$Predict_Response, bankwork$response)
cat("\nConfusion Matrix (rows=Predicted Response, columns=Actual Choice\n")
print(confusion_matrix)

predictive_accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2])/
  sum(confusion_matrix)
cat("\nPercent Accuracy: ", round(predictive_accuracy * 100, digits = 1))

# ploteamos un mosaico con las predicciones segun el 0.1 de corte
with(bankwork, print(table(Predict_Response, response, useNA = c("always"))))

pdf(file = "fig_targeting_customers_confusion_mosaic_10_percent.pdf",
    width = 8.5, height = 8.5)

mosaic( ~ Predict_Response + response, data = bankwork,
        labeling_args = list(set_varnames =
                               c(Predict_Response =
                                   "Predicted Response to Offer (10 percent cut-off)",
                                 response = "Actual Response to Offer")),
        highlighting = c("Predict_Response", "response"),
        highlighting_fill = c("green","cornsilk","cornsilk","green"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))

dev.off()


# es ideal poner un corte que maximice la ganancia, dado el ingreso y costo unitario 
# con cada celda de la amtriz de confusión.

# una opción para seleccionar el mejor corte puede ser calcular el "lift":
# la tasa de predicciones del modelo sobre la tasa de respuesta real. 

# calculamos el lift mediante la funcion "prediction()" del paquete ROCR
# y lo ploteamos

# armamos variable con la predicción y la respuesta verdadera
bankwork_prediction <-
  prediction(bankwork$Predict_Prob_Response, bankwork$response)
bankwork_prediction

bankwork_lift <- performance(bankwork_prediction,
                             "lift", # Lift value. P(Yhat = + | Y = +)/P(Yhat = +).
                             "rpp") # rpp: rate de predicciones positivas
bankwork_lift

pdf(file = "fig_targeting_customers_lift_chart.pdf",
    width = 8.5, height = 8.5)

plot(bankwork_lift,
     col = "blue", lty = "solid", main = "", lwd = 2,
     xlab = paste("Proportion of Clients Ordered by Probability",
                  " to Subscribe\n(from highest to lowest)", sep = ""),
     ylab = "Lift over Baseline Subscription Rate")
#obtenemos la proporcion de cliente con la probabilidad de suscribirse

# en el eje x tenemos la proprcion de clientes ordenados por su posibilidad de suscribirse
# en el eje x tenemos el valor de lift asociado
dev.off()


# calculo directo del lift
# creamos una variable nueva: "baselina_response_rate
# bankwork$response[2] = "NO", pero en formato numerico, es decir "1"
# dividido el numero de filas del data set "bankwork"
baseline_response_rate <-
  as.numeric(table(bankwork$response)[2])/nrow(bankwork)
baseline_response_rate
# esto da como resultado 0.09
# el ratio de respuestas

# aramamos una varaible con los deciles de la varaible "Predict_Prob_Response"
# es decir, la variable que indica la probabilidad numerica de responder
prediction_deciles <- quantile(bankwork$Predict_Prob_Response,
                               probs = seq(0, 1, 0.10), #crea una secuencia de 1 en 1 que va desde el 0 al 10
                               na.rm = FALSE)
prediction_deciles

# se reordenan los deciles del mayor al menor 
reordered_probability_deciles <- rev(as.numeric(prediction_deciles))

#calculo del lif
lift_values <- reordered_probability_deciles / baseline_response_rate

cat("\nLift Chart Values by Decile:", lift_values, "\n")




