install.packages("corrplot")
library(corrplot)

setwd("C:\\Users\\hibar\\Downloads\\archive")
base <- read.csv("ObesityDataSet.csv")
summary(base)
View(base)
dim(base)

#esto es para que la gráfica quede de mejor manera yno haya tanto espacio en blanco

n_tr<-list("Genero", "Edad", "Historial_Familiar", "FAVC", "FCVC", "NCP", "CAEC", "FUMADOR", "C_AGUA", "SCC", "FAF", "TUE", "CALC", "MTRANS", "OBES", "IMC")
colnames(base)<-n_tr

#Cuarto Bloque

base.cor = cor(base)
View(base.cor)
corrplot(base.cor)

pairs(base,       # Datos
      pch = 19, # Símbolo pch
      col = 4,  # Color
      main = "Correlaciones",    # Título
      gap = 0,           # Distancia entre gráficos
      row1attop = FALSE, # Dirección de la diagonal
      labels = colnames(base), # Etiquetas
      cex.labels = 0.8,  # Tamaño textos diagonales
      font.labels = 1)

#quinto bloque

modelo<-lm(base$IMC~base$Gender+base$Age+base$family_history_with_overweight+base$FAVC+base$FCVC+base$NCP+
             base$CAEC+base$SMOKE+base$CH2O+base$SCC+base$TUE+base$CALC+base$MTRANS)
hist(residuals(modelo), main= "Histograma de residuales",xlab = "Residuales", ylab="frecuencia")
summary(modelo)

#  este es para después detu código

# sexto bloque
ind<-subset( base, select = c(Age,family_history_with_overweight,FAVC,FCVC,CAEC,IMC))
n_tr<-list("Edad", "Historial_Familiar", "FAVC", "FCVC", "CAEC","IMC")
colnames(ind)<-n_tr
corind<-cor(ind)
corrplot(corind)

pairs(ind, main = "Correlaciones",col="red", labels = colnames(ind))

#septimo

modelo2<-lm(ind$IMC~ind$Edad+ind$Historial_Familiar+ind$FAVC+ind$FCVC+ind$CAEC)
hist(residuals(modelo2), main= "Histograma de residuales",xlab = "Residuales", ylab="frecuencia")
summary(modelo2)
