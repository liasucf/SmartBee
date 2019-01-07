#Descobrir onde esta o programa do R
getwd()

# importando arqiuvo com os dados a serem pre-processados ************************************************
data <- read.csv("emil_1estacao.csv", header=TRUE)
mydata <- subset( data, select = c(date,Month, hive_weight,hive_temperature,hive_humidity, 
                                   ambient_temperature,ambient_humidity ))
mydata_preditors <- subset( mydata, select = c(hive_weight,hive_temperature,hive_humidity, 
                                               ambient_temperature,ambient_humidity ))



#Análise Exploratória dos dados
#Histogramas
hist(mydata$ambient_temperature, main="Histogram da Temperatura Exterior")
hist(mydata$ambient_humidity, main="Histogram da Umidade Exterior")
hist(mydata$hive_temperature, main="Histogram Temperatura da Colmeia")
hist(mydata$hive_humidity, main="Histogram Umidade da Colmeia")
hist(mydata$hive_weight, main="Histogram do Peso da Colmeia")
# calculo da media do valor de cada preditor independente da classe*******************************************
mean(mydata$ambient_temperature, trim = 0, na.rm = TRUE)
mean(mydata$ambient_humidity , trim = 0, na.rm = TRUE)
mean(mydata$hive_temperature , trim = 0, na.rm = TRUE)
mean(mydata$hive_humidity , trim = 0, na.rm = TRUE)
mean(mydata$hive_weight , trim = 0, na.rm = TRUE)
# calculo do desvio padrao de cada preditor independente das classes********************************************************************************
sd(mydata$ambient_temperature , na.rm = TRUE)
sd(mydata$ambient_humidity , na.rm = TRUE)
sd(mydata$hive_temperature , na.rm = TRUE)
sd(mydata$hive_humidity , na.rm = TRUE)
sd(mydata$hive_weight , na.rm = TRUE)


# package para calcular histogram condicionado a classe  ****************************************************************************
library(lattice)
attach(mydata)
histogram( ~ ambient_temperature | factor(Month) , mydata)
histogram( ~ ambient_humidity | factor(Month) , mydata)
histogram( ~ hive_temperature | factor(Month) , mydata)
histogram( ~ hive_humidity | factor(Month) , mydata)
histogram( ~ hive_weight | factor(Month) , mydata)

# calculo da class-conditional mean,SD e skewness ****************************************************************************

aggregate(ambient_temperature ~ Month , data=mydata, mean)
aggregate(ambient_humidity ~ Month , data=mydata, mean)
aggregate(hive_temperature ~ Month , data=mydata, mean)
aggregate(hive_humidity ~ Month , data=mydata, mean)
aggregate(hive_weight ~ Month , data=mydata, mean)

aggregate(ambient_temperature ~ Month , data=mydata, sd)
aggregate(ambient_humidity ~ Month , data=mydata, sd)
aggregate(hive_temperature ~ Month , data=mydata, sd)
aggregate(hive_humidity ~ Month , data=mydata, sd)
aggregate(hive_weight ~ Month , data=mydata, sd)

aggregate(ambient_temperature ~ Month , data=mydata, skewness)
aggregate(ambient_humidity ~ Month , data=mydata, skewness)
aggregate(hive_temperature ~ Month , data=mydata,skewness)
aggregate(hive_humidity ~ Month , data=mydata, skewness)
aggregate(hive_weight ~ Month , data=mydata, skewness)

#Redimensionamento dos dados
#Padronização (media 0 e desvio padrão 1)
scale(mydata_preditors)

#Normalização (em um intervalo de 0 e 1)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dfNorm <- as.data.frame(lapply(mydata_preditors, normalize))
dfNorm

#Identificando os outliers 
#Boxplot
boxplot(mydata_preditors, horizontal=TRUE)
#Pega os valores reais dos outliers com essa função abaixo e bota em um vetor
outliers <- boxplot(mydata_preditors, plot=FALSE)$out
#Descobre em quais colunas estão os outliers
mydata_preditors[which(mydata_preditors %in% outliers),]
#Remove as colunas que tem outliers(Não recomendado p datasets pequenos)
mydata_preditors <- mydata_preditors[-which(mydata_preditors %in% outliers),]
#OU
#Retira os outliers
boxplot(mydata_preditors,horizontal=TRUE,axes=FALSE,outline=FALSE)

identify(rep(1, length(mydata_preditors)), mydata_preditors, labels = seq_along(mydata_preditors))


#Usando z-escore
install.packages('outliers')
library('outliers')
outlier(mydata_preditors)
# Calculando Z score
z <- scores(mydata_preditors)

# Show number of extreme outliers using Z-score
length(z[z > 9.9])

scores(mydata_preditors, type="z", prob=0.95)  # beyond 95th %ile based on z-scores

# Remove extreme outliers 
no_outliers <- mydata_preditors[-which(z > 9.9)]

#Imputação
install.packages('Hmisc')
library('Hmisc')
impute(mydata_preditors$ptratio, mean)  # replace with mean
impute(mydata_preditors$ptratio, median)  # median
impute(mydata_preditors$ptratio, 20)  # replace specific number
#Imputação com KNN
knnOutput <- knnImputation(mydata_preditors[, !names(mydata_preditors) %in% "medv"])  # perform knn imputation.
anyNA(knnOutput)


#Ver a performance da imputação
install.packages('DMwR')
library('DMwR')
actuals <- mydata_preditors$ptratio[is.na(mydata_preditors$ptratio)]
predicteds <- rep(mean(mydata_preditors$ptratio, na.rm=T), length(actuals))
regr.eval(actuals, predicteds)



#Utilizando o spatial sign 
#primeiro necessario normalizar
install.packages('caret')
library('caret')
myTrans <- preProcess(mydata_preditors, method=c("center", "scale"))
myTrans <- as.numeric(myTrans)
myTransformed <- spatialSign(mydata_preditors)
myTransformed <- as.data.frame(myTransformed)


p1 <- xyplot(  data = mydata_preditors, main="Original")
p2 <- xyplot(myTransformed, data = myTransformed, main="After Spatial Sign")
grid.arrange(p1, p2, ncol = 2)
# package para utilizar a funcao skewness ****************************************************************************

install.packages("moments")
library(moments)

# calculo da obliquidade dos preditores ****************************************************************************
#Nenhum apresenta uma obliquidade muito alta
skewness(mydata$ambient_temperature  , na.rm = TRUE)
skewness(mydata$ambient_humidity  , na.rm = TRUE)
skewness(mydata$hive_temperature  , na.rm = TRUE)
skewness(mydata$hive_humidity  , na.rm = TRUE)
skewness(mydata$hive_weight  , na.rm = TRUE)


#scatter plot*********************************************************************************
install.packages("corrplot")

library(corrplot)

attach(mtcars)


pairs(mydata_preditors,panel = panel.smooth, col = mydata$Month,pch=16)

correlation <- cor(mydata_preditors ,  use="pairwise.complete.obs")

corrplot(correlation, order = "hclust")

#PCA*********************************************************************************
install.packages("factoextra")
library(factoextra)
pcaObject <- prcomp(na.omit(mydata_preditors[,-c(1,11)]),center = TRUE, scale. = TRUE)
plot(pcaObject)
#nesse plot vemos os que tem as duas maiores variancias 


pcaObject$rotation #para visualizar os valores
pcaObject$sdev



#######################################################################################
install.packages("ggplot2")
install.packages("ggfortify")
library(ggfortify)
library(ggplot2)
attach(mtcars)

install.packages("mlbench")
library(mlbench)


PCA <- prcomp(na.omit(mydata_preditors[,1:5]), center = TRUE,scale. = TRUE)
autoplot(PCA, data= (na.omit(mydata)), colour = 'Month', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)


