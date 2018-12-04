getwd()

# importando arqiuvo com os dados a serem pre-processados ************************************************
data <- read.csv("arnas1_1estacao.csv", header=TRUE)
mydata <- subset( data, select = c(date,Month, hive_weight,hive_temperature,hive_humidity, 
                                               ambient_temperature,ambient_humidity ))
mydata_preditors <- subset( mydata, select = c(hive_weight,hive_temperature,hive_humidity, 
                                             ambient_temperature,ambient_humidity ))



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
aggregate(hive_humidity ~ Month , data=mydata, mean)
aggregate(hive_temperature ~ Month , data=mydata, mean)
aggregate(ambient_humidity ~ Month , data=mydata, mean)
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


