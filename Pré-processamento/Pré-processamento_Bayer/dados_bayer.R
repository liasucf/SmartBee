getwd()

# importando arqiuvo com os dados a serem pre-processados ************************************************
mydata <- read.csv("HT101_original.csv", header=TRUE)

#Uma variacao de temperatura marjoritariamente entre 20 e 30 graus celsius)
#Temperatura Media 26 graus celsius
hist(mydata$Temp.Btm.F., main="Histogram da Temperatura Exterior")


#Temperatura Media 31 graus celsius
#Praticamente nao tem temperatura em 24 a 27 graus celsius.
#Temperatura na maior parte no intervalo de 31 a 36 graus celsius
hist(mydata$Temp.Brood.F., main="Histogram da Temperatura da Ninhada")

#Maior desvio padrão
#Temperatura Media 29 graus celsius
#Praticamente nao tem temperatura em 24 a 27 graus celsius.
#Uma variacao muito grande de valores (uma frequencia alta entre 22/23 graus 
#e depois em 32/34)
hist(mydata$Temp.Hive.F., main="Histogram Temperatura da Colmeia")

#Umidade média 58.7%
#Predominantemente entre 55% a 65%
hist(mydata$BRH..., main="Histogram da Umidade da Ninhada")

#Umidade média 56.5%
#Predominantemente entre 50% a 65%
hist(mydata$HRH..., main="Histogram Umidade da Colmeia")

#Peso médio 21kg
#Entre 18 ate 27kg
hist(mydata$Weight.lbs., main="Histogram do Peso")

# calculo da media do valor de cada preditor independente da classe*******************************************


mean(mydata$Temp.Btm.F.)
mean(mydata$Temp.Brood.F.)
mean(mydata$Temp.Hive.F.)
mean(mydata$BRH...)
mean(mydata$HRH...)
mean(mydata$Weight.lbs.)

# calculo do desvio padrao de cada preditor independente das classes********************************************************************************
sd(mydata$Temp.Btm.F.)
sd(mydata$Temp.Brood.F.)
sd(mydata$Temp.Hive.F.)
sd(mydata$BRH...)
sd(mydata$HRH...)
sd(mydata$Weight.lbs.)

# package para utilizar a funcao skewness ****************************************************************************

install.packages("moments")
library(moments)
# calculo da obliquidade dos preditores ****************************************************************************

#Nenhum apresenta uma obliquidade muito alta
skewness(mydata$Temp.Btm.F.)
skewness(mydata$Temp.Brood.F.)
skewness(mydata$Temp.Hive.F.)
skewness(mydata$BRH...)
skewness(mydata$HRH...)
skewness(mydata$Weight.lbs.)

# package para calcular histogram condicionado a classe  ****************************************************************************
library(lattice)
attach(mydata)


histogram( ~ Temp.Btm.F. | factor(Month) , mydata)

#No mes 6 e 7 ela apresenta uma grande variacao de temperatura de 21/24 e depois 32/35
histogram( ~ Temp.Brood.F. | factor(Month) , mydata)

#No mes 4, todos as amostras foram na temperatura entre 21/24
#No mes 5 e 6 grande variacao de temperatura, de 21 ate 36 graus
histogram( ~ Temp.Hive.F. | factor(Month) , mydata)

histogram( ~ BRH... | factor(Month) , mydata)

#Mes 4 grande variacao de umidade de 25% ate 60%
histogram( ~ HRH... | factor(Month) , mydata)

#Variou bemno mes 9 e 6
histogram( ~ Weight.lbs. | factor(Month) , mydata)

# calculo da class-conditional mean,SD e skewness ****************************************************************************

aggregate(Temp.Btm.F. ~ Month , data=mydata, mean)
aggregate(Temp.Brood.F. ~ Month , data=mydata, mean)
aggregate(Temp.Hive.F. ~ Month , data=mydata, mean)
aggregate(BRH... ~ Month , data=mydata, mean)
aggregate(HRH... ~ Month , data=mydata, mean)
#no mes 6 peso bem baixo
aggregate(Weight.lbs. ~ Month , data=mydata, mean)

#alto desvio padrao  no mes 4
aggregate(Temp.Btm.F. ~ Month , data=mydata, sd)

#Desvio padrao MT grande no mes 7
aggregate(Temp.Brood.F. ~ Month , data=mydata, sd)
#Desvio padrao MT grande no mes 7
aggregate(Temp.Hive.F. ~ Month , data=mydata, sd)
aggregate(BRH... ~ Month , data=mydata, sd)
#Desvio padrao alto mes 4
aggregate(HRH... ~ Month , data=mydata, sd)
aggregate(Weight.lbs. ~ Month , data=mydata, sd)




aggregate(Temp.Btm.F. ~ Month , data=mydata, skewness)
#alto mes 8
aggregate(Temp.Brood.F. ~ Month , data=mydata, skewness)
#alto mes 9
aggregate(Temp.Hive.F. ~ Month , data=mydata,skewness)
#alto mes 4
aggregate(BRH... ~ Month , data=mydata, skewness)
aggregate(HRH... ~ Month , data=mydata, skewness)
aggregate(Weight.lbs. ~ Month , data=mydata, skewness)


#scatter plot*********************************************************************************
install.packages("corrplot")

library(corrplot)

attach(mtcars)

mydata_preditors <- subset( mydata, select = -c(Date,D1,D2,D3,Week, Month,Year ) )

#Correlação entre Temp.btm X Temp.Hive, Temp.Brood X Temp.Hive , 
pairs(mydata_preditors,panel = panel.smooth, col = mydata$Month,pch=16)
#matriz de correlação*****************************************************

#CORRELAÇÕES ALTAS:
#Tmp.Btm x BRH ,  Tmp.Btm x HRH , BRH X HRH,
#CORRELAÇÕES Médias:
#Tmp.Hive x BRH ,  Tmp.Hive x HRH , Weightx Temp.Brood,
#Corralacao média negativa
# Weightx BRH,  Weightx HRH 

correlation <- cor(mydata_preditors)

corrplot(correlation, order = "hclust")

#PCA*********************************************************************************
install.packages("factoextra")
library(factoextra)
pcaObject <- prcomp(mydata_preditors[,-c(1,11)],center = TRUE, scale. = TRUE)
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


PCA <- prcomp(mydata_preditors[,1:6], center = TRUE,scale. = TRUE)
autoplot(PCA, data= mydata, colour = 'Month', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)


#PCA 
#1 PC --> é composta em grande parte pelo BRH,HRH,Temp.Btm 
#a Temp.Hive e o Weight estao nas duas componentes quase metade metade
#2 PC --> é composto marjoritariamente pelo Temp.Brood 
#Juntas as duas componentes principais expressam 81.65% da variancia (entao elas representam 
# bem os preditores)
