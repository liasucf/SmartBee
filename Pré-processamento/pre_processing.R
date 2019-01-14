#Descobrir onde esta o programa do R
getwd()

# importando arqiuvo com os dados a serem pre-processados
data <- read.csv("emil_1estacao.csv", header=TRUE)
mydata <- subset( data, select = c(date,Month, hive_weight,hive_temperature,hive_humidity, 
                                   ambient_temperature,ambient_humidity ))
mydata_preditors <- subset( mydata, select = c(hive_weight,hive_temperature,hive_humidity, 
                                               ambient_temperature,ambient_humidity ))



#*************************** Análise Exploratória dos dados ***************************
#Histogramas
hist(mydata$ambient_temperature, main="Histogram da Temperatura Exterior")
hist(mydata$ambient_humidity, main="Histogram da Umidade Exterior")
hist(mydata$hive_temperature, main="Histogram Temperatura da Colmeia")
hist(mydata$hive_humidity, main="Histogram Umidade da Colmeia")
hist(mydata$hive_weight, main="Histogram do Peso da Colmeia")
# calculo da media do valor de cada preditor independente da classe
mean(mydata$ambient_temperature, trim = 0, na.rm = TRUE)
mean(mydata$ambient_humidity , trim = 0, na.rm = TRUE)
mean(mydata$hive_temperature , trim = 0, na.rm = TRUE)
mean(mydata$hive_humidity , trim = 0, na.rm = TRUE)
mean(mydata$hive_weight , trim = 0, na.rm = TRUE)
# calculo do desvio padrao de cada preditor independente das classes
sd(mydata$ambient_temperature , na.rm = TRUE)
sd(mydata$ambient_humidity , na.rm = TRUE)
sd(mydata$hive_temperature , na.rm = TRUE)
sd(mydata$hive_humidity , na.rm = TRUE)
sd(mydata$hive_weight , na.rm = TRUE)


# package para calcular histogram condicionado a classe
library(lattice)
attach(mydata)
histogram( ~ ambient_temperature | factor(Month) , mydata)
histogram( ~ ambient_humidity | factor(Month) , mydata)
histogram( ~ hive_temperature | factor(Month) , mydata)
histogram( ~ hive_humidity | factor(Month) , mydata)
histogram( ~ hive_weight | factor(Month) , mydata)

# calculo da class-conditional mean,SD e skewness

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

#***************************Redimensionamento dos dados***************************
#Padronização (media 0 e desvio padrão 1)
scale(mydata_preditors)

#Normalização (em um intervalo de 0 e 1)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dfNorm <- as.data.frame(lapply(mydata_preditors, normalize))
dfNorm

#***************************Detectando e Retirando os outliers***************************
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
# Mostra o numero de outliers extremos usando o Z-escore
length(z[z > 9.9])
scores(mydata_preditors, type="z", prob=0.95)  # beyond 95th %ile based on z-scores

# Removendo outliers extremos
no_outliers <- mydata_preditors[-which(z > 9.9)]


#***************************Transformações para resolver outliers***************************
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



#***************************Lidando com valores em falta*************************** 

#Contagem de quantos valores estão em falta em cada preditor
sapply(mydata_preditors
       , function(x) sum(is.na(x)))

#Descartar os dados
newdata <- na.omit(mydata_preditors) 

install.packages("mice")
library('mice')

#Preencher com novos valores
#Imputando valores (pode usar os mesmos do outliers) 
#Usando predictive mean matching
#https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
tempData <- mice(mydata_preditors,m=5,maxit=50,meth='pmm',seed=500)
miceOutput <- complete(tempData)
anyNA(miceOutput)

#Lets compute the accuracy of ptratio.

actuals <- is.na(mydata_preditors$ptratio)
predicteds <- miceOutput[is.na(mydata_preditors$ptratio), "ptratio"]
regr.eval(actuals, predicteds)


# package para utilizar a funcao skewness

#***************************Calculo da Obliquidade dos preditores*************************** 
install.packages("moments")
library(moments)

skewness(mydata$ambient_temperature  , na.rm = TRUE)
skewness(mydata$ambient_humidity  , na.rm = TRUE)
skewness(mydata$hive_temperature  , na.rm = TRUE)
skewness(mydata$hive_humidity  , na.rm = TRUE)
skewness(mydata$hive_weight  , na.rm = TRUE)

#***************************Resolvendo a Obliquidade ******************************************************  

#Box Cox 
#Yeo-Johnson 

library(MASS)

full.model <- lm(hive_temperature ~., data = mydata_preditors)
bc = boxcox(full.model, lambda = seq(-3, 3), plotit = TRUE)
#Pega o lambda ideal
(lambda <- bc$x[which.max(bc$y)])


#***************************Scatter plot***************************
install.packages("corrplot")
library(corrplot)
attach(mtcars)
pairs(mydata_preditors,panel = panel.smooth, col = mydata$Month,pch=16)

#***************************Correlação*******************************************

correlation <- cor(mydata_preditors ,  use="pairwise.complete.obs")
#Matriz de correlação
corrplot(correlation, order = "hclust")

#***********************Resolvendo problemas de alta correlação*******************************************

#***********************Removendo os preditores*******************************************
#Carregando as bibliotecas necessarias 
library(tidyverse)
library(caret)
library(leaps)
require(MASS)

#Backward selection


#a saída mostra as alternativas para reduzir o AIC
#a primeira linha em qualquer etapa é sua melhor opção. 
#quando a melhor alternativa é <none>, isso significa não fazer nada, 
#o procedimento para e dá os mesmos resultados da seleção para trás.

# Fit the full model 
full.model <- lm(hive_temperature ~., data = mydata_preditors)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "backward", 
                      trace = FALSE)
step(full.model,direction="backward")
summary(step.model)
step.model$anova
anova(full.model) # anova table 


#***********************Reduzindo o número de preditores *******************************************

#PCA
install.packages("factoextra")
library(factoextra)
install.packages("ggplot2")
install.packages("ggfortify")
library(ggfortify)
library(ggplot2)
attach(mtcars)

install.packages("mlbench")
library(mlbench)

pcaObject <- prcomp(na.omit(mydata_preditors[,-c(1,11)]),center = TRUE, scale. = TRUE)
plot(pcaObject)
#nesse plot vemos os que tem as duas maiores variancias 

pcaObject$rotation #para visualizar os valores
pcaObject$sdev

PCA <- prcomp(na.omit(mydata_preditors[,1:5]), center = TRUE,scale. = TRUE)
autoplot(PCA, data= (na.omit(mydata)), colour = 'Month', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)

#PLS

library(pls) 
set.seed(1)
PLS <-plsr(hive_temperature~., data = mydata_preditors, center = TRUE,scale. = TRUE, 
           validation = 'CV')
summary(PLS)
validationplot(PLS)


plot(PLS, ncomp = 1, asp = 1, line = TRUE, col=c("red", "blue", "green", 'black', 'orange'))
plot(PLS, plottype = "scores", comps = 1:2,col=c("red", "blue", "green", 'black', 'orange'))



#***********************************ANOVA***************************************
#Para testar a igualdade entre grupos
#Definimos 3 grupos 

Group1 <- c(2,3,7,2,6)
Group2 <- c(10,8,7,5,10)
Group3 <- c(10,13,14,13,15)
#Combina os grupos 
Combined_Groups <- data.frame(cbind(Group1, Group2, Group3)) # combines the data into a single data set.
Combined_Groups # shows spreadsheet like results
summary(Combined_Groups)  # min, median, mean, max
#Coloca eles em uma pilha
Stacked_Groups <- stack(Combined_Groups)
Stacked_Groups #shows the table Stacked_Groups

#Ver a tabela anova dos grupos
Anova_Results <- aov(values ~ ind, data = Stacked_Groups) 
summary(Anova_Results) # shows Anova_Results

#A diferença entre as medias é significamente diferente pois o p-value é menor do que o
#nivel de significancia 0.05


#A função anova serve para comparar dois modelos lineares
# modelo nulo, com apenas o intercepto
ajuste_lm_nulo <- lm(mpg ~ 1, data = mtcars)
# modelo com wt e cyl
ajuste_lm2 <- lm(mpg ~ wt + factor(cyl), data = mtcars)
# compara o modelo com wt com o modelo nulo
anova(ajuste_lm_nulo, ajuste_lm2)


#As the p-value is less than the significance level 0.05, we can conclude that there
#are significant differences between the groups highlighted with “*" in the model summary.


#As the ANOVA test is significant, we can compute Tukey HSD
#(Tukey Honest Significant Differences, R function:TukeyHSD()) for performing multiple
#pairwise-comparison between the means of groups.

