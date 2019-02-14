
#Descobrir onde está rodando o programa do R e colocar seus dados nesse mesmo diretorio
getwd()

# importando aquivo (que voce colocou no diretorio acima) 
#com os dados a serem pre-processados
mydata_preditors <- read.csv("dados_junho_julho.csv", header=TRUE)



#Instalação de todos os pacotes

#Pacote para função skweness
install.packages("moments")
install.packages('outliers')
install.packages('Hmisc')
install.packages('DMwR')
install.packages('caret')
install.packages("mice")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("factoextra")
install.packages("mlbench")


#*************************** Sumario dos dados ***************************

summary(mydata_preditors)

#*************************** Análise Exploratória dos dados ***************************
#Histogramas
hist(mydata_preditors$temperatura.interna, main="Histogram da Temperatura Interna")
hist(mydata_preditors$temperatura.externa, main="Histogram da Temperatura Exterior")

# calculo da media do valor de cada preditor independente da classe
mean(mydata_preditors$temperatura.interna, trim = 0, na.rm = TRUE)
mean(mydata_preditors$temperatura.externa , trim = 0, na.rm = TRUE)

# calculo do desvio padrao de cada preditor independente das classes
sd(mydata_preditors$temperatura.interna , na.rm = TRUE)
sd(mydata_preditors$temperatura.externa , na.rm = TRUE)


# package para calcular histogram condicionado a classe
# library(lattice)
# attach(mydata_preditors)
# 
# 
# histogram( ~ temperatura.interna | factor(Month) , mydata_preditors)
# histogram( ~ temperatura.externa | factor(Month) , mydata_preditors)


# calculo da class-conditional mean,SD e skewness
# 
# aggregate(temperatura.interna ~ Month , data=mydata_preditors, mean)
# aggregate(temperatura.externa ~ Month , data=mydata_preditors, mean)
# 
# 
# aggregate(temperatura.interna ~ Month , data=mydata_preditors, sd)
# aggregate(temperatura.externa ~ Month , data=mydata_preditors, sd)


#library(moments)
# 
# aggregate(temperatura.interna ~ Month , data=mydata_preditors, skewness)
# aggregate(temperatura.externa ~ Month , data=mydata_preditors, skewness)




#***************************Redimensionamento dos dados***************************
#Padronização (media 0 e desvio padrão 1)
dfPad <- as.data.frame(scale(mydata_preditors))

#Normalização (em um intervalo de 0 e 1)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dfNorm <- as.data.frame(lapply(mydata_preditors, normalize))


#***************************Detectando e Retirando os outliers***************************
#Boxplot
boxplot(mydata_preditors, horizontal=TRUE)

#Para um conjunto de dados contínuos os outliers são definido por aqueles fora da faixa de valores do
#75th and 25th quartile

quantile(mydata_preditors$temperatura.interna) 
quantile(mydata_preditors$temperatura.externa) 


#Outliers de cada coluna seguindo a regra do quartile
outlier_temp_int <- boxplot.stats(mydata_preditors$temperatura.interna)$out  # outlier values.
outlier_temp_ext <- boxplot.stats(mydata_preditors$temperatura.externa)$out  # outlier values.



#Todos os outliers do dataset
outliers <- boxplot(mydata_preditors, plot=FALSE)$out

#Removendo os outliers que estão fora da faixa de 75th and 25th quartile
mydata_sem_outliers =  mydata_preditors$temperatura.interna

qnt <- quantile(mydata_sem_outliers, probs=c(.25, .75), na.rm = T)
caps <- quantile(mydata_sem_outliers, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(mydata_sem_outliers, na.rm = T)
mydata_sem_outliers[mydata_sem_outliers < (qnt[1] - H)] <- caps[1]
mydata_sem_outliers[mydata_sem_outliers > (qnt[2] + H)] <- caps[2]
#Dados sem os outliers 
mydata_sem_outliers <- as.data.frame(mydata_sem_outliers)
#Com uma distribuição muito mais homogenea
quantile(mydata_sem_outliers)

#Retira os outliers da plotagem dos bloxpot com (outline = FALSE)
boxplot(mydata_preditors,horizontal=TRUE,axes=FALSE,outline=FALSE)

#Usando z-escore
library('outliers')

#Pega a amostra mais extrema de cada coluna
outlier(mydata_preditors)
#Pega  a mais extrema do lado oposto
outlier(mydata_preditors,opposite=TRUE)

# Calculando Z score
z <- scores(mydata_preditors)
z
#Quantidade de dados que possuem um z-score maior que 3
length(z[z > 3])
#Os outliers com -3<x<3
outliers_up = mydata_preditors[z > 3.0] 
# Mostra o numero de outliers extremos usando o Z-escore
length(scores(mydata_preditors, type="z", prob=0.95))  # beyond 95th %ile based on z-score


#***************************Transformações para resolver outliers***************************
#Imputação
library('Hmisc')

#Trocando os outliers por missing values
mydata_preditors[z > 3.0] <- NA
is.na(mydata_preditors)



#Descorbir se tem  algum valor faltando (missing values), 
#se não tiver não aplica a imputação
anyNA(mydata_preditors) 

#Contagem de quantos valores estão em falta em cada preditor
sapply(mydata_preditors
       , function(x) sum(is.na(x)))

#Se tiver aplica a imputação das seguintes formas

#Escolhendo a coluna que irá sofrer a imputação
x =  mydata_preditors$hive_weight
anyNA(x) 

#Média
y = impute(x, mean)  
anyNA(y) 
#mediana
impute(x, median)  
#subsistituir por um numero especifico (ex: 20)
impute(x, 20) 
#Imputação com KNN
library('DMwR')
knnOutput <- knnImputation(mydata_preditors[, !names(mydata_preditors) %in% "medv"])  # perform knn imputation.
anyNA(knnOutput)


#Utilizando o spatial sign 
#primeiro necessario normalizar
library('caret')
myTrans <- preProcess(mydata_preditors, method=c("center", "scale"))
myTrans <- as.numeric(myTrans)
myTransformed <- spatialSign(mydata_preditors, na.rm = TRUE)
myTransformed <- as.data.frame(myTransformed, drop = FALSE)


trellis.par.set(caretTheme())
featurePlot(mydata_preditors[,-5], mydata_preditors[,5], "pairs")
featurePlot(spatialSign(scale(mydata_preditors[,-5])), mydata_preditors[,5], "pairs")




#***************************Lidando com valores em falta*************************** 

#Contagem de quantos valores estão em falta em cada preditor
sapply(mydata_preditors
       , function(x) sum(is.na(x)))

#Descartar os dados
newdata <- na.omit(mydata_preditors) 

library('mice')
#Para entender o padrão dos dados faltantes Ex:os valores da 1 coluna 
#indicam a quantidade de valores faltantes (quando uma coluna esta indicado com um zero -- ele é a coluna que os valores faltantes de referencia)
#informa a quantidade de valores que estão completos Ex:36013
md.pattern(mydata_preditors)
install.packages("VIM")
library(VIM)
#Entender os padrões de dados faltantes
aggr_plot <- aggr(mydata_preditors, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(mydata_preditors), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#Preencher com novos valores
#Imputando valores (pode usar os mesmos do outliers) 
#Usando predictive mean matching
tempData <- mice(mydata_preditors,m=5,maxit=50,meth='pmm',seed=500)
#o data set completo imputado
miceOutput <- complete(tempData)
anyNA(miceOutput)


#***************************Calculo da Obliquidade dos preditores*************************** 

# package para utilizar a funcao skewness
library(moments)

skewness(mydata_preditors$temperatura.interna  , na.rm = TRUE)
skewness(mydata_preditors$temperatura.externa  , na.rm = TRUE)


#***************************Resolvendo a Obliquidade ******************************************************  

#Box Cox 
#Yeo-Johnson 

library(MASS)

#Modelo linear
full.model <- lm(hive_temperature ~., data = mydata_preditors)
#Valores sem obliquidade
bc = boxcox(full.model, lambda = seq(-3, 3), plotit = TRUE)
#Lambda ideal escolhido pela função box cox 
(lambda <- bc$x[which.max(bc$y)])



#***************************Correlação*******************************************

library('corrplot')

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
full.model <- lm(temperatura.interna ~., data = mydata_preditors)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "backward", 
                      trace = FALSE)
step(full.model,direction="backward")
summary(step.model)
step.model$anova
anova(full.model) # anova table 


#***********************Reduzindo o número de preditores *******************************************

#PCA

library(factoextra)
library(ggfortify)
library(ggplot2)
attach(mtcars)
library(mlbench)

pcaObject <- prcomp(na.omit(mydata_preditors[,-c(1,11)]),center = TRUE, scale. = TRUE)
plot(pcaObject)
#nesse plot vemos os que tem as duas maiores variancias 

pcaObject$rotation #para visualizar os valores
pcaObject$sdev

PCA <- prcomp(na.omit(mydata_preditors[,1:2]), center = TRUE,scale. = TRUE)
autoplot(PCA, data= (na.omit(mydata_preditors)), loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)

#PLS

library(pls) 
set.seed(1)
PLS <-plsr(temperatura.interna~., data = mydata_preditors, center = TRUE,scale. = TRUE, 
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




