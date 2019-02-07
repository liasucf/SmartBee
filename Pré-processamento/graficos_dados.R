
#Descobrir onde está rodando o programa do R e colocar seus dados nesse mesmo diretorio
getwd()

# importando aquivo (que voce colocou no diretorio acima) 
#com os dados a serem pre-processados
data <- read.csv("emil_1estacao.csv", header=TRUE)
mydata <- subset( data, select = c(date,Month, hive_weight,hive_temperature,hive_humidity, 
                                   ambient_temperature,ambient_humidity ))
mydata_preditors <- subset( mydata, select = c(hive_weight,hive_temperature,hive_humidity, 
                                               ambient_temperature,ambient_humidity ))


#*****************Gráficos para a melhor vizualização dos dados**************************

#PARA VARIAVEIS QUALITATIVAS
#************************Graficos em barra***************************
#São mais recomendados para ver a distribuicao entre variaveis qualitativas

x =  mydata_preditors$hive_weight
barplot(table(x), xlab='Peso da Colmeia', ylab='')

#PARA VARIAVEIS QUANTITATIVAS
#************************Graficos de dispersão***************************
#Duas variaveis quantitativas podem ser comparadas usando esse grafico.


x =  mydata_preditors$hive_weight
y = mydata_preditors$hive_temperature
plot (x, y, 
      xlab='Peso da colmeia' , ylab = 'Temperatura da colmeia')

#***************************Multiplos graficos de dispersao***************************
library(corrplot)
attach(mtcars)
pairs(mydata_preditors,panel = panel.smooth, col = mydata$Month,pch=16)

#***************************Gráfico de densidade***************************

#Pegar a densidade para cada um dos preditores do grafico
x =  mydata_preditors$hive_weight
plot( density(x), col="blue"  )

#***************************Gráficos Quantil-Quantil***************************

#Para ver se uma variavel tem distribuição normal
# The points will lie on a straight line if data are close to being normal. 
x =  mydata_preditors$hive_weight
qqnorm( x ) 
qqline(x )

#***************************Vizualizacao de big data**************************
##https://cran.r-project.org/web/packages/tabplot/vignettes/tabplot-vignette.html
install.packages('tabplot')
library('tabplot')
require(ggplot2)
tabplot::tableplot(mydata_preditors)

#***************************Graficos com o pacote Java**************************

install.packages('iplots')
install.packages('rJava')
s
library(rJava)
library(iplots)

#***************************Graficos vinculados**************************

#Gráficos vinculados são aqueles nos quais múltiplos gráficos são gerados e visualizados,
#e observações específicas são sinalizadas ou "escovadas" para chamar a atenção para elas 
#em cada um dos gráficos individuais. O pacote iplots oferece uma maneira fácil de criar
#um pequeno número de gráficos vinculados.

#M
names(mydata_preditors)
ipcp(mydata_preditors) # use arrow keys to control transparency
iplot(mydata_preditors$hive_weight, mydata_preditors$hive_temperature)


