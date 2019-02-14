
#Descobrir onde está rodando o programa do R e colocar seus dados nesse mesmo diretorio
getwd()

# importando aquivo (que voce colocou no diretorio acima) 
#com os dados a serem pre-processados
mydata_preditors <- read.csv("dados_junho_julho.csv", header=TRUE)


#*****************Gráficos para a melhor vizualização dos dados**************************

#PARA VARIAVEIS QUALITATIVAS
#************************Graficos em barra***************************
#São mais recomendados para ver a distribuicao entre variaveis qualitativas

x =  mydata_preditors$temperatura.interna
barplot(table(x), xlab='Temperatura Interna', ylab='')
y =  mydata_preditors$temperatura.externa
barplot(table(y), xlab='Temperatura Externa', ylab='')

#PARA VARIAVEIS QUANTITATIVAS
#************************Graficos de dispersão***************************
#Duas variaveis quantitativas podem ser comparadas usando esse grafico.


x =  mydata_preditors$temperatura.interna
y =  mydata_preditors$temperatura.externa
plot (x, y, 
      xlab='Temperatura Interna' , ylab = 'Temperatura Externa')

#***************************Multiplos graficos de dispersao***************************
library(corrplot)
attach(mtcars)
pairs(mydata_preditors,panel = panel.smooth,pch=16)

#***************************Gráfico de densidade***************************

#Pegar a densidade para cada um dos preditores do grafico
x =  mydata_preditors$temperatura.interna
y =  mydata_preditors$temperatura.externa
plot( density(x), col="blue"  )
plot( density(y), col="blue"  )

#***************************Gráficos Quantil-Quantil***************************

#Para ver se uma variavel tem distribuição normal
# The points will lie on a straight line if data are close to being normal. 
x =  mydata_preditors$temperatura.interna
y =  mydata_preditors$temperatura.externa
qqnorm( x ) 
qqline(x )

#***************************Vizualizacao de big data**************************
##https://cran.r-project.org/web/packages/tabplot/vignettes/tabplot-vignette.html
install.packages('tabplot')
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
iplot(mydata_preditors$temperatura.interna, mydata_preditors$temperatura.externa)


