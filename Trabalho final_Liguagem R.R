#Disciplina de Linguagem R - UNESP CLP PPG-BAC
#Trabalho final 
#MARINA LEITE E MARCIO C MOTTA

#Dados de observações realizadas entre os meses de julho e setembro 2021 nas praias de Paranapuã e Itaipu, dentro dos limites do Parque Estadual Xixová-Japuí
#Espécies alvo do estudo (Thalasseus maximus, T. acuflavidus e Larus dominicanus)
#Todas as contagens foram realizadas através de ponto fixo, por 15 minutos

#PERGUNTA 1: os dados apresentam uma distribuição normal nas duas localidades, para as três espécies?
#PERGUNTA 2: há diferença nas médias das espécies por localidade?
#PERGUNTA 3: qual a espécie foi mais observada nesse período de tempo?
#PERGUNTA 4: qual o mês com maior número de observações nas duas localidades? 

#---------------------------------------------
#IMPORTANDO OS DADOS

getwd() #Verificando o diretório

#Configurando o diretório
setwd("~/Desktop/Doutorado/Disciplinas/2021_Semestre 2/R/Apresentacao_R_Dados")
getwd() #Verificando se o diretório está correto

dir() #Verificando os arquivos do diretório

#---------------------------------------------
#IMPORTANDO A PLANILHA DE DADOS
#lendo dados em CSV
dados_laridae <- read.csv(file = "Laridae_pexj_inverno2021.csv" , header=TRUE, sep = ";")

#---------------------------------------------
#AJUSTANDO OS DADOS
#olhando para os dados
View(dados_laridae)
summary(dados_laridae)
str(dados_laridae)
dim(dados_laridae)

install.packages("lubridate") #instalando o pacote responsável por tornar uma variável na classe data
library(lubridate) #carregando o pacote para tornar datas

#Arrumando as classes dos variáveis 
#data
is.Date(dados_laridae$data) #perguntando
dados_laridae$data <- ymd(dados_laridae$data) #tornando datas
is.Date(dados_laridae$data) #perguntando

#numérica
dados_laridae$contagem <- as.numeric(dados_laridae$contagem)

#conferindo as classes
str(dados_laridae)

#Transformando a variável categórica em um fator que explica 
dados_laridae$local <- as.factor(dados_laridae$local)
#conferindo as classes
str(dados_laridae)

#Listando todos os objetos
ls(dados_laridae)
summary(dados_laridae)

#transformar a variável "espécie" em fator (variávies que explicam)
dados_laridae$especie <- as.factor(dados_laridae$especie)
#verificando se deu certo
dados_laridae$especie
#Olhando para a variável "espécie" 
summary(dados_laridae$especie)
summary(dados_laridae$local)
summary(dados_laridae$contagem)

#---------------------------------------------
#FILTRANDO OS DADOS
install.packages("dplyr")
library(dplyr)

#isolando as espécies com o filtro. Assim, é possível trabalhar com espécies separadas, fora do dataframe total
maximus <- dados_laridae %>% filter(., especie == "Thalasseus maximus")
acuflavidus <- dados_laridae %>% filter(., especie == "Thalasseus acuflavidus")
dominicanus <- dados_laridae %>% filter(., especie == "Larus dominicanus")

#isolando os locais com o filtro. Assim, é possível trabalhar com locais separados, fora do dataframe total
itaipu <- dados_laridae %>% filter(., local == "itaipu")
paranapua <- dados_laridae %>% filter(., local == "paranapua")

#é possível fazer filtro das datas? (buscar mais infos sobre)

#verificando as classes e médias de cada espécie
str(maximus)
str(acuflavidus)
str(dominicanus)

#---------------------------------------------
#PERGUNTA 1: os dados apresentam uma distribuição normal nas duas localidades, para as três espécies?
#ANALISAR OS PRESSUPOSTOS DE NORMALIDADE E HOMOCEDASTICIDADE
#inspeção visual geral
tapply(dados_laridae$contagem, dados_laridae$local, mean) #média
tapply(dados_laridae$contagem, dados_laridae$local, median) #mediana

#inspeção visual por espécie e local
tapply(maximus$contagem, maximus$local, mean) #média
tapply(maximus$contagem, maximus$local, median) #mediana

tapply(acuflavidus$contagem, acuflavidus$local, mean) #média
tapply(acuflavidus$contagem, acuflavidus$local, median) #mediana

tapply(dominicanus$contagem, dominicanus$local, mean) #média
tapply(dominicanus$contagem, dominicanus$local, median) #mediana

#histograma dos dados
par(mfrow=c(2,2))
hist(dados_laridae$contagem)
hist(maximus$contagem)
hist(acuflavidus$contagem)
hist(dominicanus$contagem)
dev.off() #"desligando" o par(mfrow=c(3,1)

#-----
#Olhando os dados de outra forma e comparando com a curva normal (tracejada)
#meus dados -MAXIMUS dois locais
?density
par(mfrow=c(3,1))
plot(density(maximus$contagem),
     main="MAXIMUS",xlab="contagem",
     ylab="Densidade", col="blue",  lwd =5)
#estabelecendo a curva normal
?dnorm
curve(dnorm(x,mean(maximus$contagem),sd(maximus$contagem)), add=T, lty=2, col="red", lwd =5)
#resultado: dados distantes da curva normal (assimetria negativa)

#meus dados - ACUFLAVIDUS dois locais
plot(density(acuflavidus$contagem),
     main="ACUFLAVIDUS",xlab="contagem",
     ylab="Densidade", col="blue",  lwd =5)
#estabelecendo a curva normal
curve(dnorm(x,mean(acuflavidus$contagem),sd(acuflavidus$contagem)), add=T, lty=2, col="red", lwd =5)
#resultado: dados mais distantes da curva normal (assimetria negativa)

#meus dados - DOMINICANUS dois locais
plot(density(dominicanus$contagem),
     main="DOMINICANUS",xlab="contagem",
     ylab="Densidade", col="blue",  lwd =5)
#estabelecendo a curva normal
curve(dnorm(x,mean(dominicanus$contagem),sd(dominicanus$contagem)), add=T, lty=2, col="red", lwd =5)
#resultado: dados próximos da curva normal (simétricos)

dev.off() #"desligando" o par(mfrow=c(3,1)

#salvar o gráfico
savePlot(filename = "curvanormalESPECIES_localidade.png", type = "png")#salvar o gráfico em png

#visualizar os dados por categoria (tratamentos)
#MAXIMUS por local
par(mfrow=c(3,2))
hist(maximus$contagem[maximus$local=="itaipu"]) #simétricos 
hist(maximus$contagem[maximus$local=="paranapua"]) #assimétrica negativa

#ACUFLAVIDUS por local
hist(acuflavidus$contagem[acuflavidus$local=="itaipu"]) #assimétrica negativa
hist(acuflavidus$contagem[acuflavidus$local=="paranapua"]) #assimétrica negativa

#DOMINICANUS por local
hist(dominicanus$contagem[dominicanus$local=="itaipu"]) #assimétrica negativa
hist(dominicanus$contagem[dominicanus$local=="paranapua"]) #assimétrica negativa
#resultados deste histograma não coincidiram com o gráfico acima que tem a curva normal (VERIFICAR)
dev.off()

#Interessante de ver! 
#Olhando para os dados de contagem por área de coleta e por espécie
par(mfrow=c(3,2))
#MAXIMUS e itaipu 
plot(density(maximus$contagem[maximus$local=="itaipu"]),
     main="MAXIMUS - itaipu",xlab="contagem",
     ylab="Numero", col="blue",  lwd =5)
curve(dnorm(x,mean(maximus$contagem[maximus$local=="itaipu"]),sd(maximus$contagem[maximus$local=="itaipu"])), add=T, lty=2, col="red", lwd =5)
#dados aparentemente simétricos em relaçào à curva normal

#MAXIMUS e paranapua
plot(density(maximus$contagem[maximus$local=="paranapua"]),
     main="MAXIMUS - paranapuã",xlab="contagem",
     ylab="Numero", col="blue",  lwd =5)
curve(dnorm(x,mean(maximus$contagem[maximus$local=="paranapua"]),sd(maximus$contagem[maximus$local=="paranapua"])), add=T, lty=2, col="red", lwd =5)
#dados aparentemente assimétricos (negativo) em relaçào à curva normal

#ACUFLAVIDUS e itaipu 
plot(density(acuflavidus$contagem[acuflavidus$local=="itaipu"]),
     main="ACUFLAVIDUS - itaipu",xlab="contagem",
     ylab="Numero", col="blue",  lwd =5)
curve(dnorm(x,mean(acuflavidus$contagem[acuflavidus$local=="itaipu"]),sd(acuflavidus$contagem[acuflavidus$local=="itaipu"])), add=T, lty=2, col="red", lwd =5)
#dados aparentemente simétricos em relaçào à curva normal

#ACUFLAVIDUS e paranapua
plot(density(acuflavidus$contagem[acuflavidus$local=="paranapua"]),
     main="ACUFLAVIDUS - paranapuã",xlab="contagem",
     ylab="Numero", col="blue",  lwd =5)
curve(dnorm(x,mean(acuflavidus$contagem[acuflavidus$local=="paranapua"]),sd(acuflavidus$contagem[acuflavidus$local=="paranapua"])), add=T, lty=2, col="red", lwd =5)
#dados aparentemente assimétricos (negativo) em relaçào à curva normal

#DOMINICANUS e itaipu 
plot(density(dominicanus$contagem[dominicanus$local=="itaipu"]),
     main="DOMINICANUS - itaipu",xlab="contagem",
     ylab="Numero", col="blue",  lwd =5)
curve(dnorm(x,mean(dominicanus$contagem[dominicanus$local=="itaipu"]),sd(dominicanus$contagem[dominicanus$local=="itaipu"])), add=T, lty=2, col="red", lwd =5)
#dados aparentemente simétricos em relaçào à curva normal

#DOMINICANUS e paranapua
plot(density(dominicanus$contagem[dominicanus$local=="paranapua"]),
     main="DOMINICANUS - paranapuã",xlab="contagem",
     ylab="Numero", col="blue",  lwd =5)
curve(dnorm(x,mean(dominicanus$contagem[dominicanus$local=="paranapua"]),sd(dominicanus$contagem[dominicanus$local=="paranapua"])), add=T, lty=2, col="red", lwd =5)
#dados aparentemente simétricos em relaçào à curva normal

dev.off() #"desligando" o par(mfrow=c(2,1)

#salvar o gráfico
savePlot(filename = "curvanrormalESPECIES_localidade2.png", type = "png")#salvar o gráfico em png

#Vamos fazer uma avaliação com todas as categorias de local no mesmo plot
plot(c(-0.8,20), c(0,3), xlab="Contagem", ylab="Numero", type="n", cex.axis=1, cex.lab=1.1, main="")
lines(density(maximus$contagem[maximus$local=="paranapua"]), col="red", lwd=4)
lines(density(maximus$contagem[maximus$local=="itaipu"]), col="blue", lwd=4)
lines(density(acuflavidus$contagem[acuflavidus$local=="paranapua"]), col="green", lwd=4)
lines(density(acuflavidus$contagem[acuflavidus$local=="itaipu"]), col="purple", lwd=4)
lines(density(dominicanus$contagem[dominicanus$local=="paranapua"]), col="black", lwd=4)
lines(density(dominicanus$contagem[dominicanus$local=="itaipu"]), col="orange", lwd=4)
#dados de MAXIMUS em paranapua e ACUFLAVIDUS em paranapua bem discrepantes em relação aos demais

#colocando a legenda no gráfico
levels(dados_laridae$local)
labs <- c("par_max", "ita_max","par_acu", "ita_acu","par_dom", "ita_dom")
levels(dados_laridae$local)
legend("topright", labs, lty=c(1), col=c("red","blue","green", "purple", "black", "orange", bty="n",  lwd=4))
?legend

#salvar o gráfico
savePlot(filename = "normalidadeESPECIES_localidade.png", type = "png")#salvar o gráfico em png

#qqnorm é uma função genérica cujo método padrão produz um gráfico dos valores linearizados em y. 
#qqline adiciona uma linha a um gráfico "teórico" de distribuição normal linearizada
?qqnorm
?qqline

#Olhando para os dados abaixo de contagem por área de coleta e por espécie
par(mfrow=c(3,2))
#MAXIMUS nos dois locais
qqnorm(maximus$contagem[maximus$local=="paranapua"],main = "maximus_paranapua")
qqline(maximus$contagem[maximus$local=="paranapua"], lt=2)
#alguns dados bem distantes da distribuição normal linearizada

qqnorm(maximus$contagem[maximus$local=="itaipu"],main = "maximus_itaipu")
qqline(maximus$contagem[maximus$local=="itaipu"], lt=2)
#dados se aproximam da linha da distribuição normal linearizada

#ACUFLAVIDUS nos dois locais
qqnorm(acuflavidus$contagem[acuflavidus$local=="paranapua"],main = "acuflavidus_paranapua")
qqline(acuflavidus$contagem[acuflavidus$local=="paranapua"], lt=2)
#alguns dados bem distantes da linha da distribuição normal linearizada

qqnorm(acuflavidus$contagem[acuflavidus$local=="itaipu"],main = "acuflavidus_itaipu")
qqline(acuflavidus$contagem[acuflavidus$local=="itaipu"], lt=2)
#alguns dados distantes da linha da distribuição normal linearizada

#DOMINICANUS nos dois locais
qqnorm(dominicanus$contagem[dominicanus$local=="paranapua"], main = "dominicanus_paranapua")
qqline(dominicanus$contagem[dominicanus$local=="paranapua"], lt=2)
#um dado distante da linha da distribuição normal linearizada

qqnorm(dominicanus$contagem[dominicanus$local=="itaipu"], main = "dominicanus_itaipu")
qqline(dominicanus$contagem[dominicanus$local=="itaipu"], lt=2)
#alguns dados distantes da linha da distribuição normal linearizada

dev.off() #"desligando" o par(mfrow=c(3,2)

#salvar o gráfico
savePlot(filename = "normalidadeESPECIES_localidade.png", type = "png")#salvar o gráfico em png

#ANÁLISE DA NORMALIDADE
#Por meio de um teste:
#Hipótese nula = os dados NÃO SÃO DIFERENTES DA DISTRIBUIÇÃO NORMAL (modo correto de dizer). 
#Hipótese nula = Seguem a distribuição normal (equivocado falar desta forma)
#Queremos ver o valor de p alto, pois nõo queremos rejeitar a hipótese nula
#Quando os dados são normais = o certo seria falar = eu não tenho evidências que os dados sejam diferentes da distribuição normal
#Isso é uma inversão da lógica dos testes estatísticos
#Usar o teste de normalidade seria uma distorsão do que o teste estatístico faz

#Teste de normalidade (Shapiro Wilk)
#maximus
shapiro.test(maximus$contagem[maximus$local=="itaipu"]) #resultados: W = 0.96577, p-value = 0.8619 (maior que 0,05: dados simétricos)
shapiro.test(maximus$contagem[maximus$local=="paranapua"]) #resultados: W = 0.59412, p-value = 9.586e-05 (menor que 0,05: dados assimétricos)
shapiro.test(maximus$contagem) #resultado: W = 0.83596, p-value = 0.001213 (menor que 0,05: dados assimétricos)

#acuflavidus
shapiro.test(acuflavidus$contagem[acuflavidus$local=="itaipu"]) #resultados: W = 0.86789, p-value = 0.06146 (maior que 0,05: dados simétricos)
shapiro.test(acuflavidus$contagem[acuflavidus$local=="paranapua"]) # resultados:W = 0.50433, p-value = 1.902e-05 (menor que 0,05: dados assimétricos)
shapiro.test(acuflavidus$contagem) #W = 0.66603, p-value = 3.698e-06 (menor que 0,05: dados assimétricos)

#dominicanus
shapiro.test(dominicanus$contagem[dominicanus$local=="itaipu"]) #resultados: W = 0.82575, p-value = 0.01867 (menor que 0,05: dados assimétricos)
shapiro.test(dominicanus$contagem[dominicanus$local=="paranapua"]) #resultados: W = 0.89264, p-value = 0.1275 (maior que 0,05: dados simétricos)
shapiro.test(dominicanus$contagem) #resultados: W = 0.88613, p-value = 0.01107 (menor que 0,05: dados assimétricos)

#HOMOGENEIDADE DE VARIANCIAS  
#calculando as variancias das variáveis - MAXIMUS
var_max_itaip <- var(maximus$contagem[maximus$local=="itaipu"])
var_max_paran <- var(maximus$contagem[maximus$local=="paranapua"])

#calculando as variancias das variáveis - ACUFLAVIDUS
var_acu_itaip <- var(acuflavidus$contagem[acuflavidus$local=="itaipu"])
var_acu_paran <- var(acuflavidus$contagem[acuflavidus$local=="paranapua"])

#calculando as variancias das variáveis - DOMINICANUS
var_dom_itaip <- var(dominicanus$contagem[dominicanus$local=="itaipu"])
var_dom_paran <- var(dominicanus$contagem[dominicanus$local=="paranapua"])

#criando um objeto contendo todas as variâncias dos pontos de coleta
var_max <- tapply(maximus$contagem, maximus$local, var) #MAXIMUS
var_acu <- tapply(acuflavidus$contagem, acuflavidus$local, var) #ACUFLAVIDUS
var_dom <- tapply(dominicanus$contagem, dominicanus$local, var) #DOMINICANUS

#Realização dos testes para saber se as amostras possuem variâncias iguais (H0) ou diferentes (H1)
#teste de F sobre APENAS DUAS amostras
?var.test
var.test(maximus$contagem[maximus$local=="itaipu"], maximus$contagem[maximus$local=="paranapua"])
#F = 11.997, num df = 11, denom df = 11, p-value = 0.0002664
#resultado da estatística alta, mostrando que os dados são heterogêneos

var.test(acuflavidus$contagem[acuflavidus$local=="itaipu"], acuflavidus$contagem[acuflavidus$local=="paranapua"])
#F = 588.24, num df = 11, denom df = 11, p-value = 2.776e-13
#resultado da estatística alta, mostrando que os dados são heterogêneos

var.test(dominicanus$contagem[dominicanus$local=="itaipu"], dominicanus$contagem[dominicanus$local=="paranapua"])
#F = 0.74659, num df = 11, denom df = 11, p-value = 0.6363
#resultado da estatística baixa, mostrando que os dados são homogêneos (?)

#---------------------------------------------
#PERGUNTA 2: há diferença nas médias das espécies por localidade?

#Exploração dos dados, estatística descritiva e teste T
#GERAL (distribuição norma Z, criando os objetos de cada um deles)
#para MAXIMUS em itaipu
desvio.padrão_max_ita <- sd(maximus$contagem[maximus$local=="itaipu"])
erro.padrão_max_ita <- desvio.padrão_max_ita/sqrt(length(maximus$contagem[maximus$local=="itaipu"]))
média_max_ita <- mean(maximus$contagem[maximus$local=="itaipu"])

#para MAXIMUS em paranapuã
desvio.padrão_max_par <- sd(maximus$contagem[maximus$local=="paranapua"]) 
erro.padrão_max_par <- desvio.padrão_max_par/sqrt(length(maximus$contagem[maximus$local=="paranapua"]))
média_max_par <- mean(maximus$contagem[maximus$local=="paranapua"])

#dados para as duas localidades - MAXIMUS
desvio.padrão_max <- sd(maximus$contagem) 
erro.padrão_max <- desvio.padrão_max/sqrt(length(maximus$contagem))
média_max <- mean(maximus$contagem)

# IC (95%) duas localidades
IC.contagem.max <- qnorm(0.975)*erro.padrão_max

summary(maximus)

#para ACUFLAVIDUS em itaipu
desvio.padrão_acu_ita <- sd(acuflavidus$contagem[acuflavidus$local=="itaipu"])
erro.padrão_acu_ita <- desvio.padrão_acu_ita/sqrt(length(acuflavidus$contagem[acuflavidus$local=="itaipu"]))
média_acu_ita <- mean(acuflavidus$contagem[acuflavidus$local=="itaipu"])

#para ACUFLAVIDUS em paranapuã
desvio.padrão_acu_par <- sd(acuflavidus$contagem[acuflavidus$local=="paranapua"]) 
erro.padrão_acu_par <- desvio.padrão_acu_par/sqrt(length(acuflavidus$contagem[acuflavidus$local=="paranapua"]))
média_acu_par <- mean(acuflavidus$contagem[acuflavidus$local=="paranapua"])

#dados para as duas localidades - ACUFLAVIDUS
desvio.padrão_acu <- sd(acuflavidus$contagem) 
erro.padrão_acu <- desvio.padrão_acu/sqrt(length(acuflavidus$contagem))
média_acu <- mean(acuflavidus$contagem)

# IC (95%) - duas localidades
IC.contagem.acu <- qnorm(0.975)*erro.padrão_acu

summary(acuflavidus)

#para DOMINICANUS em itaipu
desvio.padrão_dom_ita <- sd(dominicanus$contagem[dominicanus$local=="itaipu"])
erro.padrão_dom_ita <- desvio.padrão_dom_ita/sqrt(length(dominicanus$contagem[dominicanus$local=="itaipu"]))
média_dom_ita <- mean(dominicanus$contagem[dominicanus$local=="itaipu"])

#para DOMINICANUS em paranapuã
desvio.padrão_dom_par <- sd(dominicanus$contagem[dominicanus$local=="paranapua"]) 
erro.padrão_dom_par <- desvio.padrão_dom_par/sqrt(length(dominicanus$contagem[dominicanus$local=="paranapua"]))
média_dom_par <- mean(dominicanus$contagem[dominicanus$local=="paranapua"])

#dados para as duas localidades - DOMINICANUS
desvio.padrão_dom <- sd(dominicanus$contagem) 
erro.padrão_dom <- desvio.padrão_dom/sqrt(length(dominicanus$contagem))
média_dom <- mean(dominicanus$contagem)

# IC (95%) - duas localidades
IC.contagem.dom <- qnorm(0.975)*erro.padrão_dom

summary(dominicanus)

# *------

#Distribuição normal T
#Estatísticamente mais correto, pois utilizamos os graus de liberdade
#neste caso utilizamos os graus de liberdade na fórmula
#usamos 2 graus de liberdade no teste T, pois duas médias são calculadas (2 variáveis)
#ou seja, n-2 (55 observações menos 2: df=53)
qt(0.975, df=53)*erro.padrão_max

#O valor exato do intervalo de confiança a 95% de confiabilidade estatística segue-se a fórmula abaixo
ICparaMais <- média + qnorm(0.975) * (desvio.padrão/sqrt(53))
ICparaMenos <- média - 1.959964 * (desvio.padrão/sqrt(53))
ICparaMais - ICparaMenos

#Teste T e Mann-Whitney
#Como a uma das localidades de todas as espécies tinham dados assimétricos, optei por fazer MANN-Whitney para todas as espécies

#Teste MANN-WHITNEY para os dados que não passaram pelos pressupostos de normalidade
#teste DOMINICANUS para as duas localidades
wilcox.test(dominicanus$contagem[dominicanus$local=="paranapua"], dominicanus$contagem[dominicanus$local=="itaipu"])
#resultado do teste para DOMINICANUS: W = 107, p-value = 0.04421
#leitura do teste: p é menor do que o alfa (0,05), rejeita H0, ou seja, a diferença entre as medianas é significativa

#teste ACUFLAVIDUS para as duas localidades
wilcox.test(acuflavidus$contagem[acuflavidus$local=="paranapua"], acuflavidus$contagem[acuflavidus$local=="itaipu"])
#resultado do teste: W = 15, p-value = 0.0006031
#leitura do teste: p é menor do que o alfa (0,05), rejeita H0, ou seja, a diferença entre as medianas é significativa

#teste MAXIMUS para as duas localidades
wilcox.test(maximus$contagem[maximus$local=="paranapua"], maximus$contagem[maximus$local=="itaipu"])
#resultado do teste:W = 8.5, p-value = 0.0001825
#leitura do teste: p é menor do que o alfa (0,05), rejeita H0, ou seja, a diferença entre as medianas é significativa

#---------------------------------------------
#PERGUNTA 3: qual a espécie foi mais observada nesse período de tempo?
sum(maximus$contagem) #134
sum(acuflavidus$contagem) #876 - mais abundante (contado as duas localidades)
sum(dominicanus$contagem) #124 - 

#espécie MAXIMUS por localidade e total
sum(maximus$contagem[maximus$local=="paranapua"]) #9
sum(maximus$contagem[maximus$local=="itaipu"]) #125
contagem_max<-sum(maximus$contagem[maximus$local=="paranapua"],maximus$contagem[maximus$local=="itaipu"])#134
#espécie ACUFLAVIDUS por localidade e total
sum(acuflavidus$contagem[acuflavidus$local=="paranapua"]) #15
sum(acuflavidus$contagem[acuflavidus$local=="itaipu"]) #861
contagem_acu<-sum(acuflavidus$contagem[acuflavidus$local=="paranapua"],acuflavidus$contagem[acuflavidus$local=="itaipu"])#876
#espécie DOMINUCANUS por localidade e total
sum(dominicanus$contagem[dominicanus$local=="paranapua"]) #80
sum(dominicanus$contagem[dominicanus$local=="itaipu"]) #44
contagem_dom<-sum(dominicanus$contagem[dominicanus$local=="paranapua"],dominicanus$contagem[dominicanus$local=="itaipu"])#124
#todas as espécies
sum(dados_laridae$contagem)#total 1134

#voltando nas médias por espécie e localidade
tapply(maximus$contagem, maximus$local, mean) #média (ita:10.41667; par: 0.75), mais em itaipu
tapply(acuflavidus$contagem, acuflavidus$local, mean) #média (ita:71.75; par: 1.2), mais em itaipu
tapply(dominicanus$contagem, dominicanus$local, mean) #média (ita:3.66667; par: 6.66667), mais em paranapuã

media_max<-mean(maximus$contagem) #5.583333
media_acu<-mean(acuflavidus$contagem) #36.5
media_dom<-mean(dominicanus$contagem) #5.16667

#testando o argumento "trim" para eliminar 10% dos maiores e dos menores valores
#média
media_max<-mean(maximus$contagem, trim=0.1)#4.9
media_acu<-mean(acuflavidus$contagem, trim=0.1) #23.6
media_dom<-mean(dominicanus$contagem, trim=0.1) #4.6

#há diferença entre as médias de MAXIMUS e DOMINICANUS?
wilcox.test(maximus$contagem, dominicanus$contagem)
#resultado do teste: W = 275, p-value = 0.7945
#leitura: p maior que alfa (0,05), a diferença não é significativa

#há diferença entre as médias de MAXIMUS e ACUFLAVIDUS?
wilcox.test(maximus$contagem, acuflavidus$contagem)
#resultado do teste: W = 250, p-value = 0.422
#leitura: p maior que alfa (0,05), a diferença não é significativa

#há diferença entre as médias de ACUFLAVIDUS e DOMINICANUS?
wilcox.test(dominicanus$contagem, acuflavidus$contagem)
#resultado do teste: W = 291.5, p-value = 0.95
#leitura: p maior que alfa (0,05), a diferença não é significativa

#valores mínimos e máximos das espécies nas localidades
#MAXIMUS
max(maximus$contagem[maximus$local=="itaipu"])
min(maximus$contagem[maximus$local=="itaipu"])
max(maximus$contagem[maximus$local=="paranapua"])
min(maximus$contagem[maximus$local=="paranapua"])

#ACUFLAVIDUS
max(acuflavidus$contagem[acuflavidus$local=="itaipu"])
min(acuflavidus$contagem[acuflavidus$local=="itaipu"])
max(acuflavidus$contagem[acuflavidus$local=="paranapua"])
min(acuflavidus$contagem[acuflavidus$local=="paranapua"])

#DOMINICANUS
max(dominicanus$contagem[dominicanus$local=="itaipu"])
min(dominicanus$contagem[dominicanus$local=="itaipu"])
max(dominicanus$contagem[dominicanus$local=="paranapua"])
min(dominicanus$contagem[dominicanus$local=="paranapua"])

#-------------------------

#PERGUNTA 4: qual o mês com maior número de observações nas duas localidades? 
#Teste com gráfico do tipo plot espécie x mês
par(mfrow=c(3,1),mar = c(4, 4, 1, 1), family = "serif",cex.axis = 1,
    cex.lab=1.1, las=1, tcl=0.3,bty="u") 
#formatação dos parâmetros globais do conjunto de gráficos antes de contruir os gráficos em si
#MAXIMUS
plot(contagem~data, data=maximus, xlab="meses de observação", ylab="abundância",
     pch = 15, 
     cex = 1.0, 
     col = "red") 
legend("topright", labs, lty=c(1), legend = "maximus", 
       col = "red", 
       pch = 15, bty = "n")

#ACUFLAVIDUS
plot(contagem~data, data=acuflavidus, xlab="meses de observação", ylab="abundância",
     pch = 15, 
     cex = 1.0, 
     col = "blue")
legend("topright", labs, lty=c(1), legend = "acuflavidus", 
       col = "blue", 
       pch = 15, bty = "n")

#MDOMINICANUS
plot(contagem~data, data=dominicanus, xlab="meses de observação", ylab="abundância",
     pch = 15, 
     cex = 1.0, 
     col = "green")
legend("topright", labs, lty=c(1), legend = "dominicanus", 
       col = "green", 
       pch = 15, bty = "n")
dev.off()  #"desligando" o par(mfrow=c(3,1)

#salvar o gráfico
savePlot(filename = "plotESPECIES_localidade.png", type = "png") #salvar o gráfico em png


#Criação dos gráficos BLOXPOT
par(mfrow=c(3,1),mar = c(4, 4, 1, 1), family = "serif",cex.axis = 1,
    cex.lab=1.2, las=1, tcl=0.3,bty="u") 

#MAXIMUS
boxplot(contagem ~ local, data=maximus, 
         xlab="maximus",
         ylab="contagem",
         col = "gray90")
#ACUFLAVIDUS
boxplot(contagem ~ local, data=acuflavidus,
        xlab="acuflavidus",
        ylab="contagem", 
        col = "gray90")
#DOMINICANUS
boxplot( contagem ~ local, data=dominicanus, 
         xlab="dominicanus",
         ylab="contagem",
         col = "gray90")
dev.off()  #"desligando" o par(mfrow=c(3,1)

#salvar o gráfico
savePlot(filename = "bloxpotESPECIES_localidade.png", type = "png")#salvar o gráfico em png

#histogramas para ver diferenças entre os locais de amostragem por espécie
#MAXIMUS por localidade
par(mfrow=c(3,2),mar = c(4, 4, 1, 1), family = "serif",cex.axis = 1,
    cex.lab=1,  las=1,tcl=0.3)   
hist(maximus$contagem[maximus$local=="itaipu"],
     main="MAXIMUS",
     xlab="itaipu",
     ylab="contagem",)  
hist(maximus$contagem[maximus$local=="paranapua"],
     main=NULL,
     xlab="paranapua",
     ylab="contagem")
dev.off()  #"desligando" o par(mfrow=c(3,1)

#salvar o gráfico
savePlot(filename = "histMAXIMUS_localidade.png", type = "png")#salvar o gráfico em png

#ACUFLAVIDUS por localidade
par(mfrow=c(2,1),mar = c(4, 4, 1, 1), family = "serif",cex.axis = 1,
    cex.lab=1,  las=1,tcl=0.3)   
hist(acuflavidus$contagem[acuflavidus$local=="itaipu"],
     main="ACUFLAVIDUS",
     xlab="itaipu",
     ylab="contagem",)  
hist(acuflavidus$contagem[acuflavidus$local=="paranapua"],
     main=NULL,
     xlab="paranapua",
     ylab="contagem")
dev.off()  #"desligando" o par(mfrow=c(3,1)

#salvar o gráfico
savePlot(filename = "histACUFLAVIDUS_localidade.png", type = "png")#salvar o gráfico em png


#DOMINICANUS por localidade
par(mfrow=c(2,1),mar = c(4, 4, 1, 1), family = "serif",cex.axis = 1,
    cex.lab=1,  las=1,tcl=0.3)   
hist(dominicanus$contagem[dominicanus$local=="itaipu"],
     main="DOMINICANUS",
     xlab="itaipu",
     ylab="contagem",)  
hist(dominicanus$contagem[dominicanus$local=="paranapua"],
     main=NULL,
     xlab="paranapua",
     ylab="contagem")
dev.off()  #"desligando" o par(mfrow=c(3,1)

#salvar o gráfico
savePlot(filename = "histDOMINICANUS_localidade.png", type = "png")#salvar o gráfico em png

#*------FIM DO SCRIPT!!!! <3

