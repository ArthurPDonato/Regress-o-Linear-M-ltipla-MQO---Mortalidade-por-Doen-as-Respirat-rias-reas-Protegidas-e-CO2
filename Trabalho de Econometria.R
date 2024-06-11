#######   ANÁLISE DE RELAÇÃO ENTRE MORTALIDADE POR DOENÇAS RESPIRATÓRIAS, ÁREAS DE PRESERVAÇÃO
####### E EMISSÕES DE CO2 VIA REGRESSÃO LINEAR MÚLTIPLA ATRAVÉS DO MÉTODO DOS MÍNIMOS
####### QUADRADOS ORDINÁRIOS (MQO)

## Chamando os pacotes necessários
install.packages("datazoom.amazonia")
library(datazoom.amazonia)
install.packages("dplyr")
library(dplyr)
install.packages("plotly")
library(plotly)

## Baixando os dados do Índice de Progresso Social para os municípios da Amazônia Legal Brasileira
## para os anos de 2014, 2018 e 2021 
IPS_2014_2018_2021 <- load_ips(dataset = "all", raw_data = FALSE, 
                               time_period = c(2014, 2018, 2021), language = "pt")

## Verificando o nome de todas as variáveis
colnames(IPS_2014_2018_2021)

## Selecionando apenas as Variáveis Relevantes para o estudo
MDR_AP_CO2_ <- select(IPS_2014_2018_2021, ano, municipio, codigo_ibge,
                      mortalidade_por_doencas_respiratorias_obitos_100_000_habitantes, 
                      areas_protegidas_percent_area_total_do_municipio, 
                      emissoes_co2_ton_co2_habitante)

## Renomeando a coluna de mortalidade por doenças respiratórias
MDR_AP_CO2_ <- rename(MDR_AP_CO2_,
                      MDR = mortalidade_por_doencas_respiratorias_obitos_100_000_habitantes)

## Renomeando a coluna de áreas protegidas
MDR_AP_CO2_ <- rename(MDR_AP_CO2_,
                      AP = areas_protegidas_percent_area_total_do_municipio)

## Renomeando a coluna de emissões de CO2
MDR_AP_CO2_ <- rename(MDR_AP_CO2_,
                      CO2 = emissoes_co2_ton_co2_habitante)

## Chamando os pacotes necessários para exportar a base de dados
install.packages("writexl")
library(writexl)

### Exportando os dados filtrados para o excel - para guardar a base de dados
write_xlsx(MDR_AP_CO2_, path = "")

## Verificando valores ausentes na base de dados
sum(is.na(MDR_AP_CO2_))

### Estatística Descritiva das Variáveis
## Verificando a estatística descritiva da base de dados
summary(MDR_AP_CO2_)

## Criando o histograma para a mortalidade por doenças respiratórias
plot_ly(x = MDR_AP_CO2_$MDR, type = "histogram") %>%
  layout(title = "HISTOGRAMA MORTALIDADE POR DOENÇAS RESPIRATÓRIAS", 
       xaxis = list(title = "Mortalidade por Doenças Respiratórias para cada 100 mil habitantes", 
       yaxis = list(title = "Frequência")))

## Criando o histograma para as áreas de proteção 
plot_ly(x = MDR_AP_CO2_$AP, type = "histogram") %>%
  layout(title = "HISTOGRAMA ÁREAS DE PROTEÇÃO", 
     xaxis = list(title = "Áreas de Proteção percentual em relação ao total da área do município", 
     yaxis = list(title = "Frequência")))

## Criando o histograma para as emissões de CO2
plot_ly(x = MDR_AP_CO2_$CO2, type = "histogram") %>%
  layout(title = "HISTOGRAMA EMISSÕES DE CO2", 
         xaxis = list(title = "Emissões de CO2 (toneladas por habitante)", 
         yaxis = list(title = "Frequência")))

## Chamando os pacotes necessários
library(dplyr)
install.packages("car")
library(car)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)

## Criando o gráfico de linha para ver a evolução das variáveis
ggplot(MDR_AP_CO2_, aes(codigo_ibge)) +
  geom_line(aes(y = MDR), col = "red") +
  geom_line(aes(y = AP), col = "blue") +
  geom_line(aes(y = CO2), col = "green") +
labs(title = "Relação Entre as Variáveis", x = "codigo_ibge", y = "Demais Variáveis")


## Análise de Correlação das variáveis
windows()
pairs.panels(MDR_AP_CO2_)


#### REGRESSÃO
## Rodando a Regressão (CORRIGIR A TABELA ANTES DE RODAR A REGRESSÃO)
reg_MDR_AP_CO2 <- lm(MDR~ AP + CO2, data = MDR_AP_CO2_)

## Verificando o resumo da regressão
summary(reg_MDR_AP_CO2)

## Verificando os intervalos de confiança
confint(reg_MDR_AP_CO2)

## Verificando o quadro ANOVA
anova(reg_MDR_AP_CO2)

## Renomeando o resíduo da regressão
residuo <- resid(reg_MDR_AP_CO2)

## Renomeando os valores ajustados da regressão
valores_ajustados <- fitted(reg_MDR_AP_CO2)

## Analisando o Resíduo da Regressão
hist(residuo)

## Testes do MQO
install.packages("tseries")
library(tseries)
install.packages("psych")
library(psych)
library(car)
install.packages("lmtest")
library(lmtest)
install.packages("sandwiche")
library(sandwich)

## Verificando a normalidade dos resíduos
jarque.bera.test(residuo) # Resíduo não possui distribuição normal

## (I) Verificando a presença de multicolinearidade entre as variáveis explicativas
vif(reg_MDR_AP_CO2) # não há multicolinearidade no modelo!

## (II) Verificando a presença de heterocedasticidade no modelo
bptest(reg_MDR_AP_CO2) # há presença de heterocedasticidade

## (III) Verificando a autocorrelação residual 
durbinWatsonTest(reg_MDR_AP_CO2) # não há presença de autocorrelação residual

# corrigindo a heterocedasticidade
coeftest(reg_MDR_AP_CO2, vcov. = vcovHC(reg_MDR_AP_CO2))
summary(reg_MDR_AP_CO2)

## Definindo o diretório para salvar
setwd("C:/Users/TutuSurfer/Desktop/GitHub")

## Exportando os resultados da regressão para o word
library(stargazer)
stargazer(reg_MDR_AP_CO2, type="html", title="Resultados",
         dep.var.label=c("Y"), omit.stat=c("LL","ser","f"), out = "resultados reg MDR_AP_CO2.doc")
