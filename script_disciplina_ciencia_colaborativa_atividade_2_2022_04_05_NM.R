### Disciplina Ciencia Colaborativa ###
## Script atividade 2 ##
#Natalia Melo - 04-05-2022


#Tratando os dados

library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)

#Importar dados
data_NM <- read.csv("atividade1_NATALIA-MELO.csv", sep=";")
data_AC <- read.csv("atividade1_ANACLARA.csv")
data_LE <- read.csv("atividade1_LETICIA-EVANGELISTA.csv", sep=";")
data_LV <- read.csv("atividade1_LUIZA-VIEIRA.csv", sep=";")
data_MF <- read.csv("atividade1_MARIANA-FAITANIN.csv", sep=";")
data_MM <- read.csv("atividade1_MARINA-MEGA.csv", sep=";")
data_VL <- read.csv("atividade1_VICTOR-LUPINACCI.csv", sep=";")


## ANA CLARA DATA ##

#Mudar sequencia
data_AC <- data_AC[ ,c(1,4,5,6,7,2,3,8,9,10,11)]

#Renomear colunas
names(data_AC) <- c('sample_pdf', 'site', 'latitude', 'longitude', 'date', 'sample_card', 
                       'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')
#site
data_AC$site <- sub("Site1", "site_1", data_AC$site)
data_AC$site <- sub("Site2", "site_2", data_AC$site)
data_AC$site <- sub("Site3", "site_3", data_AC$site)

#date
data_AC$date <- sub("1929-12-01", "01-12-1929", data_AC$date)
data_AC$date <- sub("1929-12-02", "02-12-1929", data_AC$date)

## LETICIA EVAGELISTA DATA ##
#Mudar sequencia
data_LE <- data_LE[ ,c(1,8,9,10,11,2,3,4,5,6,7)]

#Renomear colunas
names(data_LE) <- c('sample_pdf', 'site', 'latitude', 'longitude', 'date', 'sample_card', 
                    'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')
#site
data_LE$site <- sub("Site1", "site_1", data_LE$site)
data_LE$site <- sub("Site2", "site_2", data_LE$site)
data_LE$site <- sub("Site3", "site_3", data_LE$site)

#date
data_LE$date <- sub("1929-12-01", "01-12-1929", data_LE$date)
data_LE$date <- sub("1930-02-13", "13-02-1930", data_LE$date)

## LUIZA VIEIRA DATA ##
#Mudar sequencia
data_LV <- data_LV[ ,c(1,4,5,6,7,2,3,8,9,10,11)]

#Renomear colunas
names(data_LV) <- c('sample_pdf', 'site', 'latitude', 'longitude', 'date', 'sample_card', 
                    'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')
#site
data_LV$site <- sub("Site1", "site_1", data_LV$site)
data_LV$site <- sub("Site2", "site_2", data_LV$site)
data_LV$site <- sub("Site3", "site_3", data_LV$site)

#specie
data_LV$specie <- sub("_", " ", data_LV$specie)

#date
data_LV$date <- sub("/", "-", data_LV$date)
data_LV$date <- sub("/", "-", data_LV$date)

## MARIANA FAITANIN DATA ##

#Unindo colunas (dia, mes, ano) em uma so?
data_MF$date <- paste(data_MF$Dia,data_MF$Mês, data_MF$Ano, sep = "-")

#Removendo colunas
print(data_MF[, 3:5] <- list(NULL))
print(data_MF[,4:5 ] <- list(NULL))

#Mudar sequ?ncia
data_MF <- data_MF[ ,c(1,3,5,6,11,2,4,7,8,9,10)]

#Renomear colunas
names(data_MF) <- c('sample_pdf', 'site', 'latitude', 'longitude', 'date', 'sample_card', 
                    'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')
#site
data_MF$site <- sub("Site1", "site_1", data_MF$site)
data_MF$site <- sub("Site2", "site_2", data_MF$site)
data_MF$site <- sub("Site3", "site_3", data_MF$site)

#specie
data_LV$specie <- sub("_", " ", data_LV$specie)

## MARINA MEGA DATA ##
#Mudar sequencia
data_MM <- data_MM[ ,c(2,3,4,5,6,1,7,8,9,10,11)]

#Renomear colunas
names(data_MM) <- c('sample_pdf', 'site', 'latitude', 'longitude', 'date', 'sample_card', 
                    'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')
#site
data_MM$site <- sub("1", "site_1", data_MM$site)
data_MM$site <- sub("2", "site_2", data_MM$site)
data_MM$site <- sub("3", "site_3", data_MM$site)

#sample_pdf
data_MM$sample_pdf <- sub("a", "A", data_MM$sample_pdf)
data_MM$sample_card <- sub("a", "A", data_MM$sample_card)

#date
data_MM$date <- sub("/", "-", data_MM$date)
data_MM$date <- sub("/", "-", data_MM$date)


## VICTOR LUPINACCI DATA ##

#removendo NA's
data_VL <- na.omit(data_VL)

#Unindo colunas (dia, mes, ano) em uma s?
data_VL$date <- paste(data_VL$Dia,data_VL$Mes, data_VL$Ano, sep = "-")

#Removendo colunas
print(data_VL[, 7:9] <- list(NULL))

#Mudar sequencia
data_VL <- data_VL[ ,c(1,4,5,6,11,2,3,7,8,9,10)]

#Renomear colunas
names(data_VL) <- c('sample_pdf', 'site', 'latitude', 'longitude', 'date', 'sample_card', 
                    'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')
#site
data_VL$site <- sub("1", "site_1", data_VL$site)
data_VL$site <- sub("2", "site_2", data_VL$site)
data_VL$site <- sub("3", "site_3", data_VL$site)

#specie
data_VL$specie <- sub("_", " ", data_VL$specie)


## NATALIA MELO DATA ##

#species
data_NM$specie <- sub("_", " ", data_NM$specie)
data_NM$specie <- sub("iris", "Iris", data_NM$specie)
data_NM$date <- sub("1929_12_01", "01-12-1929", data_NM$date)
data_NM$date <- sub("1930_02_13", "13-02-1930", data_NM$date)

names(data_NM) <- c('sample_pdf', 'site', 'latitude', 'longitude', 'date', 'sample_card', 
                    'specie', 'sepal_lenght_cm', 'sepal_width_cm', 'petal_lenght_cm', 'petal_width_cm')

### Juntando as planilhas

data_iris <- rbind(data_AC, data_LE, data_LV, data_MF, data_MM, data_NM, data_VL)

### Baixando dados

write.csv(data_iris, "data_iris.csv", row.names = FALSE)
