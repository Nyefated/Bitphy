####Libraries####

library(httr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(dplyr)
library(splitstackshape)
library(reshape2)
library(xlsx)
library(tidyverse)

####Reading file and preparing main dataset####

Carnisseries <- read.csv("~/Ubiqum/Bitphy/SELLERs-Ubiqum-Bernat.csv", sep = ";", stringsAsFactors = T)
Carnisseries <- Carnisseries[,-c(6,7)]
Carnisseries <- data.table::setnames(Carnisseries, old = c('ï..shopId', 'timestamp', 'sellerId', 'customerId'),
                                     new = c('ID_botiga', 'date', 'venedorID', 'compradorID'))
Carnisseries$venedorID <- as.factor(Carnisseries$venedorID)
Carnisseries$compradorID <- as.factor(Carnisseries$compradorID)
Carnisseries$date <- strptime(Carnisseries$date, "%Y-%m-%d %H:%M:%S")
Carnisseries$date <- as.POSIXct(Carnisseries$date, tz = "UTC")
Carnisseries$date <- with_tz(Carnisseries$date, tzone = "Europe/Berlin")

#length(which(Carnisseries$billing==0)) = 20226 casos
#length(which(Carnisseries$billing<=0.50)) = 3454 casos (la majoria 2 i 5 centims, bosses?)

Carnisseries <- Carnisseries[!Carnisseries$billing<0,]

Carnisseries <- mutate(Carnisseries, hora = hour(date))
Carnisseries <- mutate(Carnisseries, diaSetmana = wday(date, label = T))
levels(Carnisseries$diaSetmana) <- c("diumenge","dilluns","dimarts","dimecres","dijous","divendres","dissabte")
Carnisseries <- mutate(Carnisseries, mes = month(date, label = T))
levels(Carnisseries$mes) <- c("gener","febrer","març","abril","maig","juny","juliol","agost",
                                     "setembre","octubre","novembre","desembre")
Carnisseries <- mutate(Carnisseries, Any = year(date))
Carnisseries <- mutate(Carnisseries, dia = day(date))

Venedors <- data.table::setDT(distinct(data.frame(Carnisseries$venedorID, Carnisseries$ID_botiga)),
                              keep.rownames = T)
names(Venedors) <- c("ID_venedor", "venedorID", "ID_botiga")
save(Venedors, file = "Venedors")

Carnisseries <- left_join(Carnisseries, Venedors)
Carnisseries$ID_venedor <- as.integer(Carnisseries$ID_venedor)
Carnisseries <- Carnisseries[,-3]
save(Carnisseries, file = "Carnisseries")


####Datasets de cada botiga####

#Botiga 5b4 (14 venedors)
Botiga_5b4 <- filter(Carnisseries, Carnisseries$ID_botiga=="5b4")
save(Botiga_5b4, file = "Botiga_5b4")

#Botiga 046 (7 venedors)
Botiga_046 <- filter(Carnisseries, Carnisseries$ID_botiga=="046")
save(Botiga_046, file = "Botiga_046")

#Botiga 19a (1 venedor)
Botiga_19a <- filter(Carnisseries, Carnisseries$ID_botiga=="19a")
save(Botiga_19a, file = "Botiga_19a")

#Botiga 432 (14 venedors)
Botiga_432 <- filter(Carnisseries, Carnisseries$ID_botiga=="432")
save(Botiga_432, file = "Botiga_432")

#Botiga 562 (17 venedors)
Botiga_562 <- filter(Carnisseries, Carnisseries$ID_botiga=="562")
save(Botiga_562, file = "Botiga_562")

#Botiga 5c3 (12 venedors)
Botiga_5c3 <- filter(Carnisseries, Carnisseries$ID_botiga=="5c3")
save(Botiga_5c3, file = "Botiga_5c3")

#Fer analisis per botiga perque hi ha bastanta diferencia entre els treballadors d'una
#i altra



