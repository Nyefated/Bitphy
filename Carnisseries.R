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

Venedors <- data.table::setDT(distinct(data.frame(Carnisseries$venedorID, Carnisseries$ID_botiga)),
                              keep.rownames = T)
names(Venedors) <- c("ID_venedor", "venedorID", "ID_botiga")

Carnisseries <- left_join(Carnisseries, Venedors)
Carnisseries$ID_venedor <- as.integer(Carnisseries$ID_venedor)
Carnisseries <- Carnisseries[,-3]


ggplot(Carnisseries[!Carnisseries$billing<1,], aes(x=hora, y=billing)) + stat_summary(fun.y = "mean", geom = "bar")



