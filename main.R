setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(zoo)
library(ggplot2)

#~~~Data input

laukiantys <- read.csv(file="data/laukianciuju_eileje_ataskaita.csv",sep = ";",encoding="UTF-8")
darzeliai <- read.csv(file="data/darzeliai.csv",encoding="UTF-8")
grupes <- read.csv(file="data/grupes.csv",encoding="UTF-8")
istaigos <- read.csv(file="data/istaigos.csv",sep = ";", encoding="UTF-8")
vaikai_grup <- read.csv(file="data/lankanciu_vaiku_ataskaita_pagal_grupes.csv",sep = ";", encoding="UTF-8")
lankomumas <- read.csv(file="data/lankomumo_ziniarasciai_2016-07-01-2016-07-31.csv",sep = ";", encoding="UTF-8")
prasymai <- read.csv(file="data/visi_prasymai.csv", encoding="UTF-8")

#~Data checks & corrections

#~~If there are duplicates between lankomumas and laukiantys, the corresponding row is removed from laukiantys

seni.vaikai <- lankomumas[,c("Vaiko.Identifikacinis.Nr.","Darželio.pavadinimas")]
nauji.vaikai <- laukiantys[,c("Vaiko.Identifikacinis.Nr.","X1.pasirinktas.darželis")]
colnames(nauji.vaikai)[which(names(nauji.vaikai) == "X1.pasirinktas.darželis")] <- "Darželio.pavadinimas"
visi.vaikai <- rbind(seni.vaikai,nauji.vaikai)
jau.gave <- visi.vaikai[which(duplicated(visi.vaikai$Vaiko.Identifikacinis.Nr.)),]
jau.gave.id <- as.vector(jau.gave[,1])
laukiantys <- laukiantys[-which(laukiantys$Vaiko.Identifikacinis.Nr. %in% jau.gave.id),]

#~Laukiantys analysis

laukiantys$date <- substr(laukiantys$Prašymo.pateikimo.data,1,10)

laukiantys$kiek.laukia <- (as.yearmon(Sys.Date()) - as.yearmon(laukiantys[,"date"]))

ggplot(laukiantys, aes(x = kiek.laukia, fill = Prioritetas..deklaruotas.mieste.))  +
  geom_histogram(binwidth = 0.25) +
  xlab("Kiek metų jau laukia savo eilės šiai dienai") +
  ylab("Vaikų skaičius")

