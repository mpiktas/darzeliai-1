setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

laukiantys <- read.csv(file="laukianciuju_eileje_ataskaita.csv",sep = ";",encoding="UTF-8")

#this is important for testing