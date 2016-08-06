setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

laukiantys <- read.csv(file="data/laukianciuju_eileje_ataskaita.csv",sep = ";",encoding="UTF-8")

