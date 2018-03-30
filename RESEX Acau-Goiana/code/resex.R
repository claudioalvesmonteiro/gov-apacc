#================================================#
# ASSEMBLEIA NA COSTA BRASILEIRA       
# RESEX ACAU-GOIANA
#================================================#
# ANALISE DE CONTEUDO DAS MEMORIAS               #
# Recife - Pernambuco - Brasil                   #
#------------------------------------------------#
# Funda??o Joaquim Nabuco (FUNDAJ)               #
#------------------------------------------------#
# Claudio A. Monteiro                            #
# claudiomonteirol.a@gmail.com                   #
#================================================#

#install.packages(c("RQDA","GGally", "network", "sna"), dependencies = T)

# carregar pacotes
library(GGally); library(network); library(sna); library(ggplot2); library(readxl)
library(dplyr); library(stringr); library(ggplot2); library(networkD3); library(RQDA)

# baixar atas 
setwd("~/Documents/git_projects/gov-apacc/RESEX Acau-Goiana/data/ata_pdf")

data_url <- data.frame(link = character("https://sites.google.com/site/bibliotecavirtualdaresex/home/atas-de-reunioes/ATA%20DA%207%C2%B0%20REUNI%C3%83O%20ORDIN%C3%81RIA%20DA%20RESEX.pdf?attredirects=0&d=1",
              "https://sites.google.com/site/bibliotecavirtualdaresex/home/atas-de-reunioes/ATA%20DA%208%C2%B0%20REUNI%C3%83O%20Ordin%C3%A1ria%20do%20Conselho%20Deliberativo%20da%20RESEX%20Aca%C3%BA-Goiana.pdf?attredirects=0&d=1"),
              file = c("ATA DA 7° REUNIÃO ORDINÁRIA DA RESEX.pdf",
                       "ATA DA 8° REUNIÃO Ordinária do Conselho Deliberativo da RESEX Acaú-Goiana"))

for (link in data_url){
  download.file(link, file)
  
}

download.file(data_url, data_file)
data_json <- geojson_read(data_file, what = "sp")
  download.file(
    "https://sites.google.com/site/bibliotecavirtualdaresex/home/atas-de-reunioes/ATA%20DA%208%C2%B0%20REUNI%C3%83O%20Ordin%C3%A1ria%20do%20Conselho%20Deliberativo%20da%20RESEX%20Aca%C3%BA-Goiana.pdf?attredirects=0&d=1",
    "ATA DA 8° REUNIÃO Ordinária do Conselho Deliberativo da RESEX Acaú-Goiana")




data_url$link


for (link in data_url){
  for (file in data_url){
    download.file(link, file)
  }
}





