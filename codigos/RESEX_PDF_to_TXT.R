#=====================================================================#
# DECISAO E PARTICIPACAO NO CONSELHO GESTOR DA RESEX ACAU-GOIANA      #
#=====================================================================#
# Analise da participacao - Assentos/Frequencia/Voz                   #
# Recife - Pernambuco - Brasil                                        #
#---------------------------------------------------------------------#
# Fundacao Joaquim Nabuco (FUNDAJ)                                    #
#---------------------------------------------------------------------#
# Claudio A. Monteiro                                                 #
# claudiomonteirol.a@gmail.com                                        #
#=====================================================================# 

# carregar pacotes
library(GGally); library(network); library(sna); library(ggplot2); library(RQDA); library(readxl)
library(RQDA); library(dplyr); library(stringr); library(stringi); library(networkD3)


#===== Tranformar PDF em .txt =====#

# diretorio dos arquivos
diretorio <- "/home/pacha/Documents/git_projects/publicgov_environment/resex_acau_goiana/data"

# vetor com lista dos arquivos pdf
myfiles <- list.files(path = diretorio, pattern = "pdf",  full.names = TRUE)

# remover espacos, caixa baixa e remover caracteres latinos, dos nomes dos arquivos
sapply(myfiles, FUN = function(i){
  file.rename(from = i, to =  paste0(dirname(i), "/", stri_trans_general(tolower(gsub(" ", "_", basename(i))),"Latin-ASCII")))
})

# novo vetor com lista dos arquivos pdf
myfiles <- list.files(path = diretorio, pattern = "pdf",  full.names = TRUE)

# aplicar ferramenta de conversao
lapply(myfiles, function(i) system(paste('"/home/pacha/Downloads/xpdf-tools-linux-4.00/bin64/pdftotext"', 
                                         paste0('"', i, '"')), wait = FALSE) )
