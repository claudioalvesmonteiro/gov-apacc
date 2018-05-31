#================================================#
# PIBIC Claudio Monteiro                         #        
# RESEX ACAU-GOIANA                              #
#================================================#
# ANALISE DE CONTEUDO DAS MEMORIAS               #
# Recife - Pernambuco - Brasil                   #
#------------------------------------------------#
# Fundacaoo Joaquim Nabuco (FUNDAJ)              #
#------------------------------------------------#
# Claudio A. Monteiro                            #
# claudiomonteirol.a@gmail.com                   #
#================================================#
 
# carregar pacotes
library(stringi)

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

#==== RQDA ====#
library(RQDA)
RQDA()

addFilesFromDir(diretorio, pattern = "*.txt$")




