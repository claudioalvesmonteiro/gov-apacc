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
library(RQDA); library(dplyr); library(stringr); library(ggplot2); library(networkD3)

# tema para os graficos em ggplot
tema_massa <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour= "black",size=11,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=11,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=13,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
}


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

#==================================#
# CAPTURA DOS DADOS DA CODIFICACAO

# visualizar a contagem de cada codigo
sumario_cod <- summaryCodings()

# contagem de cada codigo
cont_cod_data <- data.frame(sumario_cod$NumOfCoding)

# visualizar infos sobre cada codificacao
coding_table <- getCodingTable()


#====================================#
# DADOS DAS INSTITUICOES

library(gsheet)
conselInsti<- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Z6LriQeZpTg5M7n9FmksAwgdumljIIgQn_4D504rAtk/edit?usp=sharing')

######################################

#============= mergir bases ==================#
cont_cod_data$cod_representante <- cont_cod_data$Var1
conselInsti$cod_representante <- paste0(conselInsti$nome_representante, conselInsti$instituicao_sigla)

resexData <- merge(cont_cod_data, conselInsti, by = "cod_representante", all = T)

#=======================#
# ANALISE DOS DADOS

# remover codes quali
resexDatax <- resexData[!is.na(resexData$instituicao_sigla),]

# renomear codes NA
resexDatax$obs[is.na(resexDatax$obs)] <- "conselheirx"

# remover analistas e chefes
resexDatax <- resexDatax[resexDatax$obs != "analista",]
resexDatax <- resexDatax[resexDatax$obs != "chefe",]

# agrupar por grupo
grupoData <- aggregate(resexDatax$Freq, by = list(resexDatax$categoria1), sum)

# ggplot2
ggplot(grupoData, aes(x = Group.1, y = x))+
  geom_bar(stat = "identity", fill = "#15041c") +
  geom_label(aes(label = x), size = 3.2)+
  labs(y = "Situações de Fala", x = "", title = "") +
  coord_flip()
ggsave("prop_voz_cat2.png", path = "Resultados",
       width = 8, height = 3, units = "in")














