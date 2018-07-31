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
library(sna); library(ggplot2); library(readxl)
library(dplyr); library(stringr); library(stringi); library(gsheet)

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
#====================================#

# baixar dados da planilha google
conselInsti<- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Z6LriQeZpTg5M7n9FmksAwgdumljIIgQn_4D504rAtk/edit?usp=sharing')

# salvar planilha 
write.csv(conselInsti, "resex_acau_goiana/data/conselInsti.csv", row.names = F)

#============= mergir bases ==================#

# colar nomes para codigo mergir
cont_cod_data$cod_representante <- cont_cod_data$Var1
conselInsti$cod_representante <- paste0(conselInsti$nome_representante, conselInsti$instituicao_sigla)

# mergir
resexData <- merge(cont_cod_data, conselInsti, by = "cod_representante", all = T)

#=======================#
# ANALISE DOS DADOS

# remover codes quali
resexDatax <- resexData[!is.na(resexData$instituicao_sigla),]

# renomear codes NA
resexDatax$obs[is.na(resexDatax$obs)] <- "conselheiro"

# remover analistas e chefes
resexDatax <- resexDatax[resexDatax$obs != "analista",]
resexDatax <- resexDatax[resexDatax$obs != "chefe",]

# agrupar por grupo
grupoData <- aggregate(resexDatax$Freq, by = list(resexDatax$categoria1), sum)

# ordenar
grupoData$Group.1 <- factor(grupoData$Group.1, levels = grupoData$Group.1[order(grupoData$x)])

# criar proporcionalidade
grupoData <- mutate(grupoData, proporcao = paste0( (round(((x/sum(x))*100), 1)),"%") )

#--- variavel prop por assento ---#

# numero de assentos (eleicoes resex)
grupoData$numero_assento <- c(6, 23, 12,4)

# prop por assento
grupoData <- mutate(grupoData, prop_assento = (round(((x/numero_assento)), 1)) )

# variavel resex
grupoData$UC <- "RESEX Acaú-Goiana"

# renomear variaveis
colnames(grupoData)[1] <- "grupo_setorial"
colnames(grupoData)[2] <- "numero_situacoes_de_fala"

# salvar
write.csv(grupoData, "analise_comparada/resex_fala_data.csv", row.names = F)

#==== visualizalizao ====#
ggplot(grupoData, aes(x = grupo_setorial, y = proporcao))+
  geom_bar(stat = "identity", fill = "#15041c") +
  geom_label(aes(label = proporcao), size = 3.2)+
  labs(y = "Porcentagem do Total", x = "", title = "") +
  coord_flip()+
  theme_minimal()%+replace% 
  theme(legend.position="bottom",
        axis.text.x = element_text(colour= "black",size=11,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=11,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="black",size=13,angle=90,hjust=0.5,vjust=0.6,face="plain"),
        title = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain")
  )
  

ggsave("prop_voz_RESEX.png", path = "resex_acau_goiana/resultados", width = 7, height = 3, units = "in")

