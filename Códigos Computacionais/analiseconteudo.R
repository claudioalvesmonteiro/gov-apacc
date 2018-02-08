#================================================#
# ASSEMBLEIA NA COSTA BRASILEIRA                 #
#================================================#
# ANALISE DE CONTEUDO DAS MEMORIAS               #
# Recife - Pernambuco - Brasil                   #
#------------------------------------------------#
# Fundação Joaquim Nabuco (FUNDAJ)               #
#------------------------------------------------#
# Claudio A. Monteiro                            #
# claudiomonteirol.a@gmail.com                   #
#------------------------------------------------#
# Any question contact the developer             #
# #UseFreeSoftware                               #
#------------------------------------------------#

# carregar pacotes
library(RQDA); library(dplyr); library(stringr); library(ggplot2)

# executar RQDA
RQDA()

# *Abra o projeto no RQDA para executar os demais codigos*


#==================================#
# Selecao e manipulacao dos dados

# visualizar a contagem de cada codigo
sumario_cod <- summaryCodings()

# contagem de cada codigo
cont_cod_data <- data.frame(sumario_cod$NumOfCoding)

# visualizar infos sobre cada codificacao
coding_table <- getCodingTable()


#================================#
# Analise dos temas em debate

# selecionar contagem de temas
cont_cod_data <- mutate(cont_cod_data, tema = 0)
cont_cod_data$tema[str_detect(cont_cod_data$Var1, "tema_")] <- 1
cont_cod_tema <- cont_cod_data[cont_cod_data$tema == 1,]

# calcular proporcao e arredondar
cont_cod_tema <- mutate(cont_cod_tema, prop_tema = (Freq / sum(Freq))*100 )
cont_cod_tema$prop_tema2 <- paste(round(cont_cod_tema$prop_tema, 2), "%", sep="")

# renomear colunas
cont_cod_tema <- mutate(cont_cod_tema, nomes_temas = Var1)
cont_cod_tema$nomes_temas <- c("Educação Socioambiental", "Fiscalização e Monitoramento", "Institucional",  "Manguezal", "Pesca",
                                "Pesquisa", "Plano de Manejo", "Recursos Financeiros", "Turismo", "Zoneamento")

# ordenar
cont_cod_tema$nomes_temas <- factor(cont_cod_tema$nomes_temas, levels = cont_cod_tema$nomes_temas[order(cont_cod_tema$prop_tema)])

# visualizar graficamente
ggplot(cont_cod_tema, aes(x = nomes_temas, y = prop_tema))+
  geom_bar(stat = "identity", fill = "#15041c") +
  geom_label(aes(x = nomes_temas, y = prop_tema, label = prop_tema2))+
  labs(y = "Proporção", x = "", title = "") +
  coord_flip()

#===========================
# relacao entre codigos

# funcao para relacionar codigos
?crossCodes

# qual a relacao entre cat_CONFLITO (cid57), tema_FISCALIZAÇÃOeMONITOR (cid59) e eduardo_machado (cid29)
crossCodes(codeList = c("cat_CONFLITO", "tema_FISCALIZAÇÃOeMONITOR", "eduardo_machado"), data = coding_table, relation = "proximity")
crossCodes(codeList = c("cat_CONFLITO", "tema_FISCALIZAÇÃOeMONITOR", "eduardo_machado"), data = coding_table, relation = "overlap")
crossCodes(codeList = c("cat_CONFLITO", "tema_FISCALIZAÇÃOeMONITOR", "eduardo_machado"), data = coding_table, relation = "inclusion")
crossCodes(codeList = c("cat_CONFLITO", "tema_FISCALIZAÇÃOeMONITOR", "eduardo_machado"), data = coding_table, relation = "exact")

crossCodes(codeList = c("cat_CONFLITO", "TESTE_pesca"), data = coding_table, relation = "exact")

#============================#
# Variacao de codigos no tempo

# Return a data frame which indicates what codes are associated with each file.
#The result is a data frame. Each row represents one file, and each variable represents one code. If a
#file is coded by a code, the value of that variable is 1, otherwise it is 0.
filesByCodes(codingTable = c("coding", "coding2"))

# Get files coded by specific codes, by specifying the code IDs
filesCodedByAnd(cid = 24, codingTable=c("coding"))

#==============================================#
# Codificacao automatizada por palavra chave
# *ADIANTO"

# cid 85 = TESTE_pesca
codingBySearch("pesca",fid=getFileIds(),cid=85)
codingBySearch("pescador",fid=getFileIds(),cid=85)

# cid 85 = TESTE_pesca
codingBySearch("turismo",fid=getFileIds(),cid=86)
codingBySearch("turis",fid=getFileIds(),cid=86)  # melhor


#=====================#
# ANNOTATIONS

#  Get cases coded by specific codes, by specifying the code IDs.
casesCodedByOr(cid =c(18, 19, 20))




# ATENCAO ESSE AQUI 
x1 <- getCodingsByOne(2, fid=NULL, codingTable=c("coding"))
View(x1)

# visualizar ids dos arquivos
getFileIds()

# visualizar nomes dos arquivos
getFileNames(fid = getFileIds())

# 
c1 <- subset(coding_table,cid==57)
c1 <- subset(coding_table,cid==24)


#
RQDATables()
RQDA


RQDAQuery("select name from source where status=1")











