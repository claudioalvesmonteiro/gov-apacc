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

# carregar pacote
library(RQDA)

# executar RQDA
RQDA()

# *Abra o projeto no RQDA para executar os demais codigos*

# visualizar a contagem de cada codigo
x <- summaryCodings()
x$NumOfCoding

# visualizar infos sobre cada codificacao
coding_table <- getCodingTable()

# relacao entre dois codigos
?relation

#=====================#


#  Get cases coded by specific codes, by specifying the code IDs.
casesCodedByOr(cid =c(18, 19, 20))

#
codingBySearch("internet",fid=1,cid=2)


# ver relacao entre dois codigos
crossTwoCodes(57, 24, data = coding_table, relation = "overlap")
crossTwoCodes(57, 24, data = coding_table, relation = "proximity")
crossCodes(codeList = c(57, 24, 26), data = coding_table, relation = "proximity")
crossTwoCodes(57, 24, data = coding_table, relation = "proximity")


# Return a data frame which indicates what codes are associated with each file.
#The result is a data frame. Each row represents one file, and each variable represents one code. If a
#file is coded by a code, the value of that variable is 1, otherwise it is 0.
filesByCodes(codingTable = c("coding", "coding2"))

# Get files coded by specific codes, by specifying the code IDs
filesCodedByAnd(cid = 24, codingTable=c("coding"))


# ATENCAO ESSE AQUI 
x1 <- getCodingsByOne(57, fid=NULL,codingTable=c("coding"))
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











