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

# Abra o projeto no RQDA para executar os demais codigos

# visualizar a contagem de cada codigo
x <- summaryCodings()
x$NumOfCoding

# visualizar infos sobre cada codificacao
getCodingTable()

# relacao entre dois codigos
?relation


relation(1, 2)










