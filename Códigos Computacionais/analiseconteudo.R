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
#================================================#

#install.packages(c("RQDA","GGally", "network", "sna"), dependencies = T)

# carregar pacotes
library(GGally); library(network); library(sna); library(ggplot2); library(RQDA)
library(RQDA); library(dplyr); library(stringr); library(ggplot2)

# executar pacote RQDA (p/ analise de conteudo)
RQDA()

# *Abra o projeto no RQDA para executar os demais codigos*

#==================================#
# CAPTURA DOS DADOS DA CODIFICACAO

# visualizar a contagem de cada codigo
sumario_cod <- summaryCodings()

# contagem de cada codigo
cont_cod_data <- data.frame(sumario_cod$NumOfCoding)

# visualizar infos sobre cada codificacao
coding_table <- getCodingTable()

#================================#
# Analise dos temas em debate    #
#================================# 

# selecionar contagem de temas
cont_cod_data <- mutate(cont_cod_data, tema = 0)
cont_cod_data$tema[str_detect(cont_cod_data$Var1, "tema_")] <- 1
cont_cod_tema <- cont_cod_data[cont_cod_data$tema == 1,]

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


#============================================#
# Variacao de codigos nos arquivos (tempo)   # **EM CONSTRUCAO**
#============================================#

# Return a data frame which indicates what codes are associated with each file.
#The result is a data frame. Each row represents one file, and each variable represents one code. If a
#file is coded by a code, the value of that variable is 1, otherwise it is 0.
filesByCodes(codingTable = c("coding", "coding2"))

# Get files coded by specific codes, by specifying the code IDs
filesCodedByAnd(cid = 24, codingTable=c("coding"))

# visualizar ids dos arquivos
getFileIds()

# visualizar nomes dos arquivos
getFileNames(fid = getFileIds())

#==============================================#
# Codificacao automatizada por palavra chave
#==============================================#

# cid 85 = TESTE_pesca # codingBySearch("pesc",fid=getFileIds(),cid=85)
# cid 86 = TESTE_turismo # codingBySearch("turis",fid=getFileIds(),cid=86)  
# cid 87 = TESTE_pesquisa # codingBySearch("pesq",fid=getFileIds(),cid=87)

# codificar nomes dos conselheiros
codingBySearch("Mauro Maida",fid=getFileIds(),cid=26)
# cid 87 = TESTE_pesquisa # codingBySearch("pesq",fid=getFileIds(),cid=87)
# cid 87 = TESTE_pesquisa # codingBySearch("pesq",fid=getFileIds(),cid=87)


#---- analise ----#

# selecionar codigos e juntar bases
debate_area85 <- getCodingsByOne(85, fid=NULL, codingTable=c("coding"))
debate_area86 <- getCodingsByOne(86, fid=NULL, codingTable=c("coding"))
debate_area87 <- getCodingsByOne(87, fid=NULL, codingTable=c("coding"))
debate_area <- rbind(debate_area85, debate_area86, debate_area87)

# contagem
debate_area_cont <- data.frame(table(debate_area$cid))

# nomear codigos
debate_area_cont$nomes <- c("Pesca", "Turismo", "Pesquisa")

# calcular proporcao e arredondar
debate_area_cont <- mutate(debate_area_cont, prop_tema = (Freq / sum(Freq))*100 )
debate_area_cont$prop_tema2 <- paste(round(debate_area_cont$prop_tema, 2), "%", sep="")

# visualizar graficamente
ggplot(debate_area_cont, aes(x = nomes, y = prop_tema))+
  geom_bar(stat = "identity", fill = "#15041c") +
  geom_label(aes(x = nomes, y = prop_tema, label = prop_tema2))+
  labs(y = "Porcentagem", x = "", title = "") +
  coord_flip()


#=================================================#
#             RELACAO ENTRE CODIGOS               #
#=================================================#

# funcao para relacionar codigos
?crossCodes

#======================================#
# TEMAS X CATEGORIAS DE ANALISE        #
#======================================#

# selecionar codigos de temas e categorias
crosscod1 <- c(as.character(cont_cod_data$Var1[str_detect(cont_cod_data$Var1, "tema_")]),
as.character(cont_cod_data$Var1[str_detect(cont_cod_data$Var1, "cat_")]))

prox1_matrix <- crossCodes(codeList = crosscod1, 
                           data = coding_table, 
                           relation = "exact")

# transformar matrix em dataframe
data_flow <- data.frame(as.table(prox1_matrix))

# capturar numero dos codigos
cod_seq <- regmatches(data_flow$Var1, gregexpr("[[:digit:]]+", data_flow$Var1))
data_flow$nod_cod <- as.numeric(unlist(cod_seq))

# transformar variaveis
data_flow$Var2 <- as.numeric(as.character(data_flow$Var2))
data_flow$Var1 <- as.character(data_flow$Var1)

# definir nomes dos nodes
nomes <- data.frame(nome_nod = crosscod1)
nomes$nome_nod <- as.character(nomes$nome_nod)
  
flow_unique <- data_flow[!duplicated(data_flow$Var1),]

# limpar base
data_flow <- mutate(data_flow, clean = ifelse(Var2 == nod_cod | is.na(Freq), 1, 0))
data_flow <- data_flow[data_flow$clean == 0,]

# function to create merge string based on similarity
best_match= function(string_vector,string_replacement){
  library(stringi)
  s<-string_replacement %>% 
    purrr::map_int(~{
      .x %>% 
        RecordLinkage::levenshteinSim(string_vector) %>%
        match(max(.),.)
    })
  string_vector[s] = string_replacement
  return(string_vector)
}

flow_unique$nome_nod <- best_match(flow_unique$Var1, nomes$nome_nod)

# mergir nome e numero dos codigos
nomes <- merge(flow_unique, nomes, by = "nome_nod")

# definir grupos
nomes <- mutate(nomes, grupos = "")
nomes$grupos[str_detect(nomes$nome_nod, "cat_")] <- "Categoria de Análise" 
nomes$grupos[str_detect(nomes$nome_nod, "tema_")] <- "Tema de Debate" 

# criar IDs
data_flow$IDsource = match(data_flow$nod_cod, nomes$nod_cod)-1 
data_flow$IDtarget = match(data_flow$Var2, nomes$nod_cod)-1

# selecionar relacoes entre temas e codigos ##
data_flow <- mutate(data_flow, IN = "")
data_flow$IN[str_detect(data_flow$Var1, "tema_")] <- "TEMA" 
data_flow$IN[str_detect(data_flow$Var1, "cat_")] <- "CAT" 

data_flow <- mutate(data_flow, OUT = "")
paste_cat <- nomes$nod_cod[str_detect(nomes$Var1, "cat_")]
paste_tema <- nomes$nod_cod[str_detect(nomes$Var1, "tema_")]
data_flow$OUT[str_detect(data_flow$Var2, paste(paste_cat, collapse = '|'))] <- "CAT"
data_flow$OUT[str_detect(data_flow$Var2, paste(paste_tema, collapse = '|'))] <- "TEMA"

cod_nome_in <- data_flow[,c("nod_cod", "IN")]

str_detect(data_flow$Var2, "st\\.|ste\\.", "st")

test <- merge(data_flow$Var2)

# Make the Network
# https://www.r-graph-gallery.com/253-custom-network-chart-networkd3/7
# https://christophergandrud.github.io/networkD3/

sankeyNetwork(Links = data_flow, Nodes = nomes,
              Source = "IDsource", Target = "IDtarget",
              Value = "Freq", NodeID = "nome_nod", 
              fontSize = 12, nodeWidth = 30,
              NodeGroup = "grupos"
              )

forceNetwork(
  data_flow, 
  Nodes = nomes,
  Source = "IDsource", 
  Target = "IDtarget",
  Value = "Freq", 
  NodeID = "nome_nod", 
  Group = "grupos",
  
  linkDistance = 200,   
  opacity = 0.7,
  legend = T,  
  height = 700,                                               
  width = 700,
  zoom = TRUE ,
  fontSize = 17,                                                    
  fontFamily = "serif"
  )


# From these flows we need to create a node data frame: it lists every entities involved in the flow
# nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())


prox1_matrix <- crossCodes(
  codeList = c("cat_CONFLITO", "tema_FISCALIZAÇÃOeMONITOR", "eduardo_machado", "cat_COOPERAÇÃO", "beatriz_mesquita", "tema_PESCA"), 
  data = coding_table, relation = "proximity")

#=============================#
# codigos flow complelto      #
#=============================#

# selecionar codes e code names
codes <- coding_table[!duplicated(coding_table$cid),]
codes <- codes[,c(2, 4)]
  
# matriz de inclusao entre codigos
prox2_matrix <- crossCodes(
  codeList = codes$codename,
    data = coding_table, relation = "inclusion")

# transformar matriz em dataframe
data_flow <- as.data.frame(as.table(prox2_matrix))

# transformar coluna 1 em numeric
cod_seq <- regmatches(data_flow$Var1, gregexpr("[[:digit:]]+", data_flow$Var1))
data_flow$Var1 <- as.numeric(unlist(cod_seq))

# limpar base
data_flow <- mutate(data_flow, clean = ifelse(Var1 == Var2 | is.na(Freq), 1, 0))
data_flow <- data_flow[data_flow$clean == 0,]
data_flow$Var2 <- as.numeric(as.character(data_flow$Var2))

# criar IDs
data_flow$IDsource = match(data_flow$Var1, codes$cid)-1 
data_flow$IDtarget = match(data_flow$Var2, codes$cid)-1

# Make the Network
# https://www.r-graph-gallery.com/253-custom-network-chart-networkd3/7
# https://christophergandrud.github.io/networkD3/

sankeyNetwork(Links = data_flow, 
              Nodes = codes,
              Source = "IDsource", 
              Target = "IDtarget",
              Value = "Freq", 
              NodeID = "codename", 
              fontSize = 12, 
              nodeWidth = 30
)

forceNetwork(
  data_flow, 
  Nodes = codes,
  Source = "IDsource", 
  Target = "IDtarget",
  Value = "Freq", 
  NodeID = "codename", 
  Group = "codename",
  
  linkDistance = 200,   
  opacity = 0.7,
  legend = F,  
  height = 700,                                               
  width = 700,
  zoom = TRUE ,
  fontSize = 17,                                                    
  fontFamily = "serif"
)


fun1 <- function(x){
if(x > 10) 
  print("Opa")
  else
    print("ent")
}

fun1(1300)



