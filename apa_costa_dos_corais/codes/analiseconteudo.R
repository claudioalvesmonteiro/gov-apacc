#================================================#
# ASSEMBLEIA NA COSTA BRASILEIRA                 #
#================================================#
# ANALISE DE CONTEUDO DAS MEMORIAS               #
# Recife - Pernambuco - Brasil                   #
#------------------------------------------------#
# Fundacao Joaquim Nabuco (FUNDAJ)               #
#------------------------------------------------#
# Claudio A. Monteiro                            #
# claudiomonteirol.a@gmail.com                   #
#================================================#

#install.packages(c("RQDA","GGally", "network", "sna"), dependencies = T)

# carregar pacotes
library(GGally); library(network); library(sna); library(ggplot2); library(readxl)
library(dplyr); library(stringr); library(ggplot2); library(networkD3); library(RQDA)

# 
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

#================================#
# Analise dos temas em debate    #
#================================# 

# selecionar contagem de temas

# criar variavel 'tema' no banco 'cont_cod_data'
cont_cod_data <- mutate(cont_cod_data, tema = 0)

# detectar termo 'tema_' na varivael 'Var1' e atribuir valor 1 a variavel 'tema'
cont_cod_data$tema[str_detect(cont_cod_data$Var1, "tema_")] <- 1

# selecionar os casos em que tema = 1
cont_cod_tema <- cont_cod_data[cont_cod_data$tema == 1,]

# calcular proporcao e arredondar
cont_cod_tema <- mutate(cont_cod_tema, prop_tema = (Freq / sum(Freq))*100 )

# criar variavel de prop com "%" 
cont_cod_tema$prop_tema2 <- paste(round(cont_cod_tema$prop_tema, 2), "%", sep="")

# criar uma nova variavel com nomes dos temas
cont_cod_tema <- mutate(cont_cod_tema, nomes_temas = Var1)
cont_cod_tema$nomes_temas <- c("Educação Socioambiental", "Fiscalização e Monitoramento", "Institucional APACC", 
                               "Institucional CONAPAC", "Plano de Manejo", "Recursos Financeiros", "Zoneamento")

# ordenar
cont_cod_tema$nomes_temas <- factor(cont_cod_tema$nomes_temas, 
                                    levels = cont_cod_tema$nomes_temas[order(cont_cod_tema$prop_tema)])

# visualizar graficamente e salvar
ggplot(cont_cod_tema, aes(x = nomes_temas, y = prop_tema))+
  geom_bar(stat = "identity", fill = "#15041c") +
  geom_label(aes(x = nomes_temas, y = prop_tema, label = prop_tema2), size = 2.3)+
  labs(y = "Procentagem do Total", x = "", title = "") +
  coord_flip()
ggsave("prop_debate_tema.png", path = "Resultados",
       width = 7, height = 3, units = "in")

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

# codificar termos de destaque
codingBySearch("Mangue",fid=getFileIds(),cid=50)
codingBySearch("Pesca",fid=getFileIds(),cid=64)
codingBySearch("Turismo",fid=getFileIds(),cid=65)

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

#crosscod1 <- crosscod1[-12]

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

#===== selecionar relacoes entre temas e codigos =====#
#data_flow <- mutate(data_flow, IN = "")
#data_flow$IN[str_detect(data_flow$Var1, "tema_")] <- "TEMA" 
#data_flow$IN[str_detect(data_flow$Var1, "cat_")] <- "CAT" 

#data_flow <- mutate(data_flow, OUT = "")
#paste_cat <- nomes$nod_cod[str_detect(nomes$Var1, "cat_")]
#paste_tema <- nomes$nod_cod[str_detect(nomes$Var1, "tema_")]
#data_flow$OUT[str_detect(data_flow$Var2, paste(paste_cat, collapse = '|'))] <- "CAT"
#data_flow$OUT[str_detect(data_flow$Var2, paste(paste_tema, collapse = '|'))] <- "TEMA"

#data_flow <- mutate(data_flow, select = ifelse(IN == OUT, 1, 0))
#data_flow <- data_flow[data_flow$select == 0,]

data_flow_mani <- data_flow[data_flow$Freq != 0,]


# Make the Network
# https://www.r-graph-gallery.com/253-custom-network-chart-networkd3/7
# https://christophergandrud.github.io/networkD3/

ColourScale <- 'd3.scaleOrdinal()
            .domain(["Categoria de An?lise", "Tema de Debate"])
           .range(["#FF6900", "#694489"]);'

#===== SANKEYNETWORK =====#
sankeyNetwork(Links = data_flow_mani, Nodes = nomes,
              Source = "IDsource", Target = "IDtarget",
              Value = "Freq", NodeID = "nome_nod", 
              fontSize = 12, nodeWidth = 30,
              NodeGroup = "grupos",
              sinksRight = FALSE,
              iterations = 0,
              colourScale = JS(ColourScale))

#===== NETWORK3 ======#
network_tema_cat <- 
  forceNetwork(data_flow_mani, Nodes = nomes, Source = "IDsource",  Target = "IDtarget",
               Value = "Freq",  NodeID = "nome_nod",  Group = "grupos",
               opacityNoHover = 1, linkDistance = 300, opacity = 0.9, legend = T,  
               height = 600, width = 600, zoom = TRUE , fontSize = 12,                                                    
               fontFamily = "serif", colourScale = JS(ColourScale) )
network_tema_cat
saveNetwork(network_tema_cat ,file = 'network_tema_categoria_all.html', selfcontained=TRUE)

# visualizar conflito
conflito <- data_flow_mani[str_detect(data_flow_mani$Var1, "CONFLITO") |
                             str_detect(data_flow_mani$Var2, "57") ,]
conflito <- mutate(conflito, prop = round((Freq / sum(Freq)*100), 2) )
conflito <- conflito[order(conflito$prop),]

# visualizar cooperacao
coop <- data_flow_mani[str_detect(data_flow_mani$Var1, "COOP"),]
conflito <- mutate(conflito, prop = round((Freq / sum(Freq)*100), 2) )

# From these flows we need to create a node data frame: it lists every entities involved in the flow
# nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())


#============================================#
# CONSEL X ATUACAO                           #
#============================================#
# **** problema com codigos de inclus?o *****
#============================================#

# selecionar codigos de temas e categorias
paste_2 <- c("cat_", "tema_", "DESTAQUES", "DUVIDA_")
cont_cod_data <- mutate(cont_cod_data, select = 1)
cont_cod_data$select[str_detect(cont_cod_data$Var1, paste(paste_2, collapse = '|'))] <- 2

crosscod2 <- cont_cod_data[cont_cod_data$select == 1,]
crosscod2 <- as.character(crosscod2$Var1)

include2_matrix <- crossCodes(codeList = crosscod2, 
                           data = coding_table, 
                           relation = "inclusion")

# transformar matrix em dataframe
data_flow2 <- data.frame(as.table(include2_matrix))

# capturar numero dos codigos
cod_seq2 <- regmatches(data_flow2$Var1, gregexpr("[[:digit:]]+", data_flow2$Var1))
data_flow2$nod_cod <- as.numeric(unlist(cod_seq2))

# transformar variaveis
data_flow2$Var2 <- as.numeric(as.character(data_flow2$Var2))
data_flow2$Var1 <- as.character(data_flow2$Var1)

# definir nomes dos nodes
nomes2 <- data.frame(nome_nod = crosscod2)
nomes2$nome_nod <- as.character(nomes2$nome_nod)

flow_unique2 <- data_flow2[!duplicated(data_flow2$Var1),]

# limpar base
data_flow2 <- mutate(data_flow2, clean = ifelse(Var2 == nod_cod | is.na(Freq), 1, 0))
data_flow2 <- data_flow2[data_flow2$clean == 0,]

flow_unique2$nome_nod <- best_match(flow_unique2$Var1, nomes2$nome_nod)

# mergir nome e numero dos codigos
nomes2 <- merge(flow_unique2, nomes2, by = "nome_nod")

# definir grupos
nomes2 <- mutate(nomes2, grupos = NA)
nomes2$grupos[str_detect(nomes2$nome_nod, "atua_")] <- "Atua??o nos Debates" 
nomes2$grupos[is.na(nomes2$grupos)] <- "Representante" 

# criar IDs
data_flow2$IDsource = match(data_flow2$nod_cod, nomes2$nod_cod)-1 
data_flow2$IDtarget = match(data_flow2$Var2, nomes2$nod_cod)-1

# selecionar relacoes entre representantes e atuacao ##
data_flow2 <- mutate(data_flow2, IN = NA)
data_flow2$IN[str_detect(data_flow2$Var1, "atua_")] <- "ATUA" 
data_flow2$IN[is.na(data_flow2$IN)] <- "REP" 

data_flow2 <- mutate(data_flow2, OUT = "")
paste2_atua <- nomes2$nod_cod[nomes2$grupos == "Atua??o nos Debates"]
paste2_rep <- nomes2$nod_cod[nomes2$grupos == "Representante"]
data_flow2$OUT[str_detect(data_flow2$Var2, paste(paste2_atua, collapse = '|'))] <- "ATUA"
data_flow2$OUT[str_detect(data_flow2$Var2, paste(paste2_rep, collapse = '|'))] <- "REP"

data_flow2 <- mutate(data_flow2, select = ifelse(IN == OUT, 1, 0))
data_flow2 <- data_flow2[data_flow2$select == 0,]
data_flow2 <- data_flow2[data_flow2$Freq != 0,]
#data_flow2_mani <- data_flow2[data_flow2$Freq > 1,]

# 
mani1 <- as.factor(data_flow2$Var1)
mani1 <- data.frame(Var1 = levels(mani1))
mani1$Var1 <- as.character(mani1$Var1) 
nomes3 <- merge(mani1, nomes2, by = "Var1")

#

ColourScale <- 'd3.scaleOrdinal()
.domain(["Atua??o nos Debates", "Representante"])
.range(["#FF6900", "#694489"]);'

#===== SANKEYNETWORK =====#
sankeyNetwork(Links = data_flow2, Nodes = nomes2,
              Source = "IDsource", Target = "IDtarget",
              Value = "Freq", NodeID = "nome_nod", 
              fontSize = 12, nodeWidth = 30,
              NodeGroup = "grupos",
              sinksRight = FALSE,
              iterations = 0,
              colourScale = JS(ColourScale))

#===== NETWORK3 ======#
network_rep_atua <- 
  forceNetwork(data_flow2, Nodes = nomes2, Source = "IDsource",  Target = "IDtarget",
               Value = "Freq",  NodeID = "nome_nod",  Group = "grupos",
               opacityNoHover = 1, linkDistance = 300, opacity = 1, legend = T,  
               height = 700, width = 700, zoom = TRUE , fontSize = 12,                                                    
               fontFamily = "serif", colourScale = JS(ColourScale) )
network_rep_atua

saveNetwork(network_rep_atua, file = 'Resultados/network_rep_atua.html', selfcontained=TRUE)

#======================================#
#     ANALISE DE VOZ NOS DEBATES       #
#======================================#

# selecionar codigos dos representantes
paste_voz<- c("cat_", "tema_", "DESTAQUES", "DUVIDA_", "atua_", "DECISOES", "termo_")
cont_cod_data <- mutate(cont_cod_data, select_voz = 1)
cont_cod_data$select_voz[str_detect(cont_cod_data$Var1, paste(paste_voz, collapse = '|'))] <- 2

codes_represent <- cont_cod_data[cont_cod_data$select_voz == 1,]

# importar base de instituicoes por representante
insti_categorias <- read_excel("Dados/institui??es_apacc_2.0.xlsx")

#==== match nomes dos representantes =====#

# limpar bases
library(stringi)
codes_represent$Var1 <- str_replace_all(codes_represent$Var1, "_", " ")
codes_represent$Var1 <- stri_trans_general(codes_represent$Var1 , "Latin-ASCII")
codes_represent$nome_consel <- as.character(codes_represent$Var1)

insti_categorias$nome_consel <- stri_trans_general(insti_categorias$nome_consel , "Latin-ASCII")

# mergir base de conselheiros
data_consel <- merge(insti_categorias, codes_represent, by = "nome_consel")

# criar e salvar base de representantes n-conselheiros
data_rep_nconsel <- codes_represent[str_detect(codes_represent$nome_consel, "1"),]
#write.csv(data_rep_nconsel, file = "Dados/data_rep_nconsel.csv")

# importar base editada manulamente
data_rep_nconsel <- read_excel("Dados/intitui??es_apacc3.xlsx")

#==== megir bases representantes ====#
base_representantes <- rbind(data_rep_nconsel, data_consel[,-c(5,7)])
#write.csv(base_representantes, file = "Dados/base_representantes_voz.csv")

#=================================#
# Visualizacao grafica

#===== CATEGORIA 1 =====#

# contar
count_cat1 <- aggregate(base_representantes$Freq, by=list(Category=base_representantes$categoria1), FUN=sum)

# sem os gestores
base_rep_sem_icmbio <- base_representantes[base_representantes$entidade_sigla != "ICMBIO",]
count_cat1 <- aggregate(base_rep_sem_icmbio$Freq, by=list(Category=base_rep_sem_icmbio$categoria1), FUN=sum)

# transformar em prop 
count_cat1 <- mutate(count_cat1, prop_cat1 = (x / sum(x))*100 )

# renomear categoria 6
count_cat1$Category[6] <- "Organiza??es de educa??o, cultura  \n  e associa??es comunit?rias"

# ordenar
count_cat1$Category <- factor(count_cat1$Category, 
                              levels = count_cat1$Category[order(count_cat1$prop_cat1)])

count_cat1$prop_cat1.2 <- paste(round(count_cat1$prop_cat1, 2), "%", sep="")

count_cat1$categoria_inst <- c("Sociedade Civil", "Sociedade Civil","Poder P?blico", "Poder P?blico",
                               "Sociedade Civil", "Sociedade Civil")


# ggplot2
ggplot(count_cat1, aes(x = Category, y = prop_cat1))+
  geom_bar(stat = "identity", aes(fill = count_cat1$categoria_ins)) +
  scale_fill_manual("Categoria",values=c("#15041c", "lightgreen"))+
  geom_label(aes(x = Category, y = prop_cat1, label = prop_cat1.2), size = 2.5)+
  labs(y = "Porcentagem do Total", x = "", title = "") +
  coord_flip()+
  theme_minimal()%+replace% 
  theme(legend.position="bottom")

ggsave("prop_voz_cat.png", path = "Resultados",
       width = 8, height = 3, units = "in")


#===== CATEGORIA 2 =====#

# contar
count_cat2 <- aggregate(base_representantes$Freq, by=list(Category=base_representantes$categoria2), FUN=sum)

# sem os gestores
count_cat2 <- aggregate(base_rep_sem_icmbio$Freq, by=list(Category=base_rep_sem_icmbio$categoria2), FUN=sum)

# transformar em prop e ordenar
count_cat2 <- mutate(count_cat2, prop_cat2 = (x / sum(x))*100 )
count_cat2$prop_cat2 <- round(count_cat2$prop_cat2, 2)
count_cat2$Category <- factor(count_cat2$Category, 
                              levels = count_cat2$Category[order(count_cat2$prop_cat2)])
count_cat2$prop_cat2.2 <- paste(round(count_cat2$prop_cat2, 2), "%", sep="")

# ggplot2
ggplot(count_cat2, aes(x = Category, y = prop_cat2))+
  geom_bar(stat = "identity", fill = "#15041c") +
  geom_label(aes(x = Category, y = prop_cat2, label = prop_cat2.2), size = 3.2)+
  labs(y = "Porcentagem", x = "", title = "") +
  coord_flip()
ggsave("prop_voz_cat2.png", path = "Resultados",
       width = 8, height = 3, units = "in")


#====================================
# IDENT MEMO

c("formato atual as mesmas n?o t?m")


codingBySearch("contrata??o de servi?os e produ??o de material de divulga??o",fid=getFileIds(),cid=130)

