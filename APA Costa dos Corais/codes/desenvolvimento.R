#=====================================================================#
# DECISAO E PARTICIPACAO NO CONSELHO GESTOR DA APA COSTA DOS CORAIS   #
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
    theme(axis.text.x = element_text(colour= "black",size=12,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
}
 
# carregar bases
apacc_atas <- read_excel("data/Listas Presença Conselho APACC.xlsx")
consel_insti <- read_excel("data/instituições_apacc_2.0.xlsx")
represent_nao_consel  <- read_excel("data/intituições_apacc3.xlsx")

#==========================================#
# Manipulacao dos dados da codificacao     #
#==========================================#

# visualizar a contagem de cada codigo
sumario_cod <- summaryCodings()

# contagem de cada codigo
cont_cod_data <- data.frame(sumario_cod$NumOfCoding)

# visualizar infos sobre cada codificacao
coding_table <- getCodingTable()

#================================#
# Analise dos temas em debate    #
#================================# 

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
  geom_label(aes(x = nomes_temas, y = prop_tema, label = prop_tema2), size = 3.3)+
  labs(y = "Procentagem do Total", x = "", title = "") +
  tema_massa()+
  coord_flip()+
  ggsave("prop_debate_tema.png", path = "outputs", width = 7, height = 3, units = "in")

#======================================#
#     ANALISE DE VOZ NOS DEBATES       #
#======================================#

#==== manipulacao dos dados ====#

# selecionar codigos dos representantes
paste_voz<- c("cat_", "tema_", "DESTAQUES", "DUVIDA_", "atua_", "DECISOES", "termo_", "tema2", "IDENT")
cont_cod_data <- mutate(cont_cod_data, select_voz = 1)
cont_cod_data$select_voz[str_detect(cont_cod_data$Var1, paste(paste_voz, collapse = '|'))] <- 2
codes_represent <- cont_cod_data[cont_cod_data$select_voz == 1,]

#---- match nomes dos representantes ----#

# padronizars nomes para mergir
codes_represent$Var1 <- str_replace_all(codes_represent$Var1, "_", " ") %>%
                        stri_trans_general("Latin-ASCII")
codes_represent$nome_consel <- as.character(codes_represent$Var1)

consel_insti$nome_consel <- stri_trans_general(consel_insti$nome_consel , "Latin-ASCII")

# mergir base de conselheiros # 
data_consel <- merge(consel_insti, codes_represent, by = "nome_consel") ### Inserir "ALL = T" PARA Analisar Prop.##

# criar e salvar base de representantes nao-conselheiros
data_rep_nconsel <- codes_represent[str_detect(codes_represent$nome_consel, "1"),]
#write.csv(data_rep_nconsel, file = "Dados/data_rep_nconsel2.csv")

#---- megir bases representantes ----#
base_representantes <- rbind(represent_nao_consel, data_consel[,-c(5,7, 8)])
#write.csv(base_representantes, file = "Dados/base_representantes_voz.csv")

#==== contagem das falas ====#

# contar
count_cat1 <- aggregate(base_representantes$Freq, by=list(Category=base_representantes$categoria1), FUN=sum)

# sem os gestores
base_rep_sem_icmbio <- base_representantes[base_representantes$entidade_sigla != "ICMBIO",]
count_cat1 <- aggregate(base_rep_sem_icmbio$Freq, by=list(Category=base_rep_sem_icmbio$categoria1), FUN=sum)

# transformar em prop 
count_cat1 <- mutate(count_cat1, prop_cat1 = (x / sum(x))*100 )

# renomear categoria 6
count_cat1$Category[6] <- "Organizações de educação, cultura  \n  e associações comunitárias"

# ordenar
count_cat1$Category <- factor(count_cat1$Category, 
                              levels = count_cat1$Category[order(count_cat1$prop_cat1)])

count_cat1$prop_cat1.2 <- paste(round(count_cat1$prop_cat1, 2), "%", sep="")

count_cat1$categoria_inst <- c("Sociedade Civil", "Sociedade Civil","Poder Público", "Poder Público",
                               "Sociedade Civil", "Sociedade Civil")

# ggplot2
ggplot(count_cat1, aes(x = Category, y = prop_cat1))+
  geom_bar(stat = "identity", aes(fill = count_cat1$categoria_ins)) +
  scale_fill_manual("Categoria",values=c("#15041c", "lightgreen"))+
  geom_label(aes(x = Category, y = prop_cat1, label = prop_cat1.2), size = 2.5)+
  labs(y = "Porcentagem do Total", x = "", title = "") +
  coord_flip()+
  tema_massa()%+replace% 
  theme(legend.position="bottom")+
  ggsave("prop_voz_cat.png", path = "outputs", width = 8, height = 3, units = "in")

#==== Proporcional ao Numero de Assentos ====#

# Contagem de assentos
cont_assento <- data.frame(table(consel_insti$categoria1)) 

# mergir bases
cont_assento$Category <- as.character(cont_assento$Var1)
cont_rep_prop <- merge(cont_assento, count_cat1, by = "Category")

# FAZER PROP
cont_rep_prop$prop <- cont_rep_prop$x / cont_rep_prop$Freq

#=====================#
# Genero              #
#=====================#

# frequencia por assento
assento_freq_sex <- data.frame(table(apacc_atas$sexo))
assento_freq_sex <- assento_freq_sex[-c(2,4),]
assento_freq_sex <- mutate(assento_freq_sex, prop = round((Freq / sum(Freq))*100, 1) )
assento_freq_sex$cat <- "Assento"

# frequencia por presenca
presen_freq_sex <- apacc_atas[apacc_atas$presente == 1,]
presen_freq_sex <- data.frame(table(presen_freq_sex$sexo))
presen_freq_sex <- presen_freq_sex[-c(2,4),]
presen_freq_sex <- mutate(presen_freq_sex, prop = round((Freq / sum(Freq))*100, 1) )
presen_freq_sex$cat <- "Presença"

# frequencia por voz
voz_freq_sex <- aggregate(apacc_voz$Freq, by=list(Var1=apacc_voz$sexo), FUN=sum)
colnames(voz_freq_sex)[2] <- "Freq"
voz_freq_sex <- mutate(voz_freq_sex, prop = round((Freq / sum(Freq))*100, 1) )
voz_freq_sex$cat <- "Voz"

# combinar dados e definr label
freq_genero <- rbind(assento_freq_sex, presen_freq_sex, voz_freq_sex)
freq_genero$Var1 <- factor(freq_genero$Var1, levels = c("1", "2"), labels = c("Homem", "Mulher"))
freq_genero$proplab <- paste(freq_genero$prop, "%", sep="")

# ggplot
ggplot(freq_genero, aes(x = cat, y = prop))+
  geom_col(aes(fill = Var1))+
  geom_label(aes(label = proplab), size = 3.2)+
  scale_fill_manual("Sexo" ,values=c("Homem" = "#15041c", "Mulher" =  "lightgreen"))+
  labs(x ="", y = "Porcent. de Representantes") +
  coord_flip()
ggsave("Resultados/barapacc_sex.png", width = 7, height =2, units = "in")









