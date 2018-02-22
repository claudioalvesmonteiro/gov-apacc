#------------------------------------------------#
# ASSEMBLEIA NO ATLANTICO BRASILEIRO             #
#------------------------------------------------#
# Recife - Pernambuco - Brasil                   #
#------------------------------------------------#
# Fundacao Joaquim Nabuco (FUNDAJ)               #
#------------------------------------------------#
# Claudio A. Monteiro                            #
# claudiomonteirol.a@gmail.com                   #
#------------------------------------------------#
# Qualquer duvida contate o desenvolvedor        #
# #UseSoftwareLivre                              #
#------------------------------------------------#

# instalar pacotes
# install.packages(c("readxl", 'ggplot2'), dependencies = T) # isso pode levar um certo tempo

# carregar pacotes
library(readxl); library(ggplot2)

# tema para os graficos em ggplot
theme_arretado<- function(base_size = 12, base_family = "") {
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

#==============================#
# Representacao por genero

# selecionar apenas consel. presentes
sex_apacc1 <- apacc_data[apacc_data$presente == 1,]
  
# count men woman
sex_apacc1 <- as.data.frame(table(sex_apacc1$sexo))
sex_apacc2 <- as.data.frame(table(apacc_data$sexo))

# label factor
sex_apacc1$Sexo <- factor(sex_apacc1$Var1, levels = c(1, 2), labels = c("Homem", "Mulher")) 
sex_apacc2$Sexo <- factor(sex_apacc2$Var1, levels = c(1, 2), labels = c("Homem", "Mulher")) 

# sex plotting 1 #
ggplot(sex_apacc2, aes(x = Sexo, y = Freq))+
  geom_bar(stat = "identity", aes(fill = Sexo)) +
  labs( x ="Sexo", y = "Prop. de Consel. Presentes") +
  scale_fill_manual("Sexo", values = c("Homem" = "#325c6c", "Mulher" =  "lightgreen"))+
  geom_label(aes(y = 200,label = sex_apacc2$Freq)) +
  theme_arretado()+
  guides(fill = F)

ggsave("barapacc_sex.png", width = 4, height =5.5, units = "in")

#=====================================#
#      ANALISE DAS INSTITUICOES       #
#=====================================#

# ler bancos
apacc_data <- read_excel("Dados/Listas Presença Conselho APACC.xlsx")
insti_categorias <- read_excel("Dados/instituições_apacc_2.0.xlsx")

# transformar em caixa alta e fator
apacc_data$entidade_sigla <- as.factor(toupper(apacc_data$entidade_sigla))

# selecionar categorias e instituicoes
insti_categorias <- insti_categorias[!duplicated(insti_categorias$entidade_sigla),]


#====== REPRESENTACAO TOTAL =======#

# contagem geral das instituicoes
represent_insti <- data.frame(table(apacc_data$entidade_sigla))

# mergir com categorias
represent_insti$entidade_sigla <- represent_insti$Var1
represent_cat <- merge(represent_insti, insti_categorias, by = "entidade_sigla")

#==== somar representacao com base na categoria 1 ====#
rep_total_cat1 <- aggregate(represent_cat$Freq, by=list(Category=represent_cat$categoria1), FUN=sum)

# ordernar valores 
rep_total_cat1 <- rep_total_cat1[order(rep_total_cat1$x),]
rep_total_cat1$Category <- factor(rep_total_cat1$Category, levels = rep_total_cat1$Category)

# grafico
ggplot(rep_total_cat1, aes(x = rep_total_cat1$Category, y = rep_total_cat1$x))+
  geom_bar(stat = "identity", fill = "#2c3b3e") +
  labs(x = "", y = "Número de Conselheiros(as)", title = "Representação por Categoria 1") +
  geom_label(label = rep_total_cat1$x,size = 2.5, color = "black", fontface = "plain") +
  theme_arretado()+
  coord_flip()
ggsave("Resultados/barra_inst_cat1.png", width = 10, height = 4, units = "in")

#==== somar representacao com base na categoria 2 ====#
rep_total_cat2 <- aggregate(represent_cat$Freq, by=list(Category=represent_cat$categoria2), FUN=sum)

# ordernar valores 
rep_total_cat2 <- rep_total_cat2[order(rep_total_cat2$x),]
rep_total_cat2$Category <- factor(rep_total_cat2$Category, levels = rep_total_cat2$Category)

# grafico
ggplot(rep_total_cat2, aes(x = rep_total_cat2$Category, y = rep_total_cat2$x))+
  geom_bar(stat = "identity", fill = "#2c3b3e") +
  labs(x = "", y = "Número de Conselheiros(as)", title = "Representação por Categoria 1") +
  geom_label(label = rep_total_cat2$x,size = 2.5, color = "black", fontface = "plain") +
  theme_arretado()+
  coord_flip()
ggsave("Resultados/barra_inst_cat2.png", width = 10, height = 4, units = "in")


#====== REPRESENTACAO POR PRESENCA =======#

# contagem das instituicoes presentes
presente_insti <- apacc_data[apacc_data$presente ==1,]
presente_insti <- data.frame(table(presente_insti$entidade_sigla))

# mergir com categorias
presente_insti$entidade_sigla <- presente_insti$Var1
presenca_cat <- merge(presente_insti, insti_categorias, by = "entidade_sigla")

#==== somar presenca com base na categoria 1 ====#
presenca_cat1 <- aggregate(presenca_cat$Freq, by=list(Category=presenca_cat$categoria1), FUN=sum)

# ordernar valores 
presenca_cat1 <- presenca_cat1[order(presenca_cat1$x),]
presenca_cat1$Category <- factor(presenca_cat1$Category, levels = presenca_cat1$Category)

# grafico
ggplot(presenca_cat1, aes(x = presenca_cat1$Category, y = presenca_cat1$x))+
  geom_bar(stat = "identity", fill = "#2c3b3e") +
  labs(x = "", y = "Número de Conselheiros(as)", title = "Presença nas Reuniões por Categoria 1") +
  geom_label(label = presenca_cat1$x,size = 2.5, color = "black", fontface = "plain") +
  theme_arretado()+
  coord_flip()
ggsave("Resultados/barra_pres_insti_cat1.png", width = 10, height = 4, units = "in")

#==== somar representacao com base na categoria 2 ====#
presenca_cat2 <- aggregate(presenca_cat$Freq, by=list(Category=presenca_cat$categoria2), FUN=sum)

# ordernar valores 
presenca_cat2 <- presenca_cat2[order(presenca_cat2$x),]
presenca_cat2$Category <- factor(presenca_cat2$Category, levels = presenca_cat2$Category)

# grafico
ggplot(presenca_cat2, aes(x = presenca_cat2$Category, y = presenca_cat2$x))+
  geom_bar(stat = "identity", fill = "#2c3b3e") +
  labs(x = "", y = "Número de Conselheiros(as)", title = "Representação por Categoria 1") +
  geom_label(label = presenca_cat2$x,size = 2.5, color = "black", fontface = "plain") +
  theme_arretado()+
  coord_flip()
ggsave("Resultados/barra_inst_cat2.png", width = 10, height = 4, units = "in")
#==== based on titularity ====#

# select cases
inst_titu <- apacc_data[apacc_data$condicao == 1,]

# find institutions present in the cousel
inst_titu <- data.frame(table(inst_titu$entidade_sigla))

# order data
inst_titu <- inst_titu[order(inst_titu$Freq),]

# define levels
inst_titu$Sigla <- factor(inst_titu$Var1, levels = inst_titu$Var1)

barra_inst3 <- ggplot(inst_titu, aes(x = inst_titu$Sigla, y = inst_titu$Freq))+
  geom_bar(stat = "identity", fill = "#2c3b3e") +
  scale_y_continuous(limits = c(0,50))+
  labs(x = "", y = "NÃºmero de Conselheiros", title = "Conselheiros Titulares") +
  geom_label(label = inst_titu$Freq,
             size = 2.5, color = "black", fontface = "plain") +
  theme_arretado()+
  coord_flip()
barra_inst3
ggsave("titularity_inst.png", barra_inst3, width = 6, height = 12, units = "in")

#----- arrange plots ----#
library(ggpubr)
grid_barrainst <- ggarrange(barra_inst, barra_inst2, barra_inst3, ncol = 3, nrow = 1)

ggsave("grid_barrainst.png",grid_barrainst,  width = 11, height = 12)



#===== Time Series =====#

times_represent <- data.frame(table(apacc_data$reuniao))

times_represent$ReuniÃ£o <- factor(times_represent$Var1, 
                                  levels =c("1_posse", "reuniao_2", "reuniao_3",  "reuniao_4",  "reuniao_5",  "reuniao_6",  "reuniao_7", 
                                            "reuniao_8",  "reuniao_9", "reuniao_10", "reuniao_11", "reuniao_12", "reuniao_13", "reuniao_14", 
                                            'reuniao_15', "reuniao_16", "reuniao_17", "reuniao_18", "reuniao_19",   "reuniao_20", "reuniao_21",
                                           "reuniao_22", "reuniao_23"))

times_represent <- times_represent[order(times_represent$ReuniÃ£o),]

theme_arretado<- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour= "black",size=12,angle=80, hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=13,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=13,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
}


ipa_plot1 <- ggplot(data = times_represent, aes(x = ReuniÃ£o, y = times_represent$Freq, group = 1))+
  geom_line(size = 1, colour = "#2c3b3e")+
  scale_y_continuous(limits= c(0, 100))+
  labs(x = "", title = "",y = "NÃƒÂºmero de Representantes")+
  geom_text(aes( angle = 20 , vjust = -0.9, hjust = 0.3, fontface = "plain"), label = times_represent$Freq, size = 4)+
  theme_arretado() 
ipa_plot1

ggsave("line_represent_total.png",ipa_plot1,  width = 10, height = 5)


#===== Cities =====#

# count cities
levels(as.factor(apacc_data$municipio))

# find institutions appearing in the cousel
represent_city <- data.frame(table(apacc_data$municipio))

represent_city <- represent_city[order(represent_city$Freq),]
represent_city$Municipio <- toupper(represent_city$Var1)

#---- 30 mais representados ----#

represent_city$Municipio <- factor(represent_city$Municipio, levels = represent_city$Municipio)

barra_inst1 <- ggplot(represent_city, aes(x = represent_city$Municipio, y = represent_city$Freq))+
  geom_bar(stat = "identity", fill = "#2c3b3e") +
  #scale_y_continuous(limits = c(0,50))+
  labs(x = "", y = "NÃºmero de Representantes", title = "") +
  geom_label(label = represent_city$Freq,
             size = 4, color = "black", fontface = "plain") +
  theme_arretado()+
  coord_flip()
barra_inst1
ggsave("repre_city.png", barra_inst1, width = 8, height = 5, units = "in")


#================================================#
# PROP. DE FALA -  MANIPULACAO DE DADOS
#================================================#

consel_insti <- apacc_data[!duplicated(apacc_data$nome_consel),c(4:6)]

write.csv(consel_insti, file = "Resultados/consel_instituicoes.csv")

#* Apos salvar essa banco, foi feita uma categorizacao das instituicoes com base na reuniao 18,
#* em que houve eleicoes.












