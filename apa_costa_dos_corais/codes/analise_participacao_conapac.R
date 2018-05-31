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
 
# carregar bases
apacc_atas <- read_excel("data/Listas Presença Conselho APACC.xlsx")
apacc_voz <- read_excel("data/base_representantes_voz.xlsx")

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


#======================================#
# INSTITUICOES CAT 1                  
# Poder Publico X Sociedade Civil

# frequencia por assento
assento_freq_cat1 <- data.frame(table(apacc_atas$entidade_sigla))
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
ggsave("Resultados/barapacc_sex.png", width = 7, height =3, units = "in")



#======================================#
#     ANALISE DE VOZ NOS DEBATES       #
#======================================#

# selecionar codigos dos representantes
paste_voz<- c("cat_", "tema_", "DESTAQUES", "DUVIDA_", "atua_", "DECISOES", "termo_")
cont_cod_data <- mutate(cont_cod_data, select_voz = 1)
cont_cod_data$select_voz[str_detect(cont_cod_data$Var1, paste(paste_voz, collapse = '|'))] <- 2

codes_represent <- cont_cod_data[cont_cod_data$select_voz == 1,]

# importar base de instituicoes por representante
insti_categorias <- read_excel("data/instituições_apacc_2.0.xlsx")

#==== match nomes dos representantes =====#

# limpar bases
library(stringi)
codes_represent$Var1 <- str_replace_all(codes_represent$Var1, "_", " ")
codes_represent$Var1 <- stri_trans_general(codes_represent$Var1 , "Latin-ASCII")
codes_represent$nome_consel <- as.character(codes_represent$Var1)

insti_categorias$nome_consel <- stri_trans_general(insti_categorias$nome_consel , "Latin-ASCII")

# mergir base de conselheiros # 
data_consel <- merge(insti_categorias, codes_represent, by = "nome_consel", all = T) ### Inserir "ALL = T" PARA Analisar Prop.##

# criar e salvar base de representantes n-conselheiros
data_rep_nconsel <- codes_represent[str_detect(codes_represent$nome_consel, "1"),]
#write.csv(data_rep_nconsel, file = "Dados/data_rep_nconsel.csv")

# importar base editada manulamente
data_rep_nconsel <- read_excel("Dados/intituições_apacc3.xlsx")

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









