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
apacc_atas <- read_excel("Dados/Listas Presença Conselho APACC.xlsx")
apacc_voz <- read_excel("Dados/base_representantes_voz.xlsx")

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










