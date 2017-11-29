s#------------------------------------------------#
# GOVERNING A COMMON COAST                       #
#------------------------------------------------#
# Recife - Pernambuco - Brasil                   #
#------------------------------------------------#
# Funda??o Joaquim Nabuco (FUNDAJ)               #
#------------------------------------------------#
# Claudio A. Monteiro                            #
# claudiomonteirol.a@gmail.com                   #
#------------------------------------------------#
# Any question contact the developer             #
# #UseFreeSoftware                               #
#------------------------------------------------#

# install.packages
# install.packages(c("readxl", 'ggplot2'))

# load packages
library(readxl)
library(ggplot2)

#
setwd("~/Research/Governing The Coast (TCC)/Replication Documentation/")
#setwd("~/Documents/Claudio/R/")

# read data
apacc_data <- read_excel("~/Research/Governing The Coast (TCC)/Replication Documentation/Analysis Data/Listas Presença Conselho APACC.xlsx", 
                     col_types = c("text", "date", "numeric", 
                                   "text", "text", "numeric", "numeric", 
                                   "text", "numeric"))
library(ggplot2)

theme_arretado<- function (base_size = 12, base_family = "") {
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

#--------------------------------#
# Sex representnation analysis

# count men woman
sex_apacc <- as.data.frame(table(apacc_data$sexo))

# label factor
sex_apacc$Sexo <- factor(sex_apacc$Var1, levels = c(1, 2), labels = c("Homem", "Mulher")) 

# sex plotting 1 #
bar_sex_apacc <- ggplot(sex_apacc, aes(x = Sexo, y = Freq))+
  geom_bar(stat = "identity", aes(fill = Sexo)) +
  labs( x ="Sexo", y = "NÃºmero de Representantes") +
  scale_fill_manual("Sexo", values = c("Homem" = "#325c6c", "Mulher" =  "lightgreen"))+
  geom_label(aes(y = 200,label = sex_apacc$Freq)) +
  theme_arretado()+
  guides(fill = F)
bar_sex_apacc

#ggsave("barapacc_sex.png", bar_sex_apacc, width = 4, height =5.5, units = "in")

#----------------#
# Institutions 
#----------------#

apacc_data$entidade_sigla <- toupper(apacc_data$entidade_sigla)
apacc_data$entidade_sigla <- (as.factor(apacc_data$entidade_sigla))

#==== based on total count ====#

# find institutions appearing in the cousel
represent_insti <- data.frame(table(apacc_data$entidade_sigla))

represent_insti <- represent_insti[order(-represent_insti$Freq),]

library(xlsx)
write.xlsx(represent_insti, file = "instituições_apacc.xlsx")

represent_insti$Sigla <- factor(represent_insti$Var1, levels = represent_insti$Var1)


barra_inst <- ggplot(represent_insti, aes(x = represent_insti$Sigla, y = represent_insti$Freq))+
  geom_bar(stat = "identity", fill = "#2c3b3e") +
  labs(x = "", y = "Número de Conselheiros", title = "Conselheiros Totais") +
  geom_label(label = represent_insti$Freq,
             size = 2.5, color = "black", fontface = "plain") +
  theme_arretado()+
  coord_flip()
barra_inst

ggsave("total_inst.png", barra_inst, width = 6, height = 12, units = "in")

#==== based on presence ====#

inst_presen <- apacc_data[apacc_data$presente == 1,]

# find institutions present in the cousel
inst_presen <- data.frame(table(inst_presen$entidade_sigla))

inst_presen <- inst_presen[order(inst_presen$Freq),]

inst_presen$Sigla <- factor(inst_presen$Var1, levels = inst_presen$Var1)

barra_inst2 <- ggplot(inst_presen, aes(x = inst_presen$Sigla, y = inst_presen$Freq))+
  geom_bar(stat = "identity", fill = "#2c3b3e") +
  scale_y_continuous(limits = c(0,50))+
  labs(x = "", y = "Número de Conselheiros", title = "Conselheiros Presentes") +
  geom_label(label = inst_presen$Freq,
             size = 2.5, color = "black", fontface = "plain") +
  theme_arretado()+
  coord_flip()
barra_inst2
ggsave("presen_inst.png", barra_inst2, width = 6, height = 12, units = "in")

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
  labs(x = "", y = "Número de Conselheiros", title = "Conselheiros Titulares") +
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

times_represent$Reunião <- factor(times_represent$Var1, 
                                  levels =c("1_posse", "reuniao_2", "reuniao_3",  "reuniao_4",  "reuniao_5",  "reuniao_6",  "reuniao_7", 
                                            "reuniao_8",  "reuniao_9", "reuniao_10", "reuniao_11", "reuniao_12", "reuniao_13", "reuniao_14", 
                                            'reuniao_15', "reuniao_16", "reuniao_17", "reuniao_18", "reuniao_19",   "reuniao_20", "reuniao_21",
                                           "reuniao_22", "reuniao_23"))

times_represent <- times_represent[order(times_represent$Reunião),]

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


ipa_plot1 <- ggplot(data = times_represent, aes(x = Reunião, y = times_represent$Freq, group = 1))+
  geom_line(size = 1, colour = "#2c3b3e")+
  scale_y_continuous(limits= c(0, 100))+
  labs(x = "", title = "",y = "NÃºmero de Representantes")+
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
  labs(x = "", y = "Número de Representantes", title = "") +
  geom_label(label = represent_city$Freq,
             size = 4, color = "black", fontface = "plain") +
  theme_arretado()+
  coord_flip()
barra_inst1
ggsave("repre_city.png", barra_inst1, width = 8, height = 5, units = "in")


