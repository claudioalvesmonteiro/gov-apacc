#====================================================================#
# PARTICIPAÇÃO SOCIAL NA GESTÃO DO MEIO AMBIENTE: 
# RESEX Acaú-Goiana e APA Costa dos Corais em Perspectiva Comparada
#====================================================================#
# ANALISE DE CONTEUDO DAS MEMORIAS               
# Recife - Pernambuco - Brasil                   
#--------------------------------------------------------------------#
# Fundacao Joaquim Nabuco (FUNDAJ)               
#--------------------------------------------------------------------#
# Claudio A. Monteiro                            
# claudiomonteirol.a@gmail.com                   
#====================================================================#

# carregar pacotes
library(readr); library(ggpubr)

#===== template dos graficos =====#

tema_massa <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour= "black",size=8,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=9,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
}


# carregar dados
apa_fala_data <- read_csv("resultados/tabelas/apa_fala_data.csv")
resex_fala_data <- read_csv("resultados/tabelas/resex_fala_data.csv")

#================================#
#      PROPORCAO DO TOTAL  
#================================#

#==== grafico RESEX ====#

# ordenar
resex_fala_data$grupo_setorial <- factor(resex_fala_data$grupo_setorial, 
                                         levels = resex_fala_data$grupo_setorial[order(resex_fala_data$proporcao_total)])

# grafico
RESEXtotal <- ggplot(resex_fala_data, aes(x = grupo_setorial, y = proporcao_total))+
  geom_bar(stat = "identity", aes(fill = resex_fala_data$grupo_setorial2)) +
  scale_fill_manual("Categoria",values=c("#15041c", "lightgreen"))+
  geom_label(aes(label = proporcao_total_label), size = 3.2)+
  labs(y = "Porcentagem do Total", x = "", title = "RESEX Acaú-Goiana") +
  coord_flip()+
  tema_massa()%+replace% 
  theme(legend.position="bottom")

#==== grafico APA ====#

# ordenar
apa_fala_data$grupo_setorial <- factor(apa_fala_data$grupo_setorial, 
                                       levels = apa_fala_data$grupo_setorial[order(apa_fala_data$proporcao_total)])
# grafico
APAtotal<- ggplot(apa_fala_data, aes(x = grupo_setorial, y = proporcao_total))+
  geom_bar(stat = "identity", aes(fill = apa_fala_data$categoria_inst)) +
  scale_fill_manual("Categoria",values=c("#15041c", "lightgreen"))+
  geom_label(aes(label = proporcao_total_label), size = 3.2)+
  labs(y = "Porcentagem do Total", x = "", title = "APA Costa dos Corais") +
  coord_flip()+
  tema_massa()%+replace% 
  theme(legend.position="bottom")

#==== combinar graficos ====#
ggarrange(RESEXtotal, APAtotal, ncol = 1, nrow = 2, common.legend = T, legend = "bottom")
ggsave("COMPARADA_fala_total.png", path = "resultados/graficos",width = 8, height = 5, units = "in")

#===========================================#
#      PROPORCAO AO NUMERO DE ASSENTOS
#===========================================#

#==== grafico RESEX ====#

# ordenar
resex_fala_data$grupo_setorial <- factor(resex_fala_data$grupo_setorial, 
                                         levels = resex_fala_data$grupo_setorial[order(resex_fala_data$prop_assento)])

# grafico
RESEXtotal2 <- ggplot(resex_fala_data, aes(x = grupo_setorial, y = prop_assento))+
  geom_bar(stat = "identity", aes(fill = resex_fala_data$grupo_setorial2)) +
  scale_fill_manual("Categoria",values=c("#15041c", "lightgreen"))+
  geom_label(aes(label = prop_assento), size = 3.2)+
  labs(y = "Porcentagem do Total", x = "", title = "RESEX Acaú-Goiana") +
  coord_flip()+
  tema_massa()%+replace% 
  theme(legend.position="bottom")

#==== grafico APA ====#

# substituir NA por 0
apa_fala_data$prop_assento[is.na(apa_fala_data$prop_assento)] <- 0

# ordenar
apa_fala_data$grupo_setorial <- factor(apa_fala_data$grupo_setorial, 
                                       levels = apa_fala_data$grupo_setorial[order(apa_fala_data$prop_assento)])
# grafico
APAtotal2 <- ggplot(apa_fala_data, aes(x = grupo_setorial, y = prop_assento))+
  geom_bar(stat = "identity", aes(fill = apa_fala_data$categoria_inst)) +
  scale_fill_manual("Categoria",values=c("#15041c", "lightgreen"))+
  geom_label(aes(label = prop_assento), size = 3.2)+
  labs(y = "Porcentagem do Total", x = "", title = "APA Costa dos Corais") +
  coord_flip()+
  tema_massa()%+replace% 
  theme(legend.position="bottom")

#==== combinar graficos ====#
ggarrange(RESEXtotal2, APAtotal2, ncol = 1, nrow = 2, common.legend = T, legend = "bottom")
ggsave("COMPARADA_fala_prop_assento.png", path = "resultados/graficos",width = 8, height = 5, units = "in")

