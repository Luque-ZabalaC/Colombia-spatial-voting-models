rm(list=ls()) 


library(pscl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(rstan)

load("~/data0.Rda")
load("~/fit0_L1.Rda")
est <- summary(stan.fit)$summary
rm(stan.fit)
s2010 <- readRDS("~/s2010.rds")
s2010_legis_data <- s2010[["legis.data"]]
s2010_vote_data <- s2010[["vote.data"]]
rm(s2010)



#############################################################################################

# -----------------------------------------
# behavior of the estimated ideal points.

# estimated ideal points.
BetaM <- est[grep("xi", row.names(est)),1][90:180]
# credibility interval
BetaQ <- est[grep("xi", row.names(est)),c(4,8)][90:180,]

Beta <- as.data.frame(cbind(BetaM, BetaQ))
dim(Beta)

leg_group <- s2010_votes %>% select(legislador, grupo,id_legis)%>% 
  distinct(legislador, grupo, id_legis) %>% 
  arrange(id_legis)

Beta$legislador <- leg_group$legislador
row.names(Beta) <- NULL
colnames(Beta)[1:3] <- c("Mean", "Lower", "Upper")

# merge "Beta" y "s2010_legis_data"
Beta <- merge(Beta, s2010_legis_data, by="legislador")
Beta <- Beta[order(Beta$Mean),]
Beta <- Beta %>% rename(group=grupo, legislator=legislador)


# Ideal Point.
windows()
Y <- seq(from=1, to=length(Beta$Mean), by=1)
ggplot(Beta, aes(x=Mean, y=Y)) +
  geom_point(aes(colour=group), shape=19, size=3) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper,colour = group),height = 0) +
  geom_text(aes(x = Upper, label = legislator, colour = group), 
            size = 2.3, 
            hjust = -.05) +
  scale_colour_manual(NULL, values = c("red", "chartreuse4", "blue", "goldenrod2"),
                      labels = c("Coalici贸n", "Independientes", "Minor铆as", "Oposici贸n")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 1,colour = "grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = .4),
        axis.text.x = element_text(colour="black", face="bold", size=8),
        legend.text = element_text (color = "black", size = 9))+
  scale_x_continuous(limits = c(-2.1, 3)) 


# party
windows()
YP <- round(Beta$Mean,2)
Beta$Party_o <- factor(Beta$partido, levels=c('CC','CR','LC','PU',"PIN","AICO","ASI","MIRA","PAV","PDA"))
ggplot(Beta, aes(x=Mean, y=YP)) +
  geom_point(size = 3, aes(colour = Party_o)) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, colour = Party_o), height = 0) +
  facet_grid(Party_o ~ .,scales = "free_y") +
  scale_colour_manual("Partido",values = c("red", "red", "red","red","chartreuse4","blue",
                                           "blue","chartreuse4", "chartreuse4", "goldenrod2"),
                      labels=c("Conservador (CC)","Cambio Radical (CR)","Liberal (LC)", "Social de Unidad Nacional (PU)",
                               "Integraci贸n Nacional (PIN)","AICO","ASI","MIRA","Alianza Verde (PAV)","Polo Democr谩tico (PDA)" 
                      )) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.title = element_blank(), 
        legend.position = "right",
        legend.direction = "vertical",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 1,colour = "grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = .4),
        axis.text.x = element_text(colour="black", face="bold", size=8),
        legend.text = element_text (color = "black", size = 9),
        legend.title = element_text (color = "black", size = 10, face="bold")) + 
  ylab(NULL) +
  xlab(NULL)


# commission
windows()
YC <- round(Beta$Mean,2)
Beta$comision[Beta$comision=="primera"]<-"Primera"
Beta$comision[Beta$comision=="segunda"]<-"Segunda"
Beta$comision[Beta$comision=="tercera"]<-"Tercera"
Beta$comision[Beta$comision=="cuarta"]<-"Cuarta"
Beta$comision[Beta$comision=="quinta"]<-"Quinta"
Beta$comision[Beta$comision=="sexta"]<-"Sexta"
Beta$comision[Beta$comision=="septima"]<-"S茅ptima"
Beta$comision_o <- factor(Beta$comision, levels=c("Primera","Segunda","Tercera","Cuarta","Quinta","Sexta","S茅ptima"))
ggplot(Beta, aes(x=Mean, y=YC)) +
  geom_point(size = 3, aes(colour = comision_o)) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, colour = comision_o),
                 height = 0) +
  facet_grid(comision_o ~ ., scales = "free_y") +
  scale_colour_manual("Tema de la comisi贸n",values = c("blue4", "black", "red","blue", "darkgreen","chocolate3","darkorchid"),
                      labels=c("Reformas constituci贸n y derechos fundamentales",
                               "Relaciones internacionales y seguridad nacional",
                               "Impuestos y cr茅dito p煤blico",
                               "Presupuesto de la naci贸n",
                               "Agro y medio ambiente",
                               "Servicios p煤blicos, educaci贸n y cultura",
                               "Salud y vivienda")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.title = element_blank(), 
        legend.position = "right",
        legend.direction = "vertical",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 1,colour = "grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = .4),
        axis.text.x = element_text(colour="black", face="bold", size=8),
        legend.text = element_text (color = "black", size = 9),
        legend.title = element_text (color = "black", size = 10, face="bold"))+ 
  ylab(NULL)+
  xlab(NULL)


#############################################################################################

# -----------------------------------------
# Behavior of discrimination parameters

AlphaM <- est[grep("alpha", row.names(est)),1]

# credibility interval
AlphaQ <- est[grep("alpha", row.names(est)),c(4,8)]

Alpha <- as.data.frame(cbind(AlphaM, AlphaQ))
row.names(Alpha) <- NULL
colnames(Alpha)[1:3] <- c("Mean", "Lower", "Upper")
dim(Alpha)

id_alpha <- s2010_votes %>% select(id_votacion, id_list) %>% distinct(id_votacion,id_list)
Alpha <- as.data.frame(cbind(id_alpha, Alpha))
Alpha$id_votacion <- trimws(Alpha$id_votacion)

# merge "Alpha" y "s2010_vote_data"
Alpha <- merge(Alpha, s2010_vote_data, by="id_votacion")
#Alpha <- Alpha[order(Alpha$Mean),]
#head(Alpha)

Alpha <- Alpha %>% mutate(distin = (Upper>0 & Lower>0 | Upper<0 & Lower<0),
                          ypc = (si/(no+si))*100,
                          miss= ((abstenciones+inasistencia)/91)*100)

round((sum(Alpha$distin)/417)*100,1)

Alpha_dis <- Alpha %>% filter(distin == "TRUE")

Alpha_disNO <- Alpha %>% filter(distin == "FALSE")

table(Alpha$tema_principal, Alpha$distin)
table(Alpha$iniciativa, Alpha$distin)
table(Alpha$legislatura, Alpha$distin)
table(Alpha$tipo_votacion, Alpha$distin)

windows()
plot(Alpha_dis$ypc, Alpha_dis$Mean)
min(ypc)
max(ypc)

windows()
plot(Alpha_disNO$ypc, Alpha_disNO$Mean)
min(ypcNO)
max(ypcNO)
sum(Alpha_disNO$ypc==0) + sum(Alpha_disNO$ypc==100)


#########
## ggplot 

Dis <- Alpha_dis %>% select(ypc, Mean)
windows()
ggplot(Dis, aes(x = ypc, y = Mean)) +
  geom_point() +
  xlab("Porcentaje de votos a favor") +
  ylab("Estimaci髇 puntual alpha") +
  geom_vline(xintercept = 2.5, col="gray34", lty=2) +
  geom_vline(xintercept = 97.5, col="gray34", lty=2) +
  scale_y_continuous(limits = c(-10, 10), breaks=seq(-10, 10, 1)) +
  scale_x_continuous(limits = c(0, 100), breaks=seq(0, 100, 10)) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=12),
        axis.text.y = element_text(colour="black", face="bold", size=12),
        axis.title = element_text(face="bold", colour="black", size=rel(1.3)))


DisNO <- Alpha_disNO %>% select(ypc, Mean)
windows()
ggplot(DisNO, aes(x = ypc, y = Mean)) +
  geom_point() +
  xlab("Porcentaje de votos a favor") +
  ylab("Estimaci髇 puntual alpha") +
  geom_vline(xintercept = 2.5, col="gray34", lty=2) +
  geom_vline(xintercept = 97.5, col="gray34", lty=2) +
  scale_y_continuous(limits = c(-10, 10), breaks=seq(-10, 10, 1)) +
  scale_x_continuous(limits = c(0, 100), breaks=seq(0, 100, 10)) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=12),
        axis.text.y = element_text(colour="black", face="bold", size=12),
        axis.title = element_text(face="bold", colour="black", size=rel(1.3)))

Dis_miss <- Alpha_dis %>% select(Mean, miss)
windows()
ggplot(Dis_miss, aes(x = miss, y = Mean)) +
  geom_point() +
  xlab("Porcentaje de datos perdidos") +
  ylab("Estimaci髇 puntual alpha") +
  scale_y_continuous(limits = c(-10, 10), breaks=seq(-10, 10, 1)) +
  scale_x_continuous(limits = c(15, 60), breaks=seq(15, 60, 10)) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=12),
        axis.text.y = element_text(colour="black", face="bold", size=12),
        axis.title = element_text(face="bold", colour="black", size=rel(1.3)))


DisNO_miss <- Alpha_disNO %>% filter(ypc!=0 & ypc!=100) %>% select(Mean, miss)
windows()
ggplot(DisNO_miss, aes(x = miss, y = Mean)) +
  geom_point() +
  xlab("Porcentaje de datos perdidos") +
  ylab("Estimaci髇 puntual alpha") +
  scale_y_continuous(limits = c(-10, 10), breaks=seq(-10, 10, 1)) +
  scale_x_continuous(limits = c(25, 75), breaks=seq(25, 75, 10)) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=12),
        axis.text.y = element_text(colour="black", face="bold", size=12),
        axis.title = element_text(face="bold", colour="black", size=rel(1.3)))



#############################################################################################

# -----------------------------------------
# behavior of approval parameters
MuM <- est[grep("mu", row.names(est)),1]

# credibility interval
MuQ <- est[grep("mu", row.names(est)),c(4,8)]

Mu <- as.data.frame(cbind(MuM, MuQ))
row.names(Mu) <- NULL
colnames(Mu)[1:3] <- c("Mean", "Lower", "Upper")
dim(Mu)

id_mu <- s2010_votes %>% select(id_votacion, id_list) %>% distinct(id_votacion,id_list)
Mu <- as.data.frame(cbind(id_mu, Mu))
Mu$id_votacion <- trimws(Mu$id_votacion)

# merge "Alpha" y "s2010_vote_data"
Mu <- merge(Mu, s2010_vote_data, by="id_votacion")
#Mu <- Mu[order(Mu$Mean),]
#head(Mu)

Mu <- Mu %>% mutate(distin = (Upper>0 & Lower>0 | Upper<0 & Lower<0),
                          ypc = (si/(no+si))*100,
                          miss= ((abstenciones+inasistencia)/91)*100)

Mu <- Mu %>% mutate(baja = Upper<0 & Lower<0,
                    media = (Upper>0 & Lower<0 |Upper<0 & Lower>0),
                    alta = Upper>0 & Lower>0)
                    

sum(Mu$baja==TRUE)
Mu_baja <- Mu %>% filter(baja==TRUE)
table(Mu_baja$tema_principal)
table(Mu_baja$iniciativa)

sum(Mu$media==TRUE)
Mu_media <- Mu %>% filter(media==TRUE)
table(Mu_media$tema_principal)
table(Mu_media$iniciativa)

sum(Mu$alta==TRUE)
Mu_alta <- Mu %>% filter(alta==TRUE)
table(Mu_alta$tema_principal)
table(Mu_alta$iniciativa)

#########
# ggplot 

Mu_dis <- Mu %>% select(ypc, Mean)
windows()
ggplot(Mu_dis, aes(x = ypc, y = Mean)) +
  geom_point() +
  xlab("Porcentaje de votos a favor") +
  ylab("Estimaci髇 puntual mu") +
  geom_vline(xintercept = 2.5, col="gray34", lty=2) +
  geom_vline(xintercept = 97.5, col="gray34", lty=2) +
  scale_y_continuous(limits = c(-8, 8), breaks=seq(-8, 8, 1)) +
  scale_x_continuous(limits = c(0, 100), breaks=seq(0, 100, 10)) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=12),
        axis.text.y = element_text(colour="black", face="bold", size=12),
        axis.title = element_text(face="bold", colour="black", size=rel(1.3)))


Mu_Alpha <- data.frame(Mu$Mean, Alpha$Mean)
windows()
ggplot(Mu_Alpha, aes(x = Mu.Mean, y = Alpha.Mean)) +
  geom_point() +
  xlab("Estimaci髇 puntual mu") +
  ylab("Estimaci髇 puntual alpha") +
  scale_y_continuous(limits = c(-10, 10), breaks=seq(-10, 10, 1)) +
  scale_x_continuous(limits = c(-8, 8), breaks=seq(-8, 8, 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=12),
        axis.text.y = element_text(colour="black", face="bold", size=12),
        axis.title = element_text(face="bold", colour="black", size=rel(1.3)))



################################################
eta <- est[grep("eta", row.names(est)),1]
theta <- 1/(1+exp(-eta))
s2010_votes$prob <- theta

class(s2010_votes$id_votacion)
p1 <- s2010_votes %>% filter(id_votacion==10000)
min(p1$prob)
max(p1$prob)
