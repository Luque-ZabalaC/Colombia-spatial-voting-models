rm(list=ls()) 



library(pscl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(rstan)

load("~/data0.Rda")
load("~/fit0_L1.Rda")
est <- summary(stan.fit)$summary
pos_sim <- extract(stan.fit)
rm(stan.fit)

s2010 <- readRDS("~/s2010.rds")
s2010_legis_data <- s2010[["legis.data"]]
s2010_vote_data <- s2010[["vote.data"]]
rm(s2010)



# Estimated ideal points.
BetaM <- est[grep("xi", row.names(est)),1][90:180]
# Credibility interval
BetaQ <- est[grep("xi", row.names(est)),c(4,8)][90:180,]

Beta <- as.data.frame(cbind(BetaM, BetaQ))
dim(Beta)

leg_group <- s2010_votes %>% select(legislador, grupo,id_legis)%>% 
  distinct(legislador, grupo, id_legis) %>% 
  arrange(id_legis)

Beta$legislador <- leg_group$legislador
row.names(Beta) <- NULL
colnames(Beta)[1:3] <- c("Mean", "Lower", "Upper")

# Merge "Beta" y "s2010_legis_data"
Beta <- merge(Beta, s2010_legis_data, by="legislador")
#Beta <- Beta[order(Beta$Mean),]
Beta <- Beta %>% rename(group=grupo, legislator=legislador)


# opposition (PDA)
Beta[1,c(1,2)]
est[grep("xi", row.names(est)),][1,1]
mean(pos_sim$xi[,1])

leg1 <- data.frame(pos_sim$xi[,1])
colnames(leg1) <- c("leg")

(q1f <- quantile(pos_sim$xi[,1], 0.025))
(q2f <- quantile(pos_sim$xi[,1], 0.975))
m1 <- mean(pos_sim$xi[,1])

windows()
ggplot(leg1, aes(x = leg)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "grey", fill = "white") +
  geom_density(colour="black",
               lwd=1.3) +
  geom_vline(xintercept = m1, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = q1f, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2f, linetype="dashed", col="green", size = 1) +
  xlab(expression(paste("  ", beta[1]))) +
  ylab("Densidad posterior") +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


# independent (PIN)
Beta[76,c(1,2)]
est[grep("xi", row.names(est)),][39,1]
mean(pos_sim$xi[,39])

leg39 <- data.frame(pos_sim$xi[,39])
colnames(leg39) <- c("leg")

(q1f <- quantile(pos_sim$xi[,39], 0.025))
(q2f <- quantile(pos_sim$xi[,39], 0.975))
m39 <- mean(pos_sim$xi[,39])

windows()
ggplot(leg39, aes(x = leg)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "grey", fill = "white") +
  geom_density(colour="black",
               lwd=1.3) +
  geom_vline(xintercept = m39, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = q1f, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2f, linetype="dashed", col="green", size = 1) +
  xlab(expression(paste("  ", beta[39])))+
  ylab("Densidad posterior") +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))

# minorities (AICO)
Beta[12,c(1,2)]
est[grep("xi", row.names(est)),][33,1]
mean(pos_sim$xi[,33])

leg33 <- data.frame(pos_sim$xi[,33])
colnames(leg33) <- c("leg")

(q1f <- quantile(pos_sim$xi[,33], 0.025))
(q2f <- quantile(pos_sim$xi[,33], 0.975))
m33 <- mean(pos_sim$xi[,33])

windows()
ggplot(leg33, aes(x = leg)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "grey", fill = "white") +
  geom_density(colour="black",
               lwd=1.3) +
  geom_vline(xintercept = m33, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = q1f, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2f, linetype="dashed", col="green", size = 1) +
  xlab(expression(paste("  ", beta[33]))) +
  ylab("Densidad posterior") +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


# coalition
Beta[69,c(1,2)]
est[grep("xi", row.names(est)),][53,1]
mean(pos_sim$xi[,54])

leg54 <- data.frame(pos_sim$xi[,54])
colnames(leg54) <- c("leg")

(q1f <- quantile(pos_sim$xi[,54], 0.025))
(q2f <- quantile(pos_sim$xi[,54], 0.975))
m54 <- mean(pos_sim$xi[,54])

windows()
ggplot(leg54, aes(x = leg)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "grey", fill = "white") +
  geom_density(colour="black",
               lwd=1.3) +
  geom_vline(xintercept = m54, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = q1f, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2f, linetype="dashed", col="green", size = 1) +
  xlab(expression(paste("  ", beta[53]))) +
  ylab("Densidad posterior") +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


#####

# left extremists
round(mean(pos_sim$xi[,1] < -1),2)  #0.99
round(mean(pos_sim$xi[,11] < -1),2) #0.98
round(mean(pos_sim$xi[,36] < -1),2)
round(mean(pos_sim$xi[,52] < -1),2) #0.06
round(mean(pos_sim$xi[,53] < -1),2)
round(mean(pos_sim$xi[,70] < -1),2) #0.01
round(mean(pos_sim$xi[,79] < -1),2) #0.75


# right extremists
sen_co <- c(3,4,6,7,8,9,10,14,15,16,17,18,19,20,21,22,23,25,26,27,28,30,31,32,34,35,37,
            38,41,42,44,45,46,48,49,51,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,
            71,72,74,75,77,78,80,81,82,84,85,86,87,88,89,90)
prob_co <- c()
for(i in 1:length(sen_co)){
prob_co[i] <- round(mean(pos_sim$xi[,sen_co[i]] > 1), 2)
}
prob_co
(prob_coa <- data.frame(sen_co,prob_co))

# center
sen_cen <- c(2,5,12,13,24,29,33,39,40,43,47,50,73,76,83,91)
prob_cen <- c()
for(i in 1:length(sen_cen)){
  prob_cen[i] <- round(mean(pos_sim$xi[,sen_cen[i]] < 0.2), 2) - round(mean(pos_sim$xi[,sen_cen[i]] < - 0.2), 2)
}
prob_cen
(prob_centro <- data.frame(sen_cen,prob_cen))

sen_PIN <- c(5,13,24,39,40,83,91)
prob_PIN <- c()
for(i in 1:length(sen_PIN)){
  prob_PIN[i] <- round(mean(pos_sim$xi[,sen_PIN[i]] > 0.2), 2) 
}
prob_PIN
(prob_PINp <- data.frame(sen_PIN,prob_PIN))


#######

# Behavior of discrimination parameters
AlphaM <- est[grep("alpha", row.names(est)),1]

# Credibility interval
AlphaQ <- est[grep("alpha", row.names(est)),c(4,8)]

Alpha <- as.data.frame(cbind(AlphaM, AlphaQ))
row.names(Alpha) <- NULL
colnames(Alpha)[1:3] <- c("Mean", "Lower", "Upper")
dim(Alpha)

id_alpha <- s2010_votes %>% select(id_votacion, id_list) %>% distinct(id_votacion,id_list)
Alpha <- as.data.frame(cbind(id_alpha, Alpha))
Alpha$id_votacion <- trimws(Alpha$id_votacion)

# Merge "Alpha" y "s2010_vote_data"
Alpha <- merge(Alpha, s2010_vote_data, by="id_votacion")

Alpha <- Alpha %>% mutate(distin = (Upper>0 & Lower>0 | Upper<0 & Lower<0),
                          ypc = (si/(no+si))*100,
                          miss= ((abstenciones+inasistencia)/91)*100)

Alpha_dis <- Alpha %>% filter(distin == "TRUE")
Alpha_disNO <- Alpha %>% filter(distin == "FALSE")


# voting lists that discriminate
Alpha[305,c(1,3)]
est[grep("alpha", row.names(est)),][305,1]
mean(pos_sim$alpha[,305])

alpha305 <- data.frame(pos_sim$alpha[,305])
colnames(alpha305) <- c("alpha")

(q1f <- quantile(pos_sim$alpha[,305], 0.025))
(q2f <- quantile(pos_sim$alpha[,305], 0.975))
m305 <- mean(pos_sim$alpha[,305])

windows()
ggplot(alpha305, aes(x = alpha)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "grey", fill = "white") +
  geom_density(colour="black",
               lwd=1.3) +
  geom_vline(xintercept = m305, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = q1f, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2f, linetype="dashed", col="green", size = 1) +
  xlab(expression(paste("  ", alpha[305]))) +
  ylab("Densidad posterior") +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))

# voting lists that do not discriminate
Alpha[412,c(1,3)]
est[grep("alpha", row.names(est)),][412,1]
mean(pos_sim$alpha[,412])

alpha412 <- data.frame(pos_sim$alpha[,412])
colnames(alpha412) <- c("alpha")

(q1f <- quantile(pos_sim$alpha[,412], 0.025))
(q2f <- quantile(pos_sim$alpha[,412], 0.975))
m412 <- mean(pos_sim$alpha[,412])

windows()
ggplot(alpha412, aes(x = alpha)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "grey", fill = "white") +
  geom_density(colour="black",
               lwd=1.3) +
  geom_vline(xintercept = m412, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = q1f, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2f, linetype="dashed", col="green", size = 1) +
  xlab(expression(paste("  ", alpha[412]))) +
  ylab("Densidad posterior") +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


# unanimous voting lists
Alpha[359,c(1,3)]
est[grep("alpha", row.names(est)),][359,1]
mean(pos_sim$alpha[,359])

alpha359 <- data.frame(pos_sim$alpha[,359])
colnames(alpha359) <- c("alpha")

(q1f <- quantile(pos_sim$alpha[,359], 0.025))
(q2f <- quantile(pos_sim$alpha[,359], 0.975))
m359 <- mean(pos_sim$alpha[,359])

windows()
ggplot(alpha359, aes(x = alpha)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "grey", fill = "white") +
  geom_density(colour="black",
               lwd=1.3) +
  geom_vline(xintercept = m359, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = q1f, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2f, linetype="dashed", col="green", size = 1) +
  xlab(expression(paste("  ", alpha[359]))) +
  ylab("Densidad posterior") +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


########

# Behavior of approval parameters
MuM <- est[grep("mu", row.names(est)),1]

# Credibility interval
MuQ <- est[grep("mu", row.names(est)),c(4,8)]

Mu <- as.data.frame(cbind(MuM, MuQ))
row.names(Mu) <- NULL
colnames(Mu)[1:3] <- c("Mean", "Lower", "Upper")
dim(Mu)

id_mu <- s2010_votes %>% select(id_votacion, id_list) %>% distinct(id_votacion,id_list)
Mu <- as.data.frame(cbind(id_mu, Mu))
Mu$id_votacion <- trimws(Mu$id_votacion)

# Merge "Alpha" y "s2010_vote_data"
Mu <- merge(Mu, s2010_vote_data, by="id_votacion")


# voting lists that discriminate
Mu[305,c(1,3)]
est[grep("mu", row.names(est)),][305,1]
mean(pos_sim$mu[,305])

mu305 <- data.frame(pos_sim$mu[,305])
colnames(mu305) <- c("mu")

(q1f <- quantile(pos_sim$mu[,305], 0.025))
(q2f <- quantile(pos_sim$mu[,305], 0.975))
me305 <- mean(pos_sim$mu[,305])

windows()
ggplot(mu305, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "grey", fill = "white") +
  geom_density(colour="black",
               lwd=1.3) +
  geom_vline(xintercept = me305, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = q1f, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2f, linetype="dashed", col="green", size = 1) +
  xlab(expression(paste("  ", mu[305]))) +
  ylab("Densidad posterior") +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


# voting lists that do not discriminate
Mu[412,c(1,3)]
est[grep("mu", row.names(est)),][412,1]
mean(pos_sim$mu[,412])

mu412 <- data.frame(pos_sim$mu[,412])
colnames(mu412) <- c("mu")

(q1f <- quantile(pos_sim$mu[,412], 0.025))
(q2f <- quantile(pos_sim$mu[,412], 0.975))
me412 <- mean(pos_sim$mu[,412])

windows()
ggplot(mu412, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "grey", fill = "white") +
  geom_density(colour="black",
               lwd=1.3) +
  geom_vline(xintercept = me412, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = q1f, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2f, linetype="dashed", col="green", size = 1) +
  xlab(expression(paste("  ", mu[412]))) +
  ylab("Densidad posterior") +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


# unanimous voting lists
Mu[359,c(1,3)]
est[grep("mu", row.names(est)),][359,1]
mean(pos_sim$mu[,359])

mu359 <- data.frame(pos_sim$mu[,359])
colnames(mu359) <- c("mu")

(q1f <- quantile(pos_sim$mu[,359], 0.025))
(q2f <- quantile(pos_sim$mu[,359], 0.975))
me359 <- mean(pos_sim$mu[,359])

windows()
ggplot(mu359, aes(x = mu)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "grey", fill = "white") +
  geom_density(colour="black",
               lwd=1.3) +
  geom_vline(xintercept = me359, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = q1f, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2f, linetype="dashed", col="green", size = 1) +
  xlab(expression(paste("  ", mu[359]))) +
  ylab("Densidad posterior") +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))
