
rm(list=ls()) 

library(pscl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(rstan)


load("~/fit0_L1.Rda")
load("~/data0.Rda")

theta <- 1/(1+exp(-(extract(stan.fit)$eta)))
dim(theta)
class(theta)

n_sims <- dim(theta)[1]
Y_obs <- dim(theta)[2]


inicio <- Sys.time()
E_proof <- c()
set.seed(1234)
for(i in 1:n_sims){
  E_proof[i] <- mean(rbinom(n = Y_obs, size = 1,prob = theta[i,]))
}
fin <- Sys.time()
fin-inicio

length(E_proof)
(m_sim <- mean(E_proof))
(m_obs <- mean(s2010_votes$voto))
(ppp <- round(mean(E_proof > m_obs),2))

E_proof1 <- data.frame(E_proof)
colnames(E_proof1) <- c("E_proof")

(q1 <- quantile(E_proof, 0.025))
(q2 <- quantile(E_proof, 0.975))

windows()
ggplot(E_proof1, aes(x = E_proof)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(colour="black") +
  geom_vline(xintercept = m_obs, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = m_sim, linetype="dashed", col="blue", size = 1.3) + 
  geom_vline(xintercept = q1, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2, linetype="dashed", col="green", size = 1) +
  xlab("Valores estadístico de prueba") +
  ylab("Densidad") +
  annotate("text", x=0.73, 150, 
           label= c("ppp = 0.21"), 
           fontface="bold",
           size = 5) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


D_proof <- sqrt(E_proof*(1-E_proof))
d_sim <- mean(D_proof)
d_obs <- sqrt(m_obs*(1-m_obs))
(ppp_d <- round(mean(D_proof > d_obs),2))

(q1d <- quantile(D_proof, 0.025))
(q2d <- quantile(D_proof, 0.975))

D_proof1 <- data.frame(D_proof)
colnames(D_proof1) <- c("D_proof")

windows()
ggplot(D_proof1, aes(x = D_proof)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(colour="black") +
  geom_vline(xintercept = d_obs, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = d_sim, linetype="dashed", col="blue", size = 1.3) + 
  geom_vline(xintercept = q1d, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2d, linetype="dashed", col="green", size = 1) +
  xlab("Valores estadístico de prueba") +
  ylab("Densidad") +
  annotate("text", x=0.4515, 300, 
           label= c("ppp = 0.79"), 
           fontface="bold",
           size = 5) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


S_proof <- ((1-E_proof)-E_proof)/sqrt((1-E_proof)*E_proof)
s_sim <- mean(S_proof)
s_obs <- ((1-m_obs)-m_obs)/sqrt((1-m_obs)*m_obs)
(ppp_s <- round(mean(S_proof > s_obs),2))

(q1s <- quantile(S_proof, 0.025))
(q2s <- quantile(S_proof, 0.975))

S_proof1 <- data.frame(S_proof)
colnames(S_proof1) <- c("S_proof")

windows()
ggplot(S_proof1, aes(x = S_proof)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(colour="black") +
  geom_vline(xintercept = s_obs, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = s_sim, linetype="dashed", col="blue", size = 1.3) + 
  geom_vline(xintercept = q1s, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2s, linetype="dashed", col="green", size = 1) +
  xlab("Valores estadístico de prueba") +
  ylab("Densidad") +
  annotate("text", x=-0.95, 27, 
           label= c("ppp = 0.79"), 
           fontface="bold",
           size = 5) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


C_proof <- (1-(6*(1-E_proof)*E_proof))/((1-E_proof)*E_proof)
c_sim <- mean(C_proof)
c_obs <- (1-(6*(1-m_obs)*m_obs))/((1-m_obs)*m_obs)
(ppp_c <- round(mean(C_proof > c_obs),2))

(q1c <- quantile(C_proof, 0.025))
(q2c <- quantile(C_proof, 0.975))

C_proof1 <- data.frame(C_proof)
colnames(C_proof1) <- c("C_proof")

windows()
ggplot(C_proof1, aes(x = C_proof)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(colour="black") +
  geom_vline(xintercept = c_obs, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = c_sim, linetype="dashed", col="blue", size = 1.3) + 
  geom_vline(xintercept = q1c, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2c, linetype="dashed", col="green", size = 1) +
  xlab("Valores estadístico de prueba") +
  ylab("Densidad") +
  annotate("text", x=-0.925, 15, 
           label= c("ppp = 0.21"), 
           fontface="bold",
           size = 5) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


En_proof <- -(1-E_proof)*log(1-E_proof)-(E_proof)*log(E_proof)
en_sim <- mean(En_proof)
en_obs <- -(1-m_obs)*log(1-m_obs)-(m_obs)*log(m_obs)
(ppp_en <- round(mean(En_proof > en_obs),2))

(q1en <- quantile(En_proof, 0.025))
(q2en <- quantile(En_proof, 0.975))

En_proof1 <- data.frame(En_proof)
colnames(En_proof1) <- c("En_proof")

windows()
ggplot(En_proof1, aes(x = En_proof)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(colour="black") +
  geom_vline(xintercept = en_obs, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = en_sim, linetype="dashed", col="blue", size = 1.3) + 
  geom_vline(xintercept = q1en, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2en, linetype="dashed", col="green", size = 1) +
  xlab("Valores estadístico de prueba") +
  ylab("Densidad") +
  annotate("text", x=0.598, 150, 
           label= c("ppp = 0.79"), 
           fontface="bold",
           size = 5) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


F_proof <- 1/((1-E_proof)*E_proof)
f_sim <- mean(F_proof)
f_obs <- 1/((1-m_obs)*m_obs)
(ppp_f <- round(mean(F_proof > f_obs),2))

(q1f <- quantile(F_proof, 0.025))
(q2f <- quantile(F_proof, 0.975))

F_proof1 <- data.frame(F_proof)
colnames(F_proof1) <- c("F_proof")

windows()
ggplot(F_proof1, aes(x = F_proof)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(colour="black") +
  geom_vline(xintercept = f_obs, linetype="dashed", col="red", size = 1.3) +
  geom_vline(xintercept = f_sim, linetype="dashed", col="blue", size = 1.3) + 
  geom_vline(xintercept = q1f, linetype="dashed", col="green", size = 1) + 
  geom_vline(xintercept = q2f, linetype="dashed", col="green", size = 1) +
  xlab("Valores estadístico de prueba") +
  ylab("Densidad") +
  annotate("text", x= 5.075, 13, 
           label= c("ppp = 0.21"), 
           fontface="bold",
           size = 5) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))



