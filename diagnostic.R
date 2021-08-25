rm(list=ls()) 


library(pscl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(rstan)

##################
# Diagnostic model
##################


# Model (a)
load("~/fit0_L1.Rda")

#convergence
print(stan.fit,"lp__")

windows()
stan_rhat(stan.fit, bins=60, fill="white") + 
  xlab(expression(paste("Estadística  ", hat(R)))) +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))
       
windows()
stan_ess(stan.fit, bins=60, fill="white") + 
  xlim(c(0.8,1.11)) + 
  xlab("Tamaño efectivo de muestra/Tamaño de muestra") +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))

windows()
stan_mcse(stan.fit, bins=60, fill="white") + 
  xlim(c(0.009,0.0106)) + 
  xlab("Error estándar Monte Carlo/Desviación estádar posterior") +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))

windows()
traceplot(stan.fit, par = "lp__", colour="black") + 
  theme (legend.position = "none",
         axis.text.x = element_text(colour="black", face="bold", size=13),
         axis.text.y = element_text(colour="black", face="bold", size=13),
         axis.title = element_text(face="bold", colour="black", size=rel(1.5))) + 
  ylab("Log-verosimilitud posterior") +
  xlab("iteraciones")

x <- extract(stan.fit)$lp
df <- data.frame(x)
ggplot(df, aes(x = x)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(colour="black") +
  xlab("Log-verosimilitud posterior") +
  ylab("Densidad") +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))

est <- summary(stan.fit)$summary
rm(stan.fit)


l1 <- est[grep("xi", row.names(est)),][90,]
sampler <- extract(stan.fit)$xi[,1]
(mean1 <- mean(sampler))
l1[1]
(sd1 <- sd(sampler))
l1[3]

(tm1 <- as.numeric(l1[9]))
(se_mean1 <- sd1/sqrt(tm1))
l1[2]
(se_mean1/sd1)
(1/sqrt(tm1))
(cv1 <- se_mean1/abs(mean1))


se_mean <- as.vector(est[,2])
length(se_mean)
mean <- abs(as.vector(est[,1]))
length(mean)
cv <- round((se_mean/mean)*100,2)
length(cv)
summary(cv)
which(cv == 5966.730)

((se_mean[19931]/mean[19931])*100)
cv[19931]
est[19931,]


se_mean <- as.vector(est[1:923,2])
length(se_mean)
mean <- abs(as.vector(est[1:923,1]))
length(mean)
cv <- round((se_mean/mean)*100,2)
length(cv)
summary(cv)
cv[cv>25]
which(cv == 74.75)
hist(cv)

windows()
cv1 <- data.frame(cv)
colnames(cv1) <- c("cv")
ggplot(data=cv1, aes(x=cv)) + 
  geom_histogram(breaks=seq(0, 13, by = 0.5), 
                 col="black", 
                 fill="white") + 
  labs(x="Coeficiente de variación de las estimaciones (%)", y="Número de parámetros") + 
  xlim(c(0,13)) +
  theme_classic() +
  scale_x_continuous(breaks=seq(0, 13, 2))+
  scale_y_continuous(breaks=seq(0, 600, 100))+
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))


(n_eff <- summary(est[,9]))
(n_eff_beta <- summary(est[grep("xi", row.names(est)),9][90:180]))
(n_eff_mu <- summary(est[grep("mu", row.names(est)),9]))
(n_eff_alpha <- summary(est[grep("alpha", row.names(est)),9]))
