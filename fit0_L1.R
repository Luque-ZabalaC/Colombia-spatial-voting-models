###---------------------------------------------------------------------------------------------------------------------###
########################### Model_ideal_point with logit link and omitting missing values #################################
###---------------------------------------------------------------------------------------------------------------------###

# Author: Carolina Luque
# email: cluque2.d@universidadean.edu.co
# affiliation: Universidad Ean

rm(list=ls()) 


###---------------------------------------------------------------------------------------------------------------------###
######################################################## Load libraries ###################################################
###---------------------------------------------------------------------------------------------------------------------###

library(readxl)
library(readr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


###---------------------------------------------------------------------------------------------------------------------###
######################################################### Load dataset ####################################################
###---------------------------------------------------------------------------------------------------------------------###

load("~/data0.Rda")

###---------------------------------------------------------------------------------------------------------------------###
######################################################### Data ###########################################################
###---------------------------------------------------------------------------------------------------------------------###

(N <- max(s2010_votes$id_legis))                           
(M <- max(s2010_votes$id_list)) 


#path = "~/scripts"
file.names <- dir(path, pattern =".stan",full.names = TRUE)
mod_ideal_point <- stan_model("ideal_point_0.stan")
mod_ideal_point


# L1. Anchor legislators. 
xi <-
  s2010_votes %>%
  select(legislador,id_legis, grupo)%>%
  distinct(legislador,id_legis, grupo)%>%
  arrange(id_legis)%>%
  mutate( xi = if_else(legislador == "Roy Leonardo Barreras Montealegre:PU", 1,
                       if_else(legislador == "Jorge Enrique Robledo Castillo:PDA", -1, NA_real_)),
          init = if_else(grupo == "coalicion", 1,-1))



legislators_data <-
  within(list(), {
    y <- as.integer(s2010_votes$voto)                   
    y_idx_leg <- as.integer(s2010_votes$id_legis)       
    y_idx_vote <- as.integer(s2010_votes$id_list)       
    Y_obs <- length(y)                              
    N <- max(s2010_votes$id_legis)                      
    M <- max(s2010_votes$id_list)                       
    # priors
    mu_loc <- 0                                                                               
    mu_scale <- 5                                         
    alpha_loc <- 0                                        
    alpha_scale <- 5                                      
    beta_loc <- 0                                         
    beta_scale <- 1
    N_xi_obs <- sum(!is.na(xi$xi))                      
    idx_xi_obs <- which(!is.na(xi$xi))                  
    xi_obs <- xi$xi[!is.na(xi$xi)]                    
    N_xi_param <- sum(is.na(xi$xi))                     
    idx_xi_param <- which(is.na(xi$xi))                 
  })

legislators_init <- list(                               
  list(xi_param = xi$init[is.na(xi$xi)])
)

ini <- Sys.time()
stan.fit <- sampling(mod_ideal_point, 
                       data = legislators_data,
                       chains = 1, 
                       iter = 80000,
                       warmup = 24000, 
                       thin = 5,
                       init = legislators_init,
                       refresh = 10000,
                       seed=12345)
end <- Sys.time()
(time <- end - ini)

save(stan.fit, file = "~/fit0_L1.Rda")


