library(tidyverse)
library(knitr)
library(xtable)
library(plyr)
library(gtable)
library(data.table)
######################################################################
################### MODEL IMPLEMENTATION BES #########################
######################################################################
#This script implement the Gibbs sampler for estimating the latent well being
#factor for each Italian Province according to the different parametrizion of
#prior the spatial matrix explained in the paper.
rm(list=ls())
source("1_data.R") #Read_in the BES DATA
source("Sampler_Factor.R")#Read_in Sampler

Seed=123
for (d in domains)
{
  for (m in models)
  {
    for (t in years)
    {
      N=dim(y_norm[[d]][[t]])[1]
      J=dim(y_norm[[d]][[t]])[2]
      L=1
      inits <- list(lambda=matrix(1,J,L),
                    mu=matrix(0,J,1),
                    Sigma=diag(1,J),
                    PSI=diag(N),
                    delta= rmvnorm(N,rep(0),diag(1)),
                    a = ifelse (m %in% c("1","4"),1,0)
      )
      set.seed(Seed + as.numeric(m) + ifelse(m %in% c("3","5"),2,1))
	  xx <- sprintf("Data/output_%s_%s_%s.RData",d,m,t)
	  print(paste(d, m, t))
      output <- simple(n_iter = 30000, burn_in = 20000,
                                      y =y_norm[[d]][[t]],
                                      inits,
                                      0, G =10000,
                                      1/1000,1/1000,
                                      R=spmatrices[[m]][["R"]],
                                      J=J,
                                      W=spmatrices[[m]][["W"]],
                                      O=spmatrices[[m]][["O"]],
                                      w=spmatrices[[m]][["w"]],
                                      V_mu=1000,
                                      mu_a = ifelse (m %in% c("1","4"),1,0),
		        V_a =  ifelse(m %in% c("1","4"),100,100),
                                      tuning=ifelse(m == "1",sqrt(1000), sqrt(10/N)),
		        tuning_lambda= ifelse(d == "Social",10,ifelse(d == "Economic",10,5)),
                                      model = m)
	save(output,file=xx);
   
    }
  }
}

