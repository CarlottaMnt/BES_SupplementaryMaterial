library(tidyverse)
library(tidybayes)
library(knitr)
library(xtable)
library(plyr)
library(gtable)
library(janitor)
library(bayesplot)
##################################################################################################################
##################################################################################################################
################################# DIAGNOSTIC-CHECK #############################################################
##################################################################################################################
##################################################################################################################
#This script is for creating the diagnostic covergence plot for the MCMC. Be aware that the code will
#generate plots for each parameter estimated in the chain. So, select that parameter of interest before running. 
#The plots will be store in the "Diagnostic" folder. 
#-----------------------------------------------------------------------------------------------------
# The theory of Markov chains guarantees that a Markov chain that is irreducible and has invariant
# distribution $f$ converges to the invariant distribution. The three tasks of diagnosing entail 
# different aspect of convergence:

#1: Convergence to the target distribution: mainly, we need to assess whether $(X^{(t)})_t$
#   has reached a stationary regime and whether $(X^{(t)})_t$ covers the entire support 
#   of the target distribution.
#2: Convergence of the averages: Does  $\sum_{t=1}^T h(X^(t))/T$ provide a good approximation
#   to the expectation $E_f(h(X))$ under the target distribution?
#3: Comparison to i.i.d sampling:.How much information is contained in the sample from the Markov
#   chain compared to i.i.d sampling? 
rm(list=ls())
load("1_data.RData")
#Diagnostic convergence--------------------------------------------------------------
##Factor loading
dev.off()
par(mar=c(1,1,1,1))
 par(mfrow=c(2,1))
for (d in domains)
{
  for (m in models)
  {
    for (t in years)
    {
      xx <- sprintf("Data/output_%s_%s_%s.RData",d,m,t)
      load(file=xx);
      D = dim(output[[1]])[1]
      for (l in 1:dim(output[["lambda"]])[2])
      { 
        xf <- sprintf("Diagnostic/lambdamc_%s_%s_%s_%s.png",d,m,t,l)
        readline(paste(d, m, t, l))  
        png(xf) 
        readline(paste(d, m, t, l))  
        plot(output$lambda[,l], type="l",xlab = "iterations", ylab=  paste(d, m, t, l))
        plot(cumsum((output$lambda[,l]))/(1:D), type="l", xlab = "iterations", ylab=  paste(d, m, t, l))    
        dev.off()        
      }
   }
 } 
}

##Latent variable

dev.off()       

par(mar=c(1,1,1,1))
for (d in domains)
{
  for (m in models)
  {
    for (t in years)
    {
      xx <- sprintf("Data/output_%s_%s_%s.RData",d,m,t)
      load(file=xx)
      D = dim(output[[2]])[1]
      N=  dim(output[[2]])[2]
      for (l in 1:N)
      { 
       xf <- sprintf("Diagnostic/deltamc_%s_%s_%s_%s.png",d,m,t,l)
       readline(paste(d, m, t, l))  
       png(xf) 
       par(mfrow=c(2,1))
       plot(output$delta[,l], type="l", xlab="iterations", ylab= paste(d, m, t, l))
       plot(cumsum(output$delta[,l])/(1:D), type="l", xlab="iterations", ylab= paste(d, m, t, l)) 
       dev.off()         
      }      
    } 
  } 
}

##Posterior mean delta (d)
dev.off()       

par(mar=c(1,1,1,1))
for (d in domains)
{
  for (m in models)
  {
    for (t in years)
    {
      xx <- sprintf("Data/output_%s_%s_%s.RData",d,m,t)
      load(file=xx)
      D = dim(output[[2]])[1]
      N=  dim(output[[2]])[2]
      for (l in 1:N)
      { 
       xf <- sprintf("Diagnostic/dmc_%s_%s_%s_%s.png",d,m,t,l)
       readline(paste(d, m, t, l))  
       png(xf) 
       par(mfrow=c(2,1))
       plot(output$delta[,l], type="l", xlab="iterations", ylab= paste(d, m, t, l))
       plot(cumsum(output$delta[,l])/(1:D), type="l", xlab="iterations", ylab= paste(d, m, t, l)) 
       dev.off()         
      }      
    } 
  } 
}
#Posterior Variance-Covariance delta (D): only the diagonal, i.e the posterior variance of the latent factor
dev.off()       

par(mar=c(1,1,1,1))
par(mfrow=c(2,1))
for (d in domains)
{
  for (m in models[2])
  {
    for (t in years)
    {
      xx <- sprintf("Data/output_%s_%s_%s.RData",d,m,t)
      load(file=xx)
      D = dim(output[[2]])[1]
      N = dim(output[[2]])[2]
      for (s in (0:(N-1)))
      { 
       xf <- sprintf("Diagnostic/Dmc_%s_%s_%s_%s.png",d,m,t,l)
       readline(paste(d, m, t, s))  
       png(xf) 
       plot(output$D[,(s*N + (s+1))], type="l", xlab="iterations", ylab= paste(d, m, t, s))
       plot(cumsum(output$D[,(s*N +(s+1))])/(1:D), type="l", xlab="iterations", ylab= paste(d, m, t, s)) 
       dev.off()         
      }     
    } 
  } 
}

##Mu
dev.off()

par(mar=c(1,1,1,1))
for (d in domains)
{
  for (m in models)
  {
    for (t in years)
    {
      xx <- sprintf("Data/output_%s_%s_%s.RData",d,m,t)
      load(file=xx)
      D = dim(output[[1]])[1]
      for (l in 1:dim(output[["mu"]])[2]))
      { 
        plot(output$mu[,l], type="l")
        plot(cumsum((output$mu[,l]))/(1:D), type="l")
        readline(paste(d, m, t, l))
      }      
    } 
  } 
}
#Sigma

dev.off()       
par(mfrow=c(2,1))
par(mar=c(1,1,1,1))
for (d in domains)
{
 for (m in models)
 {
   for (t in years)
   { 
      x <- sprintf("Data/output_%s_%s_%s.RData",d,m,t)
      load(file=xx)
      D = dim(output[[1]])[1]
      for (l in 1:dim(output[["Sigma"]])[2]))
      { 
        plot(output$Sigma[,l], type="l", xlab="iterations", ylab= paste(d, m, t, l))
        plot(cumsum(output$Sigma[,l])/(1:D), type="l", xlab="iterations", ylab= paste(d, m, t, l)) 
        readline(paste(d, m, t, l))           
      }      
    } 
  } 
}

#Y_rep: here we will plot the predictive distribution over the true distribution
color_scheme_set("brightblue")
for (d in domains)
{
 for (m in models)
 {
   for (t in years)
   { 
      x <- sprintf("Data/output_%s_%s_%s.RData",d,m,t)
      load(file=xx)
      D = dim(output[[1]])[1]
      J =  dim(output[[1]])[2]
      N =  dim(y_norm[[d]][[t]])[1]
      yrep <- matrix(NA,D, N * J) 
    for(j in (0:(J-1)))
    { 
    for(s in (1:D))
      { 
      yrep[s,] <- output$y_rep[s,] %>% 
      matrix(N,J,byrow=TRUE) %>% 
      as.vector() 
      }  
      y <- y_norm[[d]][[t]][,j+1]      
      ppc_dens_overlay(y, yrep[1:500,(N*j+1):(N *(j+1))])
     }      
   } 
  } 
}   
