library(tidyverse)
library(tidybayes)
library(knitr)
library(xtable)
library(plyr)
library(gtable)
library(janitor)
##################################################################################################################
##################################################################################################################
################################# DIAGNOSTIC-CHECK #############################################################
##################################################################################################################
##################################################################################################################

rm(list=ls())
#Diagnostic convergence--------------------------------------------------------------
#Factor loading
dev.off()
par(mar=c(1,1,1,1))
for (d in domains)
{
  for (m in models)
  {
    for (t in years)
    {
      xx <- sprintf("Data/output_%s_%s_%s.RData",d,m,t)
      load(file=xx);
      D = dim(output)[[1]]
      for (l in 1:dim(output)[["lambda"]][2])
      { 
        xf <- sprintf("Diagnostic/lambda/lambdamc_%s_%s_%s_%s.png",d,m,t,l)
        readline(paste(d, m, t, l))  
        png(xf) 
        par(mfrow=c(2,1))
        plot(output$lambda[,l], type="l",xlab = "iterations", ylab=  paste(d, m, t, l))
        plot(cumsum((output$lambda[,l]))/(1:D), type="l", xlab = "iterations", ylab=  paste(d, m, t, l))       
        dev.off()        
      }
   }
 } 
}

#Latent variable
dev.off()       

par(mar=c(1,1,1,1))
for (d in domains)
{
  for (m in models[models %in% c("3")])
  {
    for (t in years[years %in% c("2017")])
    {
      xx <- sprintf("Data/output4_%s_%s_%s.RData",d,m,t)
      load(file=xx)
      D = dim(output)[[1]]
      for (l in (1:107))
      { 
       xf <- sprintf("Diagnostic/delta/deltamc_%s_%s_%s_%s.png",d,m,t,l)
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

#Mu
dev.off()

par(mfrow=c(2,1))
par("mar")
par(mar=c(1,1,1,1))


for (d in domains[1])
{
  for (m in models[models %in% c("0","1","2","3")])
  {
    for (t in years[years %in% c("2004","2006","2014","2017")])
    {
	   xx <- sprintf("Data/output3_%s_%s_%s.RData",d,m,t)
	   load(file=xx);
      for (l in 1:length(names[[d]]))
      { 
        plot(output$mu[,l], type="l")
        plot(cumsum((output$mu[,l]))/(1:10000), type="l")
        readline(paste(d, m, t, l))
      }
      
    } 
  } 
}
#Sigma

dev.off()       
par(mfrow=c(2,1))
par(mar=c(1,1,1,1))
for (d in domains[3])

{
  for (m in models[models %in% c("3")])
  {
    for (t in years[years %in% c("2004")])
    {
	   xx <- sprintf("Data/output3_%s_%s_%s.RData",d,m,t)
	   load(file=xx)
      for (l in 1:length(names[[d]]))
      { 
        plot(output$Sigma[,l], type="l", xlab="iterations", ylab= paste(d, m, t, l))
        plot(cumsum(output$Sigma[,l])/(1:5000), type="l", xlab="iterations", ylab= paste(d, m, t, l)) 
        readline(paste(d, m, t, l))           
      }      
    } 
  } 
}
#PSI
dev.off()       
par(mfrow=c(2,1))
par(mar=c(1,1,1,1))
for (d in domains[3])
{
  for (m in models[models %in% c("1","2","3")])
  {
    for (t in years[years %in% c("2004","2006","2014","2017")])
    {
	   xx <- sprintf("Data/output2_%s_%s_%s.RData",d,m,t)
	   load(file=xx)
      for (l in 1:N)
      { 
        plot(output$PSI[,l], type="l", xlab="iterations", ylab= paste(d, m, t, l))
        plot(cumsum(output$PSI[,l])/(1:5000), type="l", xlab="iterations", ylab= paste(d, m, t, l)) 
        readline(paste(d, m, t, l))           
      }      
    } 
  } 
}