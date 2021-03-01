library(tidyverse)
library(knitr)
library(xtable)
library(plyr)
library(gtable)
library(data.table)
library(grid)
library(gridExtra)
library(janitor)
######################################################################
###########DELTA DF D ################################################
######################################################################
#The scope of the script is to processing the ouput from the Simple function
#to create the dataframe of provincial composite indicators of the Social,
#Economic and Environmental domain.
-----------------------------------------------------------------------------------
#Import the initial data
rm(list=ls())
load("1_data.RData")

#Composite indicator
delta <- vector("list",length=length(domains))
d_summary <- vector("list",length=length(domains))

#Naming
names(delta) <- domains
names(d_summary) <- domains
for (d in domains)
{
  delta[[d]] <- vector("list",length=length(models[-c(5,6,7)]))
  names(delta[[d]]) <- models
  d_summary[[d]] <- vector("list",length=length(models[-c(5,6,7)]))
  names(d_summary[[d]]) <- models
  for (m in models)
  {
    delta[[d]][[m]] <- vector("list",length=length(years))
    names(delta[[d]][[m]]) <- years
    d_summary[[d]][[m]] <- vector("list",length=length(years))
    names(d_summary[[d]][[m]]) <- years
  }
}

#CREATE OBJECT FROM OUTPUT

for (d in domains)
{
  for (m in models)
  {
    for (t in years)
      {
          xx <- sprintf("Data/output_%s_%s_%s.RData",d,m,t)
          load(file=xx)
          N=dim(y_norm[[d]][[t]])[1]
          J=dim(y_norm[[d]][[t]])[2]         
          delta[[d]][[m]][[t]] <- output$delta %>% as.data.frame()
          d_summary[[d]][[m]][[t]] <- multi.sapply(delta[[d]][[m]][[t]], mean, median , third_qu=function(x) quantile(x, 0.75),firt_qu= function(x) quantile(x,0.25), IQR = IQR) %>%
			as.data.frame()%>%
			mutate(
			comune = province,
			anno=t ,
			model = m
			) %>% 
			as.list()
          
     }                  
  }
}


#DELTA_DF_D


delta_df <- vector("list",length=length(domains))
names(delta_df) <- domains
for (d in domains)
{
  delta_df[[d]] <- vector("list",length=length(models))
  names(delta_df[[d]]) <- models
}

for (d in domains)
{
  for (m in models)
  {
   delta_df[[d]][[m]] <- do.call(rbind.data.frame, d_summary[[d]][[m]])
  }
}

delta_df_d <- vector("list",length=length(domains))
names(delta_df_d) <- domains

for (d in domains)
{
  delta_df_d[[d]] <- do.call(rbind.data.frame, delta_df[[d]])
}

save(delta_df_d, file="delta_df_d.RData")
