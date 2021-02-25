library(tidyverse)
library(knitr)
library(xtable)
library(plyr)
library(gtable)
library(data.table)
library(gridExtra)
library(janitor)
################################################################################
################################################################################
################################################################################
#################################OVERALL WELL-BEING ############################
################################################################################
################################################################################
#This script must be run only after "2_model_implementation.R". The objective is
#to estimate the overall well being by applying the factor analytic model another time
#on the 3-dimensional vector of well being composite indicators estimated in the first step. 


rm(list=ls())
#import the data
source("1_data.RData")
source("Sampler_Factor.R")
source("delta_df_d.R")
source("functions.R")


##CREATE THE EMPTY DATASET##--------------------------------------------------------------

dataframe <- vector("list",length=length(domains))
names(dataframe) <- domains
data_df <- vector("list",length=length(domains))
names(data_df) <- domains

for (d in domains)
{
  dataframe[[d]] <- vector("list",length=length(models))
  names(dataframe[[d]]) <- models
  data_df[[d]] <- vector("list",length=length(models))
  names(data_df[[d]]) <- models]
  for (m in models)
  {
    dataframe[[d]][[m]] <- vector("list",length=length(years))
    names(dataframe[[d]][[m]]) <- years
  }
}

for (d in c(1,2,3))
{ 
 for (m in models)
 {
 for (t in years)
  {
  dataframe[[d]][[m]][[t]] <- delta_df_d[[d]] %>% filter (model == m, anno == t) %>% mutate(domain = domains[d], comune = province) %>% dplyr::select(mean,domain,anno,comune,model) %>% tidyr::spread(domain,mean)
  }
 }
}

for (d in domains)
{
  for (m in models)
  {
   data_df[[d]][[m]] <- do.call(rbind.data.frame, dataframe[[d]][[m]])
  }
}

data_df_d <- vector("list",length=length(domains))
names(data_df_d) <- domains


for (d in domains)
{
  data_df_d[[d]] <- do.call(rbind.data.frame, data_df[[d]])
}

dataframe <- cbind(data_df_d[["Social"]],"Economic" = data_df_d[["Economic"]]$Economic,"Environmental" = data_df_d[["Environmental"]]$Environmental)

#IMPLEMENTING THE FUNCTION-------------------------------------------------------------
Seed=123
for (t in years)
 {
 for (m1 in model)
  {
      N=107
      J=3
      L=1
      inits <- list(lambda=matrix(10,J,L),
                    mu=matrix(0,J,1),
                    Sigma=diag(1,J),
                    PSI=diag(N),
                    delta= rmvnorm(N,rep(0),diag(1)),
                    a= ifelse (m %in% c("1","4"), 1, 0)
      )
      set.seed(Seed + as.numeric(m) + ifelse(m %in% c("3","5"),2,1))
	  xx <- sprintf("Data/overall_%s_%s.RData",t,m1)
	  print(paste(t,m1))
      output <- simple(n_iter = 30000,burn_in = 20000,
                                      y = dataframe %>% filter(anno == t, model == m1) %>% dplyr::select(Social,Economic,Environmental) %>% as.matrix(),
                                      inits,
                                      0,10000,
                                      1/1000,1/1000,
                                      V_mu=1000,
                                      tuning=ifelse(m == "1",sqrt(1000), sqrt(10/N)),
		        tuning_lambda= 5,
                                      model = "0")
	save(output,file=xx);
  }
}


#PROCESSING THE OUTPUT OF THE FUNCTION-------------------------------------------------------------
deltaoverall <- vector("list",length=length(years))
names(deltaoverall) <- years
deltaoverall_summary <- vector("list",length=length(years))
names(deltaoverall_summary) <- years  
for (t in years)
{
  deltaoverall[[t]] <- vector("list",length=length(models))
  names(deltaoverall[[t]]) <- models
  deltaoverall_summary[[t]] <- vector("list",length=length(models))
  names(deltaoverall_summary[[t]]) <- models
}

for (t in years)
{
 for(m1 in models)
 {
   xo <- sprintf("Data/overall_%s_%s.RData",t,m1)
   load(file=xo)
    deltaoverall   <- output$delta %>% as.data.frame() 
    deltaoverall_summary[[t]][[m1]] <- multi.sapply(deltaoverall, mean, median , third_qu=function(x) quantile(x, 0.75),firt_qu= function(x) quantile(x,0.25), IQR = IQR) %>%
       as.data.frame()%>%
       mutate(
       comune = province, 
       anno=t ,
       model = m1) %>% 
       as.list()
  }
}


#CREATE THE DATASET OF OVERALL WELL BEING COMPOSITE INDICATOR
for (t in years)
{
 deltaoverall_df[[t]] <- do.call(rbind.data.frame, deltaoverall_summary[[t]])
}
save(deltaoverall_df, file = "delta_overall_df.RData")