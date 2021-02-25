library(ggmap)
library(tmaptools)
library(tmap)
library(tidyverse)
library(tidybayes)
library(spdep)
library(knitr)
library(bestNormalize)
library(dlm)
library(xtable)
library(plyr)
library(gtable)
library(data.table)
library(grid)
library(gridExtra)
library(janitor)
library(rstan)

##################################################################################################################
##################################################################################################################
###################################### HIERARCHICAL MODEL #########################################################################################
##################################################################################################################

rm(list=ls())
#import the data
source("1_data.RData")
source("functions.RData")
load("delta_df_d.RData")
load("deltaoverall_df_d.RData")

#COMBINE THE RESULTS FOR UNIFIED DATAFRAME

##Create the groups of province

N_O <- c("Savona","Imperia","La Spezia","Genova",
         "Cuneo","Biella","Vercelli","Alessandria",
         "Asti","Verbano-Cusio-Ossola","Novara","Torino",
         "Bergamo","Brescia","Como","Lecco","Cremona","Lodi",
         "Mantova","Milano","Monza e della Brianza","Pavia","Sondrio","Varese",
         "Aosta")
N_E <- c("Bologna","Forli-Cesena","Ferrara","Modena",
         "Parma","Piacenza","Ravenna","Reggio nell'Emilia",
         "Rimini","Gorizia","Pordenone","Trieste","Forlì-Cesena",
         "Udine","Bolzano Bozen","Trento","Belluno","Padova","Rovigo",
         "Treviso","Venezia","Verona","Vicenza")

CE <- c("Frosinone","Latina","Rieti","Roma",
        "Viterbo","Arezzo","Firenze","Grosseto",
        "Livorno","Lucca","Massa-Carrara","Pisa",
        "Pistoia","Prato","Siena","Perugia","Terni","Ancona",
        "Ascoli Piceno","Fermo","Macerata","Pesaro e Urbino"
)
SUD <- c("Cosenza","Crotone","Reggio di Calabria",
  "Vibo Valentia","Avellino","Benevento","Caserta","Napoli",
  "Salerno","Campobasso","Isernia","L'Aquila","Pescara","Teramo","Bari",
  "Barletta-Andria-Trani","Chieti", "Foggia","Lecce","Matera","Potenza" ,
  "Taranto","Brindisi")

ISOLE <- c("Cagliari","Nuoro","Oristano","Sassari",
           "Agrigento","Sud Sardegna","Caltanissetta","Catania","Enna",
           "Messina","Palermo","Ragusa","Siracusa",
           "Trapani","Catanzaro")
##-----------------------------------------------------------------------------------------------------------
mean_common_summary <- list()        

for(d in domains)
{
 for (t in years)
 {
  for (m in models)
  {
   mean_common_summary[[d]][[t]][[m]] <-  delta_df_d[[d]]%>%
			as.data.frame() %>%
			filter(model == m, anno == t) %>% 
			mutate(area = ifelse(comune %in% N_O, "1",
                                                     ifelse(comune %in% N_E, "2",
                                                            ifelse(comune %in% CE, "3",
                                                                   ifelse(comune %in% SUD, "4",
                                                                          ifelse(comune %in% ISOLE, "5",
                                                                                 "2"))))))
  }
 }
}
##-----------------------------------------------------------------------------------------------------------
mean_commonoverall_summary <- list()        
for (t in years)
{
 for (m1 in models[models %in% models])
 {
  mean_commonoverall_summary[[t]][[m1]] <- deltaoverall_df_d %>%
			as.data.frame() %>%
			filter(model == m1, anno == t) %>% 
			mutate(area = ifelse(comune %in% N_O, "1",
                                                     ifelse(comune %in% N_E, "2",
                                                            ifelse(comune %in% CE, "3",
                                                                   ifelse(comune %in% SUD, "4",
                                                                          ifelse(comune %in% ISOLE, "5",
                                                                                 "2"))))))
 }
}


#Overall well being by macro area-------------------------------------------------------------------------
model <- list()
for (t in years)
{
 for (m1 in models)
 { 
   xh <- sprintf("Data/hierarchicaloverall_%s_%s.RData",t,m1)
   print(paste(t,m1))
   y <-  mean_commonoverall_summary[[t]][[m1]]
  stan_data <- list(
    N=dim(y)[1],
    y= y$mean,
    jj=as.numeric(y$area),
    J=5
  )
  # model_code <- "data{
      # int<lower=1> J;//number of groups
      # int<lower=1> N;
      # real y[N];
      # int<lower=1,upper=J> jj[N];
    # }
    # parameters {
    # real mu[J];
    # real<lower=0> Sigma[J];
    # real eta;
    # real<lower=0>nu;
    # }
    # transformed parameters{
    # real lognu;
    # lognu =log(nu);
    # }
    # model {
    # for (i in 1:J) 
    # {
    # mu[i]~ normal(0, 1);
    # Sigma[i] ~ cauchy(eta, nu);
    # }
    # for (n in 1:N)
    # y[n] ~ normal(mu[jj[n]],Sigma[jj[n]]);
    # }
    # generated quantities{
    # real y_pred[N];
    # real log_lik[N];
    # for (n in 1:N)
    # y_pred[n]=normal_rng(mu[jj[n]],1);//fitted values
    # for (n in 1:N)
    # log_lik[n]=normal_lpdf(y[n]|mu[jj[n]],1);
    # }
  # "
  # stan_model <- stan_model(model_code=model_code)

model <- sampling(stan_model,
                      data= stan_data,
                      chains=4,
                      iter=6000,
                      control=list(adapt_delta=0.95,
                                   max_treedepth = 15
                      ))

samples <- extract(model, c("mu","Sigma","y_pred"))	
save(samples,file=xh);   
 }    
}

#Socio-Economic-Environmemtal well being by macro area---------------------------------------------------
for(d in domains)
{ 
 for (m in models)
 {
  for (t in years)
  { 
   xh <- sprintf("Data/hierarchical_%s_%s.RData",d,m,t)
   print(paste(t,m1))
   y <- mean_common_summary[[d]][[t]][[m1]]
  stan_data <- list(
    N=dim(y)[1],
    y= y$mean,
    jj=as.numeric(y$area),
    J=5
  )
  # model_code <- "data{
      # int<lower=1> J;//number of groups
      # int<lower=1> N;
      # real y[N];
      # int<lower=1,upper=J> jj[N];
    # }
    # parameters {
    # real mu[J];
    # real<lower=0> Sigma[J];
    # real eta;
    # real<lower=0>nu;
    # }
    # transformed parameters{
    # real lognu;
    # lognu =log(nu);
    # }
    # model {
    # for (i in 1:J) 
    # {
    # mu[i]~ normal(0, 1);
    # Sigma[i] ~ cauchy(eta, nu);
    # }
    # for (n in 1:N)
    # y[n] ~ normal(mu[jj[n]],Sigma[jj[n]]);
    # }
    # generated quantities{
    # real y_pred[N];
    # real log_lik[N];
    # for (n in 1:N)
    # y_pred[n]=normal_rng(mu[jj[n]],1);//fitted values
    # for (n in 1:N)
    # log_lik[n]=normal_lpdf(y[n]|mu[jj[n]],1);
    # }
  # "
  # stan_model <- stan_model(model_code=model_code)

model <- sampling(stan_model,
                      data= stan_data,
                      chains=4,
                      iter=6000,
                      control=list(adapt_delta=0.95,
                                   max_treedepth = 15
                      ))

samples <- extract(model, c("mu","Sigma","y_pred"))	
save(samples,file=xh);   
  }    
 }
}



mean_area_overall <- vector("list",length=length(domains))
deltarea_df <- vector("list",length=length(domains))
graphdf <- vector("list",length=length(domains))
names(mean_area_overall) <- domains
names(deltarea_df) <- domains
names(graphdf) <- domains

for (d in domains)
{
  mean_area_commoverall <- vector("list",length=length(years))
  names(mean_area_commoverall) <- years 
  deltarea_df_common <- vector("list",length=length(years))
  names(deltarea_df_common) <- years 
  mean_area_overall[[d]] <- vector("list",length=length(years))
  deltarea_df[[d]] <- vector("list",length=length(years))
  names(mean_area_overall[[d]]) <- years 
  names(deltarea_df[[d]]) <-  years 
  for (t in years)
  {
   mean_area_overall[[d]][[t]] <- vector("list",length=length(models))
   names(mean_area_overall[[d]][[t]]) <- models
   mean_area_commoverall[[t]] <- vector("list",length=length(models))
   names( mean_area_commoverall[[t]]) <- models
   deltarea_df[[d]][[t]] <- vector("list",length=length(models))
   names(deltarea_df[[d]][[t]]) <- models
   deltarea_df_common[[t]] <- vector("list",length=length(models))
   names(deltarea_df_common[[t]]) <- models
  }
}


for (d in domains)
{
    for (t in years)
    {
     for (m in models)
     {
      xh <- sprintf("Data/hierarchical_%s_%s_%s.RData",d,t,m)
      load(file=xh)
      mean_area <- samples[["mu"]] %>% 
      as.data.frame() 
      mean_area_overall[[d]][[t]][[m]] <- multi.sapply(mean_area, mean, median , third_qu=function(x) quantile(x, 0.75),firt_qu= function(x) quantile(x,0.25), IQR = IQR) %>%
      as.data.frame() %>%
      mutate(area = c("North-West","North-East","Center","South","Islands"),
      anno=t, model = m) %>% 
      as.list()   
    }
  }
}

for (t in years)
{
  for (m1 in models)
  {
      xh <- sprintf("Data/hierarchicaloverall_%s_%s.RData",t,m1)
      load(file=xh)
      mean_area <- samples[["mu"]] %>% 
      as.data.frame() 
      mean_area_commoverall[[t]][[m1]] <- multi.sapply(mean_area, mean, median , third_qu=function(x) quantile(x, 0.75),firt_qu= function(x) quantile(x,0.25), IQR = IQR) %>%
      as.data.frame() %>%
      mutate(area = c("North-West","North-East","Center","South","Islands"),
      anno=t, model = m1) %>% 
      as.list()   
  }
}

