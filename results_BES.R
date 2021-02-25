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
######################################################################
####################### RESULTS BES ##################################
######################################################################
#This script is created for processing the results of the Gibbs sampler
#in a more readable way. It creates posterior distribution summaries,
#plots and map. 
rm(list=ls())
#import the data
load("1_data.RData")
source("functions.R")
#Creation of empty objects to fill
##Model selection----------------------------------------------------------------

y_rep <- vector("list",length=length(domains))
sd <-  vector("list",length=length(domains))
G <- vector("list",length=length(domains))
p <- vector("list",length=length(domains))
C <- vector("list",length=length(domains))


###Factor loadings----------------------------------------------------------------
summary <- vector("list",length=length(domains))
sigma <- vector("list",length=length(domains))
f <- vector("list",length=length(domains))

#Spatial parameter
a <- vector("list",length=length(domains))

#Composite indicator
delta <- vector("list",length=length(domains))
d_summary <- vector("list",length=length(domains))
D_means <- vector("list",length=length(domains))

#Naming
names(summary) <- domains
names(sd) <- domains
names(y_rep) <- domains
names(G)<- domains
names(p) <- domains
names(C) <- domains
names(sigma) <- domains
names(f) <- domains
names(delta) <- domains
names(d_summary) <- domains
names(D_means0) <- domains

for (d in domains)
{
  y_rep[[d]] <- vector("list",length=length(models))
  names(y_rep[[d]]) <- models
  sd[[d]] <- vector("list",length=length(models))
  names(sd[[d]]) <- models
  G[[d]] <- vector("list",length=length(models))
  names(G[[d]]) <- models
  p[[d]] <- vector("list",length=length(models))
  names(p[[d]]) <- models
  C[[d]] <- vector("list",length=length(models))
  names(C[[d]]) <- models
  summary[[d]] <- vector("list",length=length(models))
  names(summary[[d]]) <- models
  sigma[[d]] <- vector("list",length=length(models))
  names(sigma[[d]]) <- models
  f[[d]] <- vector("list",length=length(models))
  names(f[[d]]) <- models
  a[[d]] <- vector("list",length=length(models))
  names(a[[d]]) <- models
  delta[[d]] <- vector("list",length=length(models[-c(5,6,7)]))
  names(delta[[d]]) <- models
  d_summary[[d]] <- vector("list",length=length(models[-c(5,6,7)]))
  names(d_summary[[d]]) <- models
  D_means[[d]] <- vector("list",length=length(models[-c(5,6,7)]))
  names(D_means[[d]]) <- models
  for (m in models)
  {
    y_rep[[d]][[m]] <- vector("list",length=length(years))
    names(y_rep[[d]][[m]]) <- years
    sd[[d]][[m]] <- vector("list",length=length(years))
    names(sd[[d]][[m]]) <- years
    G[[d]][[m]] <- vector("list",length=length(years))
    names(G[[d]][[m]]) <- years
    p[[d]][[m]] <- vector("list",length=length(years))
    names(p[[d]][[m]]) <- years
    C[[d]][[m]] <- vector("list",length=length(years))
    names(C[[d]][[m]]) <- years
     summary[[d]][[m]] <- vector("list",length=length(years))
    names(summary[[d]][[m]]) <- years
    sigma[[d]][[m]] <- vector("list",length=length(years))
    names(sigma[[d]][[m]]) <- years
    f[[d]][[m]] <- vector("list",length=length(years))
    names(f[[d]][[m]]) <- years
    a[[d]][[m]] <- vector("list",length=length(years))
    names(a[[d]][[m]]) <- years
    delta[[d]][[m]] <- vector("list",length=length(years))
    names(delta[[d]][[m]]) <- years
    d_summary[[d]][[m]] <- vector("list",length=length(years))
    names(d_summary[[d]][[m]]) <- years
    D_means[[d]][[m]] <- vector("list",length=length(years))
    names(D_means[[d]][[m]]) <- years
  }
}
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
          y_rep[[d]][[m]][[t]] <- apply(output$y_rep,2,mean) %>% matrix(N,J,byrow=TRUE)
          sd[[d]][[m]][[t]] <- apply(output$y_rep,2,var) %>% matrix(N,J,byrow=TRUE)
          G[[d]][[m]][[t]] <- sum(rowSums((y_rep[[d]][[m]][[t]]- y_norm[[d]][[t]])^2))
          p[[d]][[m]][[t]] <- sum(rowSums(sd[[d]][[m]][[t]]))
          C[[d]][[m]][[t]] <- G[[d]][[m]][[t]] + p[[d]][[m]][[t]]
          Sigma <- output$Sigma %>% as.data.frame()   
          lambda <- output$lambda %>% as.data.frame()
          foutput <- (lambda ^ 2) /(lambda^2+Sigma) %>% as.data.frame()
         f[[d]][[m]][[t]] <- rbind(apply(foutput, 2,mean),apply(foutput,2, quantile,c(.025, .50, .975)),apply(foutput,2, IQR))
         colnames(f[[d]][[m]][[t]]) <- ord_name[[d]][["2017"]]
         f[[d]][[m]][[t]] <- f[[d]][[m]][[t]] %>%
                                             as.data.frame() %>%
			mutate(
			anno=t, 
			model = m,
			row_names = c("mean", "first","median","third","IQR")) %>%       
			as.list()         
          summary[[d]][[m]][[t]] <- rbind(apply(lambda, 2,mean),apply(lambda,2, quantile,c(.025, .50, .975)),apply(lambda,2, IQR)) 
          colnames(summary[[d]][[m]][[t]]) <- ord_name[[d]][["2017"]]      
          summary[[d]][[m]][[t]] <- summary[[d]][[m]][[t]] %>%
                                             as.data.frame() %>%
			mutate(
			anno=t, 
			model = m,
			row_names = c("mean", "first","median","third","IQR")) %>%       
			as.list()   
          
          sigma[[d]][[m]][[t]] <- rbind(apply(Sigma, 2,mean),apply(Sigma,2, quantile,c(.025, .50, .975)),apply(Sigma,2, IQR))
          colnames(sigma[[d]][[m]][[t]]) <-  ord_name[[d]][["2017"]]
          a[[d]][[m]][[t]] <- output$a%>% 
			as.data.frame() %>% 
			dplyr::summarise(min=min(.),first_qu = quantile(.,0.25),
			mean=mean(.),median=median(.),third_qu=quantile(.,0.75),
			max=max(.),sd=sd(.),IQR=IQR(.),year=t,model= m,domain = d) %>% 
			as.list()	          
          delta[[d]][[m]][[t]] <- output$delta %>% as.data.frame()
          d_summary[[d]][[m]][[t]] <- multi.sapply(delta[[d]][[m]][[t]], mean, median , third_qu=function(x) quantile(x, 0.75),firt_qu= function(x) quantile(x,0.25), IQR = IQR) %>%
			as.data.frame()%>%
			mutate(
			comune = province,
			anno=t ,
			model = m
			) %>% 
			as.list()
         D_means[[d]][[m]][[t]] <- apply(output$D,2,mean)   		
          
     }                  
  }
}



#Plot Factor loadings

plot_f <- vector("list",length=length(domains))
names(plot_f) <- domains
f_df <- vector("list",length=length(domains))
names(f_df) <- domains
summary_df <- vector("list",length=length(domains))
names(summary_df) <- domains

for (d in domains)
{
  plot_f[[d]] <- vector("list",length=length(models))
  names(plot_f[[d]]) <- models1
  f_df[[d]] <- vector("list",length=length(models))
  names(f_df[[d]]) <- models1
  summary_df[[d]] <- vector("list",length=length(models))
  names(summary_df[[d]]) <- models1
  for (m in models)
  {
    plot_f[[d]][[m]] <- vector("list",length=length(years))
    names(plot_f[[d]][[m]]) <- years
  }
}
#SQUARED CORRELATION---------------------------------------------------------------------------------------
for (d in domains)
{
  for (m in models)
  {
   f_df[[d]][[m]] <- do.call(rbind.data.frame, f[[d]][[m]])
  }
}
f_df_d <- vector("list",length=length(domains))
names(f_df_d) <- domains

for (d in domains)
{
  f_df_d[[d]] <- do.call(rbind.data.frame, f_df[[d]])
}

#FACTOR LOADINGS---------------------------------------------------------------------------------------

for (d in domains)
{
  for (m in models)
  {
   summary_df[[d]][[m]] <- do.call(rbind.data.frame, summary[[d]][[m]])
  }
}
summary_df_d <- vector("list",length=length(domains))
names(f_df_d) <- domains

for (d in domains)
{
  summary_df_d[[d]] <- do.call(rbind.data.frame, summary_df[[d]])
}

width_scale <- 12
for (d in domains)
{
  {
   for (t in years)
   {
    plot_f[[d]][[t]] <- summary_df_d[[d]]%>% 
    clean_names() %>%     
    filter(anno == t)%>%
    tidyr::gather(Indicatore, value,-c(anno,model,row_names)) %>%
    tidyr::spread(row_names, value) %>%
    mutate(xmin = pmin(first,third),xmax = pmax(first,third)) %>%
      ggplot(aes(y= reorder(Indicatore, mean),x = mean, group = model))+
      geom_bar(aes(fill = model), stat = "identity",alpha = 0.5,width=0.75, position ='dodge')+
      geom_point(position = position_dodge(0.75), alpha = 0.8, size = 2 )+
      geom_errorbar(aes(xmin=xmin, xmax= xmax), lty=1, alpha = 0.5,width = 0.2, position=position_dodge(0.75))+
      geom_vline(xintercept=0, linetype="dashed")+
      xlab(paste0(t,"\n Factor Loadings"))+
      ylab("")+
      theme_light() +
      scale_fill_discrete(name  = "Models",
                           breaks = c("0", "1","2","3"),
                           labels = c("Spatial Independence","Marginal correlation", "CAR 3A", "CAR 3B"),
	            guide = guide_legend()) +
      scale_shape_discrete(name  = "Models",
                           breaks = c("0", "1","2","3"),
                           labels = c("Spatial Independence","Marginal correlation", "CAR 3A", "CAR 3B"))+
      scale_y_discrete(label = function(x) stringr::str_trunc(x, 12))+
      theme(strip.background = element_rect(fill = NA, color = "black"),
            plot.caption = element_text(hjust = 1),
            legend.position="bottom",
            axis.text.y =  element_text(size = rel(6)),
            axis.text.x =  element_text(size = rel(6)),
            axis.title.x = element_text(size = rel(6)),
            legend.text = element_text(size = rel(5.5)),
            legend.title = element_text(size = rel(6)),
            axis.ticks.y=element_blank())
  }
 }
}

#SPATIAL PARAMETER--------------------------------------------------------------
#DATA FRAME SPATIAL PARAMETER
a_df <- vector("list",length=length(domains))
a_df_d <- vector("list",length=length(domains))
names(a_df) <- domains
names(a_df_d) <- domains
for (d in domains)
{
  a_df[[d]] <- vector("list",length=length(models[-1]))
  names(a_df[[d]]) <-  models[-1]
}
for (d in domains)
{
  for (m in models[-1])
  {
    a_df[[d]][[m]] <- do.call(rbind.data.frame, a[[d]][[m]])
  }
}
for (d in domains)
{
  a_df_d[[d]] <- do.call(rbind.data.frame, a_df[[d]])
}

#PLOT---------------------------------------------------------------------------------
a_df_d_tot <- do.call(rbind.data.frame, a_df_d)
a_df_d_tot$model <- plyr::revalue(x = a_df_d_tot$model, 
c("1" = "Marginal correlation", "2" = "CAR 3A", "3" = "CAR 3B"))

pd <- position_dodge(0.1) 

p <- ggplot(a_df_d_tot %>% filter(model %in% c("Marginal correlation","CAR 3A","CAR 3B")) ,aes(y=mean,x=year,col = domain))+
    geom_errorbar(aes(ymin=first_qu, ymax=third_qu), width=.4,lty=1, position=pd) +
    geom_point(size=1)+
    facet_wrap(. ~ model, scales="free_y",ncol= 1)+
    geom_line(aes(colour = domain, linetype = domain,group = domain)) +
    geom_hline(yintercept=0, linetype="dashed")+
    ylab(expression(omega))+
    theme_bw() +
    theme(strip.background = element_rect(fill = NA, color = "black"),
          axis.text.x = element_text(size = 8, angle = 45,vjust=0.5),
          plot.caption = element_text(hjust = 1))
#COMPOSITE INDICATORS----------------------------------------------------------------
#PLOT
plot_delta <- vector("list",length=length(domains))
names(plot_delta) <- domains
delta_df <- vector("list",length=length(domains))
names(delta_df) <- domains
for (d in domains)
{
  plot_delta[[d]] <- vector("list",length=length(models))
  names(plot_delta[[d]]) <- models
  delta_df[[d]] <- vector("list",length=length(models))
  names(delta_df[[d]]) <- models
  for (m in models)
  {
    plot_delta[[d]][[m]] <- vector("list",length=length(years))
    names(plot_delta[[d]][[m]]) <- years
  }
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

for (d in domains)
{
  {
   for (t in years[years %in% c("2008")])
   {
    plot_delta[[d]][[t]] <- delta_df_d[[d]]%>%     
      filter(anno == t) %>%
      #comune %in% c("Milano","Roma","Torino","Genova","Palermo","Cagliari",
                                                # # "Trento","Venezia","Perugia","Potenza","Bari",
                                              # # "Napoli","Bologna","Firenze","Aosta","Reggio di Calabria",
                                              # # "Ancona","Trieste","Campobasso","Catanzaro"))%>%
      ggplot(aes(y= reorder(comune, mean),x = mean, fill = model))+
      geom_bar(stat = "identity",alpha = 0.5, position = position_dodge())+
      geom_point(position = position_dodge(0.9), alpha = 0.8, size = 2 )+
      geom_errorbar(aes(xmin=firt_qu, xmax=third_qu), width=.2,lty=1,alpha = 0.5, position=position_dodge(1))+
      geom_vline(xintercept=0, linetype="dashed")+
      xlab(paste0(t,"\nComposite indicator\n",d))+
      ylab("")+
      theme_bw() +
      scale_fill_discrete(name  = "Models",
                           breaks = c("0", "1","2","3"),
                           labels = c("Spatial Independence","Marginal correlation", "CAR 3A", "CAR 3B"),
	            guide = guide_legend()) +
      scale_shape_discrete(name  = "Models",
                           breaks = c("0", "1","2","3"),
                           labels = c("Spatial Independence","Marginal correlation", "CAR 3A", "CAR 3B"))+
      theme(strip.background = element_rect(fill = NA, color = "black"),
            plot.caption = element_text(hjust = 1),
            legend.position="bottom",
            axis.text.y =  element_text(size = rel(3)),
            axis.text.x =  element_text(size = rel(3)),
            axis.title.x = element_text(size = rel(3)),
            legend.text = element_text(size = rel(3.5)),
            legend.title = element_text(size = rel(3)),
            axis.ticks.y=element_blank())
  }
 }
}

##MAPS----------------------------------------------------------------------------------------------------
VARmap <- vector("list",length=length(domains))
names(VARmap) <- domains
map <- vector("list",length=length(domains))
names(map) <- domains
for (d in domains)
{
  VARmap[[d]] <- vector("list",length=length(models))
  names(VARmap[[d]]) <- models
  map[[d]] <- vector("list",length=length(models))
  names(map[[d]]) <- models
  for (m in models)
  {
    VARmap[[d]][[m]] <- vector("list",length=length(years))
    names(VARmap[[d]][[m]]) <- years
  }
}

for (d in domains)
{ 
  for (m in models)
  {
   for (t in years)
   {
   VARmap[[d]][[m]][[t]] <- merge(italgeo,delta_df_d[[d]] %>% filter(model == m ,anno == t),by = "comune")
   }
  }
}

for (d in domains)
{
 for (m in models)
 {
  VARmap[[d]][[m]] <- rbind(VARmap[[d]][[m]][["2004"]],VARmap[[d]][[m]][["2006"]],VARmap[[d]][[m]][["2008"]],VARmap[[d]][[m]][["2011"]],VARmap[[d]][[m]][["2014"]],VARmap[[d]][[m]][["2017"]])
  map[[d]][[m]] <- tm_shape(VARmap[[d]][[m]])+
  tm_fill("mean",title= paste0(d,m), style = "quantile", midpoint = NA)+
  tm_borders(alpha=.5)+
  tm_facets(by=c("anno"), ncol  =2, showNA = FALSE)+
  tm_shape(VARmap[[d]][[m]] %>%  filter(comune %in% c("Milano","Roma","Torino","Genova","Palermo","Cagliari",
                                                 "Trento","Venezia","Perugia","Potenza","Bari",
                                                 "Napoli","Bologna","Firenze","Aosta","Reggio di Calabria",
                                                 "Ancona","Trieste","Campobasso")))+
  tm_dots("comune",size=0.4, col="black", shape= 21, legend.show = FALSE)
  }
}



