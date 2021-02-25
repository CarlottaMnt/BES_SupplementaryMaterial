setwd("C:/Users/carlo/Documents/ISTAT_project/ISTA_project")
# install.packages("spdep")
# install.packages("knitr")
# install.packages("bestNormalize")
# install.packages("dlm")
# install.packages("xtable")
#install.packages("rstan")

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
library(grid)
library(gridExtra)
library(rstan)
library(ncf)
#devtools::install_github("karthik/wesanderson")
#library(wesanderson)
rm(list=ls())

#Read_in the data
data<- read.csv("BES_data/Imputed_BES_dataset.csv")
data <- data %>% 
  mutate(anno=sapply(strsplit(Anno, split='_', fixed=TRUE), function(x) (x[2]))) %>% 
  select(-Anno)
location <- readxl::read_xlsx("BES_data/Location_BES.xlsx")
data <- cbind(data,location)
data_norm <- read.csv("BES_data/Imputed_BES_normalized.csv")

#factor_eco <- read.csv("factor_econ.csv")
italgeo <-  sf::st_read("BES_data/italgeo.shp")
italgeo$DEN_UTS <- as.character(italgeo$DEN_UTS)
#English names
names <- vector("list",length=4)
names(names) <- c("Overall","Social","Economic","Environmental")
names[[1]] <-  c("Employees in cultural business",
                 "Prison density", "Other reported crimes","Youth (<40 years old) political representation in municipalities",
                 "Women' s political representation in municipalities","Collection capacity: provincial governments",
                 "Children who benefited of early childhood services","Collection capacity: municipalities",
                 "Landfill of urban waste","Widespread crimes reported","Density of historical green areas",
                 "Dissemination of holidays farm","Availability of urban green areas","Regional health services outflows (hospital admittances)",
                 "Energy from renewable sources","Working days of paid (employee)",
                 "People not in education, employment, or training (Neet)","Average yearly per capita pension income",
                 "Irregular electricity services","People having completed tertiary education (25-34 years)",
                 "Graduates mobility (25-39 years)","Infant mortality rate",
                 "Age-standardised mortality rate for dementia and nervous diseases",
                 "Roads accidents mortality rate (15-34 years old)","Age-standardised cancer mortality rate",
                 "Mortality rate in extra-urban road accidents","Homicide rate","Participation in long-life learning",
                 "Participation to childhood school",
                 "Electoral participation (european elctions)",
                 "Electoral participation (regional elections)",
                 "Per capita asset","Pensioners with low-pension",
                 "People with at least upper secondary education level (25-64 years)",
                 "Public transport network","Separate collection of municipal waste",
                 "Average disposable per capita income",
                 "Average yearly earnings of employee",
                 "Life expectancy at birth","Mortal accidents and inabilities rate",
                 "Rate of bank' s non-performing loans to households","Non-participation rate",
                 "Youth non-participation rate (15-29 years)","Employment rate(20-64 years)",
                 "Youth employment rate (15-29 years)"
)%>% janitor::make_clean_names()
names[[2]] <- c(  "Life expectancy at birth","Infant mortality rate",
                   "Roads accidents mortality rate (15-34 years old)",
                   "Employees in cultural business",
                   "Other reported crimes",
                   "Age-standardised cancer mortality rate",
                   "Youth (<40 years old) political representation in municipalities",
                   "Age-standardised mortality rate for dementia and nervous diseases",
                   "Participation in long-life learning",
                   "People with at least upper secondary education level (25-64 years)",
                   "People not in education, employment, or training (Neet)",
                   "People having completed tertiary education (25-34 years)",
                   "Participation to childhood school",
                   "Electoral participation (european elctions)",
                   "Electoral participation (regional elections)",
                   "Women' s political representation in municipalities",
                   "Prison density",
                   "Graduates mobility (25-39 years)",
                   "Collection capacity: municipalities",
                   "Collection capacity: provincial governments",
                   "Homicide rate",
                   "Widespread crimes reported",
                   "Mortality rate in extra-urban road accidents",
                   "Children who benefited of early childhood services",
                   "Irregular electricity services",
                   "Public transport network",
                   "Regional health services outflows (hospital admittances)",
                   "Dissemination of holidays farm",
                   "Density of historical green areas")%>% janitor::make_clean_names()
names[[3]] <- c("Average disposable per capita income",
                   "Average yearly earnings of employee",
                   "Average yearly per capita pension income",
                   "Pensioners with low-pension",
                   "Per capita asset",
                   "Rate of bank' s non-performing loans to households",
                   "Employment rate(20-64 years)",
                   "Non-participation rate",
                   "Youth non-participation rate (15-29 years)",
                   "Youth employment rate (15-29 years)",
                   "Working days of paid (employee)",
                   "Mortal accidents and inabilities rate"
                   
) %>% janitor::make_clean_names()
names[[4]] <- c("Landfill of urban waste",
                  "Availability of urban green areas",
                  "Energy from renewable sources",
                  "Separate collection of municipal waste"
)%>%  janitor::make_clean_names()


#Import data-set for sustainable development domains
#social <- read.csv("economic_BES_normalized.csv")
#economic <- read.csv("Economic_BES_normalized.csv")
#environment <- read.csv("Environment_BES_normalized.csv")
#Non normalized data
colnames(data)[2:46] <- names[[1]]
colnames(data_norm)[1:45] <- names[[1]]
#colnames(economic)[1:26] <- economic_name
#colnames(economic)[1:12] <- economic_name
#colnames(environment)[1:4] <- environ_name
#Remove absent provinces
data <- data %>% filter(!(comune %in% c("Olbia-Tempio","Medio Campidano","Ogliastra")))
data <- data %>% 
  mutate(comune= case_when(
    grepl("ForlÃ¬-Cesena",comune) ~ "Forli'-Cesena",
    grepl("Carbonia-Iglesias",comune)~ "Sud Sardegna",
    TRUE ~ as.character(comune)))
data_norm <- data_norm %>% 
  mutate(anno = data$anno, comune=data$comune)
#Create data-set for each well-being domains
#Italy geo locations
italgeo <- italgeo %>% 
  mutate(comune=DEN_UTS)
#Remove unnecessary columns
italgeo <- italgeo %>% 
  dplyr:: select(-c("COD_CM","TIPO_UTS","DEN_PROV","SIGLA","COD_RIP"))
#Replace names
italgeo <- italgeo %>% 
  mutate(comune=case_when(
    grepl("Massa Carrara",comune)~ "Massa-Carrara",
    grepl("Bolzano",comune)~ "Bolzano Bozen",
    TRUE ~ as.character(comune)
  ))
italgeo <- italgeo[order(italgeo$comune),]

#Spatial correlation matrices---------------------------------------------------
#Model 2
longlat04 <- data %>% 
  filter(anno=="2004") %>% 
  dplyr::select(`data$long`,`data$lat`) %>% as.matrix()
latlong04 <- data %>% 
  filter(anno=="2004") %>% 
  dplyr::select(`data$lat`,`data$long`) %>% as.matrix()
PSI_04 <- as.matrix(dist(longlat04))#Euclidean Distance
PSI_04.inv <- 1/PSI_04
diag(PSI_04.inv) <- 0

#Model CAR 2-3
knn1 <- knearneigh(longlat04)
k1 <- knn2nb(knn1)
critical.threshold <- max(unlist(nbdists(k1,longlat04)))
nb.dist.band <- dnearneigh(longlat04, 0, critical.threshold)
summary(nb.dist.band)
plot(nb.dist.band, longlat04, lwd=.2, col="blue", cex = .5)
R <- nb2mat(nb.dist.band, style='B',zero.policy=TRUE)
N <- dim(R)[1]

#Create the matrix for model 3B
o_b<- rep(NA,N)
Xi <- eigen(R)
xi <- sort(Xi$values)
xi_inv <- 1/(xi)
#if (xi_inv[1] > x  | x > xi_inv[N])
#  x=0.12
for (i in 1:N){
  o_b[i]=length(nb.dist.band[[i]])
}
O_b=diag(o_b)

W_b=matrix(0,N,N)
for (i in 1:N)
{
  for (j in 1:N)
  {
    if (j %in% nb.dist.band[[i]])
      W_b[i,j] = sqrt(o_b[i]*o_b[j])
    else
      W_b[i,j]= 0
  }
}
#Model 3C
o_c <- rep(NA,N)
W_c = matrix(0,N,N)
for (i in 1:N){
  o_c[i]=sum(exp(-x*PSI_04[i,]))
}
O_c=diag(o_c)
for (i in 1:N)
{
  W_c[i,] = exp(-x*PSI_04[i,])
}

Diff = O_c - W_c
diag(Diff) <- 1
PSI = solve(Diff)
return(dmvnorm(t(param$delta),rep(0,N),PSI)*
         dnorm(x,mu_a,V_a)* dnorm(mu_x,x,sqrt(tuning)))
#Model 3D
PSI_04_d <- nbdists(w, coords= longlat04,longlat = TRUE) #Crate the list of neighbors


#Lisa statistic/moran statistic and variogram

lisa <- list()
moran <-list()
variogram <- list()
p_value <- list()
for (t in c("2004","2006","2008","2014","2017"))
{
y_moran_norm <- data_norm %>% filter(anno == t)
y_moran <- data %>% filter(anno == t)
for (i in names[[1]])
{
lisa[[t]][[i]] <-  lisa(x = latlong04[,1], y = latlong04[,2], z = y_moran[,i], neigh = 100, resamp = 500,latlon = TRUE)
p_value[[t]][[i]] <- round(sum(lisa[[t]][[i]]$p < 0.05) / N,3) 
}
}
p_value_df <- list()
for (t in c("2004","2006","2014","2017"))
{
  p_value_df[[t]] <- do.call(rbind.data.frame, p_value[[t]])
}
p_value_df <- rbind(p_value[["2004"]],p_value[["2006"]],p_value[["2008"]],p_value[["2014"]],p_value[["2017"]])

kable(t(p_value_df %>% as.data.frame()), format = "latex", digits = 3)