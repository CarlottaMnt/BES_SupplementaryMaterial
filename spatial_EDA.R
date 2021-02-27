library(ggmap)
library(tmaptools)
library(tmap)
library(spdep)
library(knitr)
library(xtable)
library(plyr)
library(gtable)
library(grid)
library(gridExtra)
library(ncf)
library(geoR)
##################################################################################################################
##################################################################################################################
################################# SPATIAL-EXPLANATORY DATA ANALYSIS ##############################################
##################################################################################################################
##################################################################################################################
#In this section, we present the prior exploratory data analysis we have performed for envisaging 
#spatial association among the elementary indicators observed in the Italian provinces under analysis.
#Specifically, for each of the Italian provinces, we have computed a local indicator of the spatial 
# association through the "lisa" for testing the hypothesis of significant local spatial clusters across the Italian provinces within 
# each BES elementary indicators. 
#Moreover, for each BES elementary indicator, we computed an empirical variogram. Variograms are widely used in 
#the geostatistical analysis for exploratory purposes, to estimate covariance parameters and/or to
#compare theoretical and fitted models against sample variograms.
#By generating a variogram, we will be able to look at the variance of the differences
#of each elementary indicators among pairs of provinces at different distances.
#variogram

#Next, we can calculate a variogram using the latitude and longitude of the stations. 
#For this, we will use the variog command. We will indicate the distance intervals we wish to consider.
#Based on the summary of distances, we can look at 10 lag intervals of .15.
#To do this, we will first create a breaks vector of the endpoints of our intervals. 
#To variog, we provide our coordinate variables and the “data”, the variable of interest.  
#Then, we can look at the variog output that we will be plotting, the semi-variance,
#and the number of pairs counted in each interval. We want to check that our variogram is
#not calculating the semi-variance on small numbers of pairs.


rm(list=ls())

#Read_in the data
load("1_data.RData")

#Lisa statistic/moran statistic/p-value/variogram for each indicator and each year

lisa <- list()
moran <-list()
variogram <- list()
p_value <- list()
data.gdf <- list()
plot <- list()
for (t in years)
{
y_moran_norm <- data_norm %>% filter(anno == t) %>% select(-c(anno,comune))
y_moran <- data %>% filter(anno == t) %>% select(-c(anno,comune))
 for (i in names[[1]])
 {
 xv <- sprintf("Variogram/vario_%s_%s.png",t,i)
 lisa[[t]][[i]] <-  lisa(x = longlat04[,2], y = longlat04[,1], z = y_moran[,i], neigh = 100, resamp = 500,latlon = TRUE)
 p_value[[t]][[i]] <- round(sum(lisa[[t]][[i]]$p < 0.05) / N,3) 
 data.gdf[[t]][[i]] <- as.geodata(y_moran , coords.col = c(46, 47), data.col = i) 
 # png(file=xv)
 plot(variog(data.gdf[[t]][[i]], trend = "1st", max.dist = 6.5), main = paste0(t,"-",i),
  xlab = "Distance (lat/long degrees)", ylab = "Variance at distance")
 # dev.off()
 }
}


