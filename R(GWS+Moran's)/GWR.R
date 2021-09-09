library(tmap)
library(readr)
library(rgdal)
library(ggplot2)
library(ggmap)
library(maptools)
library(maps)
library(dplyr)
library(viridis)
library(mapproj)
library(spdep)
library(car)
library(spgwr)


chinaSHP <- readOGR("exe/china.shp")# Read SHP file
qtm(chinaSHP)
dataWindow <- read_csv("data.csv") # Read data

model <- lm(`Confirmed numbers` ~ `Economic aggregate`+`Threetune population`+`Passenger volume`
            +`Urbanization rate`,data=dataWindow) # OLS model
summary(model)
plot(model) # Plot result
durbinWatsonTest(model) #Autocorrelation

dataWindow$residuals <- residuals(model) 
qplot(dataWindow$residuals) + geom_histogram() 


#Function to plot residuals
bubblefunc <- function(myData,size,name,myBreaks){
  China <- chinaSHP
  map <- 
    ggplot() +
    geom_polygon(data=China,aes(long, y = lat, group = group), fill="black", alpha=0.3) +
    geom_point(data=myData, aes(x=longitude, y = latitude, size=size, color=size, alpha=size), shape=20, stroke=FALSE) +
    scale_size_continuous(name=name,  trans="identity",range=c(1,12),breaks=myBreaks) +
    scale_alpha_continuous(name=name, trans="identity", range=c(0.1, 0.6),breaks=myBreaks) +
    scale_color_viridis(option="magma", trans="identity", name=name,breaks=myBreaks ) + 
    theme_void()  + coord_map() +  xlim(70,149)+ ylim(18,55)+
    guides( colour = guide_legend()) +
    
    theme(
      legend.position = c(0.9, 0.8),
      text = element_text(color = "#22211d",size=15), # Choose colour
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#FFFFFF", color = NA), 
      legend.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    )
  plot(map)
}
bubblefunc(dataWindow,dataWindow$residuals,"Residuals",c(-1000,-500,0,500,1000)) # Plot residuals


xy=dataWindow[,c(13,14)]
chinasf <- st_as_sf(x=xy,coords = c("longitude", "latitude"))
chinasp <- as(chinasf, "Spatial") # Transform to "spatial"

coordsW <- coordinates(chinasp) # calculate the centroids of all Wards in London
plot(coordsW)
knn_wards <- knearneigh(coordsW, k=4)# nearest neighbours
LWard_knn <- knn2nb(knn_wards)
plot(LWard_knn, coordinates(coordsW), col="blue") #Plot
#create a spatial weights matrix object from  weight
Lward.knn_4_weight <- nb2listw(LWard_knn, style="C")
#moran's I test on the residuals
moran.test(dataWindow$residuals, Lward.knn_4_weight)


#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(`Confirmed numbers` ~ `Economic aggregate`+`Threetune population`+`Passenger volume`
                     +`Urbanization rate`,data=dataWindow,coords=coordsW,adapt=T)
#run the gwr model
GWRModel <- gwr(`Confirmed numbers` ~ `Economic aggregate`+`Threetune population`+`Passenger volume`
                +`Urbanization rate`,data=dataWindow,coords=coordsW,adapt=GWRbandwidth,
                hatmatrix = TRUE,se.fit = TRUE)
GWRModel #print the results of the model
results<-as.data.frame(GWRModel$SDF)
names(results)


#attach coefficients to original dataframe
dataWindow$coeaggregate <- results$X.Economic.aggregate.
dataWindow$coepopulation <- results$X.Threetune.population.
print(dataWindow$coepopulation)
dataWindow$coevolume <- results$X.Passenger.volume.
dataWindow$coerate <- results$X.Urbanization.rate.


# Plot the coefficient
bubblefunc(dataWindow,dataWindow$coeaggregate,'coeaggregate',c(0,0.01, 0.02,0.03, 0.04))
bubblefunc(dataWindow,dataWindow$coepopulation,'coepopulation',c(-0.000001,-0.000005,-0.00001,-0.000015,-0.00002, -0.00005))
bubblefunc(dataWindow,dataWindow$coevolume,'coevolume',c( 0,0.01,0.02,0.03,0.04, 0.05))
bubblefunc(dataWindow,dataWindow$coerate,'coerate',c(0,500,1000,1500,2000,2500))

#statistically significant test
sigTest_Temp = abs(GWRModel$SDF$"`Economic aggregate`") - 2 * GWRModel$SDF$"`Economic aggregate`_se"
dataWindow$GWRTemp <- sigTest_Temp


sigTest_Wind = abs(GWRModel$SDF$"`Threetune population`") - 2 * GWRModel$SDF$"`Threetune population`_se"
dataWindow$GWRWind <- sigTest_Wind
print(dataWindow$GWRWind)

sigTest_volume = abs(GWRModel$SDF$"`Passenger volume`") - 2 * GWRModel$SDF$"`Passenger volume`_se"
dataWindow$GWRvolume <- sigTest_volume

sigTest_Pre = abs(GWRModel$SDF$"`Urbanization rate`") - 2 * GWRModel$SDF$"`Urbanization rate`_se"
dataWindow$GWRPre <- sigTest_Pre

# Plot the coefficient
bubblefunc(subset(dataWindow,dataWindow$GWRTemp>0),subset(dataWindow$coeaggregate,dataWindow$GWRTemp>0),'GWREconomic',c(0,0.001,0.009))
bubblefunc(subset(dataWindow,dataWindow$GWRWind>0),subset(dataWindow$coepopulation,dataWindow$GWRWind>0),'GWRpopulation',c(-0.1,-0.05,0,0.01, 0.1))
bubblefunc(subset(dataWindow,dataWindow$GWRvolume>0),subset(dataWindow$coevolume,dataWindow$GWRvolume>0),'GWRvolume',c(-0.005,-0.001, 0,0.001, 0.005,0.01))
bubblefunc(subset(dataWindow,dataWindow$GWRPre>0),subset(dataWindow$coerate,dataWindow$GWRPre>0),'GWRUrbanization',c(-0.03,-0.01, 0,0.01, 0.03))

