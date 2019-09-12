library(sp)
library(rgdal)
library(rgeos) 
library(RColorBrewer)
library(classInt)
library(tmap)
library(GISTools)
library(spdep)

tmap_mode("view")

#read table
UK_pp_2018 <- read.csv("pp-2018.csv", header = F)
UK_loc <- read.csv("all.csv", header = F)

#import boundary
LondonWard <- readOGR("London-wards-2018_ESRI/London_Ward_CityMerged.shp")

#rename the filed
names(UK_pp_2018) <- c("TransactionID","Price","Date","Postcode","Type","Old-New","Duration","PAON","SAON","Street","Locality","Town-City","District","County","PPD","RecordStatus")
names(UK_loc) <- c("ID","NAMES_URI","NAME1","NAME1_LANG	NAME2","NAME2_LANG","TYPE	LOCAL_TYPE","GEOMETRY_X","GEOMETRY_Y","MOST_DETAIL_VIEW_RES","LEAST_DETAIL_VIEW_RES","MBR_XMIN","MBR_YMIN","MBR_XMAX","MBR_YMAX","POSTCODE_DISTRICT","POSTCODE_DISTRICT_URI","POPULATED_PLACE","POPULATED_PLACE_URI","POPULATED_PLACE_TYPE","DISTRICT_BOROUGH","DISTRICT_BOROUGH_URI","DISTRICT_BOROUGH_TYPE","COUNTY_UNITARY","COUNTY_UNITARY_URI","COUNTY_UNITARY_TYPE","REGION","REGION_URI","COUNTRY","COUNTRY_URI","RELATED_SPATIAL_OBJECT","SAME_AS_DBPEDIA","SAME_AS_GEONAMES")
#select the london and exclude the extreme value
London_pp_2018 <- UK_pp_2018[which(UK_pp_2018$County == "GREATER LONDON"),]
#London_pp_2018 <- London_pp_2018[which(London_pp_2018$Type != "O"),]
London_loc <- UK_loc[which(UK_loc$COUNTY_UNITARY_TYPE == "Greater London"),]
#join tables
London_pp_2018_Loc <- merge(London_pp_2018, London_loc, by.x = "Postcode", by.y = "NAME1")

#make a spatial data frame
bng <- "+init=epsg:27700" #BNG, British National Grid
coords <- cbind(Longitude = London_pp_2018_Loc$MOST_DETAIL_VIEW_RES, Latitude = London_pp_2018_Loc$LEAST_DETAIL_VIEW_RES)
pp.pts <- SpatialPointsDataFrame(coords, London_pp_2018_Loc[, -(24:25)],proj4string = CRS(bng))
LondonWard <- spTransform(LondonWard, CRS(bng))
plot(pp.pts,pch='.',col='darkred')
plot(LondonWard, add = T)

#count the point number in each ward
#res <- poly.counts(pp.pts, LondonWard)
#LondonWard@data$deal.Count<-res

#group by the code
pp_over<- over(pp.pts, LondonWard)
London_pp_2018_Loc <- data.frame(London_pp_2018_Loc,  CODE = pp_over$GSS_CODE)
median_price <- aggregate(London_pp_2018_Loc$Price,list(London_pp_2018_Loc$CODE), median)
LondonWard@data <- merge(LondonWard@data, median_price, by.x = "WD11CD", by.y = "Group.1")
#LondonWard@data$Average_price <- LondonWard@data$x / LondonWard@data$deal.Count

#plot
tm_shape(LondonWard) +
  tm_polygons("x",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              title="House Price distribution in London")

# spatial autocorrelation
coordsW <- coordinates(LondonWard)
LWard_nb <- poly2nb(LondonWard, queen=T)
Lward.lw <- nb2listw(LWard_nb, style="C")

#moran's I test - this tells us whether we have clustered values (close to 1) or dispersed values (close to -1)
I_LWard_Global_Price <- moran.test(LondonWard@data$x, Lward.lw)
I_LWard_Global_Price

#use the localmoran function to generate I for each ward in the city
I_LWard_Local_Price <- localmoran(LondonWard@data$x, Lward.lw)
LondonWard@data$BLocIRz <- I_LWard_Local_Price[,4]

#set the breaks manually based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))
tm_shape(LondonWard) +
  tm_polygons("BLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, House Price Distribution in London")

#the Getis Ord G???i statisic for hot and cold spots
Gi_LWard_Local_Price <- localG(LondonWard@data$x, Lward.lw)
LondonWard@data$BLocGiRz <- Gi_LWard_Local_Price
GIColours<- rev(brewer.pal(8, "RdBu"))
tm_shape(LondonWard) +
  tm_polygons("BLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, House Price Distribution in London")

# correlation with 

LondonData <- read.csv("ward-profiles-excel-version.csv")
names(Cor_make)
#Cor_make <- merge(LondonData,LondonWard@data, by.x = "New.code", by.y = "GSS_CODE")
Cor_make <- merge(LondonWard@data, LondonData, by.x = "WD11CD", by.y = "New.code")
Cor_make[,71] <- as.numeric(as.character(Cor_make[,71]))
Cor_make[is.na(Cor_make[,71]),71] <- median(Cor_make[,71],na.rm = T)
cor(Cor_make[,76], Cor_make$x)

#writeOGR(LondonWard, dsn = ".", layer = "lty", driver = "ESRI Shapefile")

66 53 42