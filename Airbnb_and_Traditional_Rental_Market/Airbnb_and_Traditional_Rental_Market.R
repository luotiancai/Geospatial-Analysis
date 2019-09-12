
setwd("")



# Load required packages from library (install them if needed)
library(dplyr) #For manipulation of tidy data

library(ggplot2) #For making good graphics

library(rgdal) #For GIS operations

library(rgeos) #For GIS operations

library(tmap) #For cartography



############################### PART 1: Borough level patterns ################################



# Read in raw Airbnb listings dataset
Lets18 <- read.csv("London_Airbnb_listings_Oct18.csv")



# Compute Airbnb density per borough
Lets18 <- Lets18 %>% group_by(borough) # Tell R to group the listings by borough


counts18 <- summarise(Lets18, density18 = n()) %>% # Create a new file holding Airbnb density
            arrange(density18)                     # Arrange the new data in ascending order



# Map crude density of Airbnbs across London boroughs - compare results with IPPR report (2016)
boroughs <- readOGR(".", "London_boroughs") 
# Read in the borough shapefile to a new object called 'boroughs'. The "." tells R to load from
# working directory location. "London_boroughs" is the name of the shapefile to load in.


borough_map <- merge(boroughs, counts18, # Merge counts18 data onto shapefile polygons
                    by.x = "NAME", # Tells R that the variable 'NAME' identifies borough polygons
                    by.y = "borough") # The variable 'borough' identifies boroughs in the table


qtm(borough_map, fill = "density18", # Create a 'quick map' of the density18 variable
                 fill.style = "quantile") # Categorise density into quantiles




# Merge in data on the size of the dwelling stock in each borough (2017 estimates)
borough_dwelling_stock <- read.csv("Borough_dwelling_stock.csv") # Load data to R


counts18 <- merge(counts18, borough_dwelling_stock, # Merge stock numbers onto count data
                  by = "borough") # This is the variable to use for the merging


counts18 <- counts18 %>% # Remember %>% is a 'pipe' for linking together commands
            mutate(perc_Airbnbs = (density18/n_dwellings)*100) %>% # Compute %
            arrange(perc_Airbnbs) # Arrange file in ascending order



# Graph density data
plot <- ggplot(counts18, # Tell R which dataset to use for the ggplot object - called plot
               aes(counts18$density18,  
                   counts18$perc_Airbnbs)) # Set variables to use as aesthetics


plot + geom_point(colour = "Blue", size = 3) +
        geom_text(size = 4, colour = "Dark Red", 
                  nudge_x = 200,
                  aes(label = borough)) +
       labs(title = "Distribution of Airbnb listings across London, 2018",
            x = "Number of listings",
            y = "Listings as % of borough's dwelling stock") +
       theme(plot.title = element_text(hjust = 0.5)) +
       coord_cartesian(ylim = c(0, 7), xlim = c(0, 7000))



# Explore if density of Airbnbs relates to the geography of private rents
borough_rents <- read.csv("London_borough_rents_18.csv") # Dataset holds mean and medians


counts18 <- merge(counts18, borough_rents, # Merge rent data onto Airbnb density dataset
                  by = "borough")



# Use plots and correlations to explore patterns in count and density of Airbnb with rents
plot(counts18$density18, counts18$median_18) # Median is probably a more suitable 'average' to use

cor.test(counts18$density18, counts18$median_18, method = "spearman") # Test correlation


plot(counts18$perc_Airbnbs, counts18$median_18)

cor.test(counts18$perc_Airbnbs, counts18$median_18, method = "spearman")



# Fit a quick regression of Airbnb density against rents
fit1 <- lm(median_18 ~ density18, data = counts18) # Linear model
counts18 <- counts18[-7, ]
summary(fit1) 

plot(fit1) # Explore diagnostics - is the model fitting well? What happens if you remove the City?



# Tidy up workspace
rm(fit1, plot)




############################### PART 2: Local pattern analysis ################################



# Convert Airbnb data into spatial points - working here with WGS84 coordinates. This means
# using the EPSG4326 code when telling R how to create the spatial point object.
Airbnb_coord18 <-SpatialPointsDataFrame(Lets18 [7:6], # Tell R which columns hold x,y coordinates
                                        Lets18,
                                        proj4string = CRS("+init=EPSG:4326")) # Set projection



# Reproject spatial object into BNG 27700 system for UK datasets
Airbnb_coord18 <- spTransform(Airbnb_coord18, CRS("+init=EPSG:27700"))


plot(Airbnb_coord18) # Plot creates a basic plot of the new object
                                                            


# Load in shapefile of London postcode sectors
postcodes <- readOGR(".", "London_postcodes") # Read in the postcodes shapefile from working dir


qtm(postcodes) + qtm(Airbnb_coord18, dots.col = "darkred") 
# qtm stands for quick tmap. overlay different shapefiles as multiple layers using '+'



# Perform point in polygon count operation to count the n Airbnbs per postcode district
library(GISTools) # This package contains the poly.count function

postcodes$counts <- poly.counts(Airbnb_coord18, postcodes) # First in the brackets is the
# point data object, followed by the polygons.



# Explore the distribution of Airbnbs over postcode districts
summary(postcodes$counts) # Summary stats of the distribution


boxplot(postcodes$counts) # Descriptive box plot


qtm(postcodes, fill = "counts",
               fill.style = "quantile",
               title = "N Airbnbs in London postcode districts, 2018")



# Adjust counts for size of the dwelling stock within postcode areas
postcode_dwelling_stock <- read.csv("Postcode_address_densities_11.csv")


postcodes <- merge(postcodes, postcode_dwelling_stock,
                   by.x = "PostDist", # The name of the postcode district variable on the shapefile
                   by.y = "postcode_district") # The same variable on the census data


postcodes$count_density <- (postcodes$counts/postcodes$n_addresses)*100 # Compute density per 100 dwellings


summary(postcodes$count_density) # Explore the distribution of values

boxplot(postcodes$count_density) # Boxplot of distribution


qtm(postcodes, fill = "count_density", # Make a basic map of the densities (expressed/100 dwellings)
               fill.style = "fixed", # Fixed tells R to use specific category break points
               fill.breaks = c(0,0.2,0.5,1,2,3,5,40)) # These cut points work fairly well



# Examine how the density of Airbnbs relates to the geography of local rents
postcode_rents <- read.csv("GLA_London_rents_18.csv")


postcodes <- merge(postcodes, postcode_rents,
                   by.x = "PostDist",
                   by.y = "postcode_district")



# Simple bivariate plot followed by correlation rank test
plot(postcodes$count_density, postcodes$median)

cor.test(postcodes$count_density, postcodes$median, method = "spearman")


# Fit a basic regression model
fit1 <- lm(median ~ count_density, data = postcodes)

summary(fit1)

plot(fit1) # Model diagnostic checks

