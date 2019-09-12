
setwd("")



# Load required packages from library (install them if needed)
library(dplyr) #For manipulation of tidy data

library(ggplot2) #For making good graphics

library(rgdal) #For GIS operations

library(rgeos) #For GIS operations

library(tmap) #For cartography

library(reshape2) #For manipulating datasets into different formats



############################### PART 1: Segregation indices ################################



Ethnic_OAs <- read.csv("Ethnic_OAs_11.csv")



# Explore distribution of ethnic groups across OAs within the 5 LADs.
plot_data <- melt(Ethnic_OAs, id.vars='Local_Authority', 
                  measure.vars=c('p_White_British',
                                 'p_Other_White',
                                 'p_Mixed',
                                 'p_Indian',
                                 'p_Pakistani',
                                 'p_Bangladeshi',
                                 'p_Chinese',
                                 'p_African',
                                 'p_Caribbean',
                                 'p_Other'))


# Create boxplot showing variable distributions across OAs in each LAD 
ggplot(plot_data) +
  geom_boxplot(aes(x=Local_Authority, y=value, color=variable)) +
  ggtitle("Ethnic composition (%) of OAs across Integration Areas")


rm(plot_data) # Delete plotting dataset



# Set up the data to allow us to compute the Index of Dissimilarity for different ethnic groups.
# Step 1: Compute total n White British and Minority across all OAs in each LAD
Ethnic_OAs <- Ethnic_OAs %>% 
              group_by(Local_Authority) %>%  # Tell R to group the data frame by LAD
              mutate(LAD_White_British = sum(White_British), # Produce total White British
                     LAD_Minorities = sum(Other_White, # Produce total minority 
                                          Mixed,
                                          Indian,
                                          Pakistani,
                                          Bangladeshi,
                                          Chinese,
                                          African,
                                          Caribbean,
                                          Other))
                     


# Step 2: Compute each OA's contribution to the index value
Ethnic_OAs <- Ethnic_OAs %>% 
              mutate(OA_diss = abs(White_British/LAD_White_British - # Abs computes absolute values
                                  (Other_White + Mixed + Indian +
                                        Pakistani + Bangladeshi +
                                        Chinese + African + Caribbean +
                                        Other)/LAD_Minorities))


# Step 3: Sum OA contributions to get LAD totals measure - the index score
OA_dissim_scores <- Ethnic_OAs %>%
                    summarise(Dissimilarity = .5*sum(OA_diss)) # Remember data are already grouped into LADs



# Read in ward level data
Ethnic_wards <- read.csv("Ethnic_wards_11.csv")



# Step 1: Compute total n White British and Pakistani across all wards in each LAD
Ethnic_wards <- Ethnic_wards %>% 
  group_by(Local_Authority) %>% 
  mutate(LAD_White_British = sum(White_British), 
         LAD_Pakistani = sum(Pakistani))
                              
         

# Step 2: Compute each ward's contribution to the index
Ethnic_wards <- Ethnic_wards %>% 
  mutate(Ward_diss = abs(White_British/LAD_White_British -
                         Pakistani/LAD_Pakistani))

        

# Step 3: Compute ward index scores
Ward_dissim_scores <- Ethnic_wards %>%
  summarise(Dissimilarity = .5*sum(Ward_diss)) 


# NOTE: There is a seg package that will perform these calculations for you :-)



# Calculate Isolation Index for each minority group at OA level
# Step 1: Compute LAD counts for each minority - note we already have White British
Ethnic_OAs <- Ethnic_OAs %>%  # Data are already grouped into LADs
  mutate(LAD_Other_White = sum(Other_White),
         LAD_Mixed = sum(Mixed),
         LAD_Indian = sum(Indian),
         LAD_Pakistani = sum(Pakistani),
         LAD_Bangladeshi = sum(Bangladeshi),
         LAD_Chinese = sum(Chinese),
         LAD_African = sum(African),
         LAD_Caribbean = sum(Caribbean),
         LAD_Other = sum(Other))



# Step 2: Calculate isolation of each group in each OA (Eth_tot = total OA population)
Ethnic_OAs <- Ethnic_OAs %>%
  mutate(White_British_isol = (White_British/LAD_White_British * White_British/Eth_tot),
         Other_White_isol = (Other_White/LAD_Other_White * Other_White/Eth_tot),
         Mixed_isol = (Mixed/LAD_Mixed * Mixed/Eth_tot),
         Indian_isol = (Indian/LAD_Indian * Indian/Eth_tot),
         Pakistani_isol = (Pakistani/LAD_Pakistani * Pakistani/Eth_tot),
         Bangladeshi_isol = (Bangladeshi/LAD_Bangladeshi * Bangladeshi/Eth_tot),
         Chinese_isol = (Chinese/LAD_Chinese * Chinese/Eth_tot),
         African_isol = (African/LAD_African * African/Eth_tot),
         Caribbean_isol = (Caribbean/LAD_Caribbean * Caribbean/Eth_tot),
         Other_isol = (Other/LAD_Other * Other/Eth_tot))


# Step 3: Summarise OA values across LADs
OA_isol_scores <- Ethnic_OAs %>%
  summarise(White_British_I = sum(White_British_isol),
            Other_White_I = sum(Other_White_isol),
            Mixed_I = sum(Mixed_isol),
            Indian_I = sum(Indian_isol),
            Pakistani_I = sum(Pakistani_isol),
            Bangladeshi_I = sum(Bangladeshi_isol),
            Chinese_I = sum(Chinese_isol),
            African_I = sum(African_isol),
            Caribbean_I = sum(Caribbean_isol),
            Other_I = sum(Other_isol))

#social class
NSSEC_OAs <- read.csv("NSSEC_OAs_11.csv")
names(NSSEC_OAs)
NSSEC_OAs$Prof <- NSSEC_OAs$NSSEC_1HigherManProf +NSSEC_OAs$NSSEC_2LowerManProf
NSSEC_OAs$Routine <- NSSEC_OAs$NSSEC_5LowerSupTech + NSSEC_OAs$NSSEC_6SemiRoutine + NSSEC_OAs$NSSEC_7Routine

NSSEC_OAs <- NSSEC_OAs %>% 
  group_by(Local_Authority) %>%  # Tell R to group the data frame by LAD
  mutate(LAD_High = sum(Prof), # Produce total White British
         LAD_Low = sum(Routine))

NSSEC_OAs <- NSSEC_OAs %>% 
  mutate(OA_diss = abs(Prof/LAD_High - # Abs computes absolute values
                         Routine/LAD_Low))

OA_dissim_scores_class <- NSSEC_OAs %>%
  summarise(Dissimilarity = .5*sum(OA_diss))



############################### PART 2: Local patterns ################################



# Load in Bradford OA shapefile for case study analysis of this city
Bradford_shapes <- readOGR(".", "Bradford_OAs")



# Merge census data onto Bradford shapefile using OA codes as identifiers
Bradford_map <- merge(Bradford_shapes, Ethnic_OAs,
                      by.x = "oa11cd",
                      by.y = "OA_code")
Bradford_map <- merge(Bradford_shapes, NSSEC_OAs,
                      by.x = "oa11cd",
                      by.y = "OA_code")


# Quick choropleths of (i) % White British and (ii) % Pakistani
qtm(Bradford_map, 
    fill = "p_White_British",
    fill.style = "quantile")

qtm(Bradford_map, 
    fill = "p_Pakistani",
    fill.style = "quantile")

plot(Bradford_map$p_White_British, Bradford_map$p_Pakistani) # Explore bivariate patterns



# Load spdep package for spatial analysis
library(spdep) # Install package first if necessary



# Define 'neighbouring' OAs as those that share borders with each other
neighbour_OAs <- poly2nb(Bradford_map, queen = TRUE)


plot(Bradford_map, border = "lightgrey")
plot(neighbour_OAs, coordinates(Bradford_map), add = T, col ="red")


NSSEC_OAs$p_Prof <- NSSEC_OAs$Prof/NSSEC_OAs$NSSEC_tot
NSSEC_OAs$p_Routine <- NSSEC_OAs$Routine /NSSEC_OAs$NSSEC_tot
# Compute G* statistic for White Britons and Pakistanis
neighbour_OAs <- nb2listw(neighbour_OAs) # Convert neighbours object to a listw

g_Prof <- localG(Bradford_map$p_Prof, neighbour_OAs) # British

g_Routine <- localG(Bradford_map$p_Routine , neighbour_OAs) # Pakistani
  
  

# Bind G* statistics onto shapefile to generate mappable G* statistics
g_Prof <- cbind(Bradford_map, as.matrix(g_Prof)) 

g_Routine <- cbind(Bradford_map, as.matrix(g_Routine))



# Rename messy variables to have sensible names
names(g_Prof)[24] <- "gstat_High-income_group" # [50] tells R to rename variable number 50

names(g_Routine)[24] <- "gstat_Low-income_group"


# Map clusters of White Britons using G* statistic
qtm(Bradford_map, fill = "p_White_British", fill.style = "quantile")
qtm(Bradford_map, fill = "p_Prof", fill.style = "quantile")

tm_shape(g_Prof) +  # Set the object to map
  tm_fill("gstat_High-income_group", # Set variable to map
          palette = "RdBu", # Select colour palette
          style = "fixed", # Set fixed category break points to 
          breaks = c(-10, -2.58, -1.96, 1.96, 2.58, 10)) + 
  tm_borders(alpha=.4) # Set OA border transparency



# Repeat for Pakistani
qtm(Bradford_map, fill = "p_Pakistani", fill.style = "quantile")


tm_shape(g_Routine) +  
  tm_fill("gstat_Low-income_group", 
          palette = "RdBu", 
          style = "fixed", 
          breaks = c(-10, -2.58, -1.96, 1.96, 2.58, 10)) + 
  tm_borders(alpha=.4) # Set OA border transparency
