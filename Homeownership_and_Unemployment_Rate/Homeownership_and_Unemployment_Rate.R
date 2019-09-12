setwd("")



# Load required packages from library (install them if needed)
library(dplyr) #For manipulation of tidy data

library(ggplot2) #For making good graphics

library(rgdal) #For GIS operations

library(rgeos) #For GIS operations

library(tmap) #For cartography

library(reshape2) # For reshaping data



############################### PART 1: European correlations ################################



# Read in the csv files containing European (i) ownership and (ii) unemp rates
Ownership <- read.csv("Ownership_Europe.csv")

Unemployment <- read.csv("Unemp_Europe.csv")



# Merge data frames together keying the merge on Year and Country
Europe_rates <- merge(Unemployment, 
                      Ownership, 
                      by = c("Year", "Country")) # Key merge on 2 variables

rm(Ownership, Unemployment) # Remove datasets we don't need



# Create year specific data frame for initial analysis of correlations
data_2010 <- Europe_rates %>% filter(Year==2010)



# Plot correlations using ggplot
plot10 <- ggplot(data_2010, # Data to use for plotting
               aes(data_2010$perc_own,  # Aesthetic variables
                   data_2010$Unemp_rate_u25))


plot10 + geom_point(colour = "Dark blue", size = 3) +
  geom_text(size = 4, colour = "Dark Red", 
            nudge_x = .5,
            aes(label = Country_short)) +
  labs(title = "Homeownership and unemployment of the under 25 across Europe, 2010",
       x = "% homeowners",
       y = "Unemployment rate of the under 25") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 30), xlim = c(50, 100)) + 
  geom_smooth(method = "lm", col = "red") # Fits a line to represent x-y relationship



# Spearman's correlation coefficient
cor.test(data_2010$Unemp_rate_u25, 
         data_2010$perc_own, 
         method = "spearman") # The red text is just a warning that there are tied values



# Repeat correlation tests for all years
tests <- Europe_rates %>%
        group_by(Year) %>%
        summarise(Cor=cor(Unemp_rate_u25,perc_own), N=n()) # Creates two columns of output

print(tests) # Print results to console



# ggplot scatter plots of x-y relationship by year
plot <- ggplot(Europe_rates, aes(Europe_rates$perc_own, 
                                 Europe_rates$Unemp_rate_u25)) +
  geom_point(colour = "Dark blue", size = 3) + 
  labs(title = "Homeownership and unemployment of the under 25 across Europe, 2010-2017",
       x = "Percent homeowners",
       y = "Unemployment rate of the under 25") + 
  coord_cartesian(ylim = c(0,30), xlim = c(50,100)) + 
  geom_smooth(method="lm", col = "Dark red")

plot + facet_wrap(~Year)



# Import Europe shapefile for mapping rates - note that it is quite large
Europe_shapes <- readOGR(".", "Europe_shapes")



# Convert rates data to wide format for mapping
Reshaped_rates <- reshape(Europe_rates, 
                          direction = "wide", # The direction the reshape is going
                          idvar = "Country", # The variable identifying each research unit
                          timevar = "Year", # The panel variable tracking time 
                          v.names = c("Unemp_rate_total", # Names of time-varying variables
                                      "Unemp_rate_u25",
                                      "Unemp_rate_2574",
                                      "perc_own",
                                      "perc_rent"))



# Merge rates data onto the shapefile for quick mapping
Europe_map <- merge(Europe_shapes, Reshaped_rates,
                    by.x = "NAME_0",
                    by.y = "Country",
                    all.x = T) # Some polygons have no Eurostat data but 
                               # we still want to keep them for neat mapping



# Example base maps for unemployment and homeownership in 2017
# Unemployment map 
tm_shape(Europe_map) + 
  tm_polygons("Unemp_rate_total.2017",
              style = "cont",
              title = "% unemployed (2017)",
              palette = "Reds") +
  tm_scale_bar(size = 0.3, position = c("LEFT", "BOTTOM"))


# Homeownership map
tm_shape(Europe_map) + 
  tm_polygons("perc_own.2017",
              style = "cont",
              title = "% homeowners (2017)",
              palette = "Purples") +
  tm_scale_bar(size = 0.3, position = c("LEFT", "BOTTOM"))



############################### PART 2: CHANGE OVER TIME ################################



# Compute change score variables
Reshaped_rates <- Reshaped_rates %>%
                  mutate(Unemp_rate_ch = Unemp_rate_total.2017 - Unemp_rate_total.2010,
                         Unemp_rate_u25_ch = Unemp_rate_u25.2017 - Unemp_rate_u25.2010,
                         perc_own_ch = perc_own.2017 - perc_own.2010)



# Correlation tests of change scores: Test multiple correlation stats
cor.test(Reshaped_rates$Unemp_rate_u25_ch, Reshaped_rates$perc_own_ch, method = "pearson")
cor.test(Reshaped_rates$Unemp_rate_u25_ch, Reshaped_rates$perc_own_ch, method = "spearman")
cor.test(Reshaped_rates$Unemp_rate_u25_ch, Reshaped_rates$perc_own_ch, method = "kendall")



# Produce a neat gg scatter comparing change in ownership to change in total unemp
ch_plot <- ggplot(Reshaped_rates, aes(Reshaped_rates$perc_own_ch, 
                           Reshaped_rates$Unemp_rate_u25_ch)) 


ch_plot + geom_point(colour = "Dark blue", size = 3) + 
  geom_hline(yintercept = 0, col = "black") +
  geom_vline(xintercept = 0, col = " black") +
  geom_text(size = 4, colour = "Dark Red", 
            nudge_x = .25,
            aes(label = Country_short)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Change in homeownership and unemployment rates of the under 25, 2010-2017",
       x = "Percentage point change in homeownership",
       y = "Percentage point change in unemployment of the under 25") +  
  coord_cartesian(ylim = c(-7, 5), xlim = c(-6, 8)) + 
  geom_smooth(method="lm", col = "Dark red")



# Linear regressions: Compare with O & B's Figure 2
lm(Unemp_rate_u25_ch ~ perc_own_ch, data = Reshaped_rates) %>%
  summary()

lm(Unemp_rate_u25_ch ~ perc_own_ch, data = Reshaped_rates) %>%
  summary()

rm(data_2010, tests, plot, plot10, ch_plot) # Not needed now
  


############################### PART 3: REGRESSIONS  ################################
  


# Read in datasets containing additional predictors
Avg_age <- read.csv("Age_Europe.csv")

Education <- read.csv("Education_Europe.csv")



# Merge onto the main dataset
Europe_rates <- merge(Europe_rates, Avg_age,
                      by = c("Year", "Country"))

Europe_rates <- merge(Europe_rates, Education,
                      by = c("Year", "Country"))



# Histogram of dependent variable (total unemployment rate)
hist(Europe_rates$Unemp_rate_u25, breaks = 20)

shapiro.test(Europe_rates$Unemp_rate_u25) # p<0.05 so non-normal



# Log transform dependent variable and check its distribution
Europe_rates <- Europe_rates %>% 
                 mutate(log_unemp = log(Unemp_rate_u25))


hist(Europe_rates$log_unemp, breaks = 20)

shapiro.test(Europe_rates$log_unemp) # P right on the 5% threshold now



# Use slide to compute (i) lagged homeownership and (ii) lagged unemp rates
library(DataCombine) # Contains the slide() function


Europe_rates <- Europe_rates %>% arrange(Country, Year) # For easier visualizing


Europe_rates <- slide(Europe_rates, Var ='perc_own', # Select variable to slide
                      TimeVar = 'Year', # Time variable
                      NewVar= 'perc_own_lag', # New variable's name
                      GroupVar = 'Country', # Group indicator
                      slideBy = -1) 


Europe_rates <- slide(Europe_rates, Var ='log_unemp',
                                    TimeVar = 'Year', 
                                    NewVar= 'log_unemp_lag', 
                                    GroupVar = 'Country', 
                                    slideBy = -1) # 
 


# Fit regressions predicting log unemp following O & B
# Start with only homeownership as a predictor
base <- lm(log_unemp ~ perc_own_lag, data = Europe_rates)
summary(base)



# Add socio-demographics
sociodem <- lm(log_unemp ~ perc_own_lag + Median_age + perc_degree, data = Europe_rates)
summary(sociodem)



# Add period (as a linear term for parsimony here)
years <- lm(log_unemp ~ perc_own_lag + Median_age + perc_degree + Year,
            data = Europe_rates)
summary(years)



# Plus country dummies - Austria by default is the ref category
countries <- lm(log_unemp ~ perc_own_lag + Median_age + perc_degree + Year +
                  factor(Country), data = Europe_rates)
summary(countries)



# Explore residual diagnostics for last model
plot(countries) # Don't look too bad really!



# Clean models for export - have a look also at the stargazer package too
library(memisc)

mtable(base, sociodem, years, countries,
       summary.stats = T,
       digits = 3)

