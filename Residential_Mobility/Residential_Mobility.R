
setwd("")



############################### PART 1: Exploring the data ################################


# Read in 2005-6 APS
APS1718 <- read.csv("aps1718_eul.csv") # 364866 obs in dataset


# Load required packages from library (you might need to install packages first)
library(tidyverse) # Contains dplyr and ggplot2

library(descr) # Useful for categorical variables and weighting


# Examine variable measuring duration at address to see coding scheme
table(APS1718$restme)


# Create dummy separating those moving in last 12 months from those not moving in last 12 months

APS1718 <- APS1718 %>% mutate(moved = fct_collapse(restme, # fct_collapse tells R to collapse a factor
                                                   "No move" = # Next come categories collapsed into 'not moved'
                                                     c("10 years or longer", 
                                                       "5 years, less than 10",
                                                       "3 years, less than 5", 
                                                       "2 years, less than 3",
                                                       "12 months, less than 24"),
                                                   "Moved" = # Next come categories collapsed into 'moved'
                                                            "Less than 12 months",
                                                   "N/A" = # Next come missings collapsed into 'N/A'
                                                     c("No answer", "Does not apply")))


# Check the recode looks ok
table(APS1718$restme, APS1718$moved) # Recode seems to have worked


# Discard the 98 missing values

APS1718_sub <- APS1718 %>% filter(moved!="N/A") # Leaves 364768 cases - note the pipe (%>%)

table(APS1718_sub$restme) # Fine - invalid values removed


# Calculate weighted mobility rate using frequency weights

freq(APS1718_sub$moved, w = APS1718_sub$weight, plot=F) 
# Mobility rate is 10.03%. 2011 UK census estimated ~11% so this seems plausible

# Compare with unweighted rate
table(APS1718_sub$moved)

32768/(332000+32768)*100 # 8.9% if unweighted. So APS undercounts mobile people.



############################### PART 2: Age migration profile ################################


# Create banded age variable and use to derive age-mobility profile
#cut(), Convert Numeric To Factor
APS1718_sub$age_band <- cut(APS1718_sub$age, 
                            breaks = c(-1, 4, 9, 14, 19, 24, 29, 34, 39, 44, 
                                       49, 54, 59, 64, 69, 74, 79, 84, 100),
                        labels = c("0-4", 
                                   "5-9", 
                                   "10-14", 
                                   "15-19", 
                                   "20-24", 
                                   "25-29", 
                                   "30-34", 
                                   "35-49", 
                                   "40-44",
                                   "45-49",
                                   "50-54", 
                                   "55-59", 
                                   "60-64", 
                                   "65-69", 
                                   "70-74", 
                                   "75-79", 
                                   "80-84", 
                                   "85+"))

# Check coding
check_tab <- table(APS1718_sub$age, APS1718_sub$age_band)

print(check_tab) # Patterns look ok 
                                     
rm(check_tab) # Delete dataframe we no longer need         


# Compute xtab showing weighted mobility rates by age band
#crosstab(first factor, second factor, weighting)
crosstab(APS1718_sub$age_band, APS1718_sub$moved, 
         w = APS1718_sub$weight, # Applies a weight to the cross-tab
         prop.r = T) # Tells R to show row proportions


# Select out kids so we only calculate rates for 15 and older
APS1718_adults <- APS1718_sub %>% filter(age >=15) # The dataset without kids is APS1718_adults


# Re-compute xtabs and export for graphing
tabplot <- crosstab(APS1718_adults$age_band, APS1718_adults$moved, 
                    w = APS1718_adults$weight,
                    prop.r = T)

tabplot <- tabplot$prop.row*100 # Converts proportions to %

tabplot_agemob <- as.data.frame(tabplot) # Converts results to a data frame

tabplot_agemob <- tabplot_agemob %>% 
  filter(APS1718_adults.moved == "Moved") # Retain only mover cells


# Graph rates using columns
p <- ggplot(tabplot_agemob, 
            aes(tabplot_agemob$APS1718_adults.age_band, 
                tabplot_agemob$Freq)) 

p + geom_col(fill= "navy") + 
  labs(title = "Age-mobility profile for 2005-6", 
       x = "Age band", 
       y = "Percent moved in last year") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_cartesian(ylim = c(0, 30))  

# Tabulate mobility rate by tenure
crosstab(APS1718_adults$tenure, APS1718_adults$moved, 
         w = APS1718_adults$weight, # Set which weight variable to use
         prop.r = T, # Switch on row proportions
         plot = T) # Switch off plot of patterns


