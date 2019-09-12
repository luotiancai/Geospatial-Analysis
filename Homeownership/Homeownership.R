
setwd("")



# Load required packages from library (install them if needed)
library(dplyr)

library(ggplot2)

library(descr)




############################### PART 1: Data inspection (wave one) ################################



# Read in the wave one survey dataset
wave1 <- read.csv("ukhls_w1.csv")



# Inspect survey dataset variables one by one
table(wave1$sex) # Categorical variable

table(wave1$age) # Integer age variable

table(wave1$partner) # Categorical variable

table(wave1$degree) # Categorical variable. Note 92 missings.

table(wave1$inc4) # Categorical income quartile variable. 

table(wave1$parmprof) # Categorical parental class variable. Our key predictor today.

table(wave1$owner) # Categorical housing status measure. Our dependent variable. Note 156 missings.

table(wave1$gor_dv) # Categorical region variable (GOR = Goverment Office Region)



# Compute age band variable like in last practical
wave2$age_band <- cut(wave2$age, # Cutting age variable into groups to generate age_band
                      breaks = c(15, 24, 34, 44, 54, 64, 74, 110), # Break points between groups
                        labels = c("16-24", 
                                   "25-34", 
                                   "35-44", 
                                   "45-54", 
                                   "55-64", 
                                   "65-74", 
                                   "75+")) # New labels to assign to each age band group



# Cross-tabulate age band against ownership status, switching on the weights
crosstab(wave1$age_band, wave1$owner, 
         weight = wave1$indinus_xw, # This switches on the pweight
         user.missing.indep = "Missing", # Tell R that the 'missing' category is missing data
         prop.r = TRUE, # Switch on row proportions (lower row of numbers in each cell)
         total.r = TRUE, # Switch on row totals
         chisq = TRUE) # Switch on a chi-sq test of independence



# Repeat for degree variable
crosstab(wave1$degree, wave1$owner, 
         weight = wave1$indinus_xw, 
         user.missing.indep = "Missing", 
         user.missing.dep = "Missing", # Tell R that 'missing' degree is missing data
         prop.r = TRUE, 
         total.r = TRUE, 
         chisq = TRUE) 



# Repeat for income variable
crosstab(wave1$inc4, wave1$owner, 
         weight = wave1$indinus_xw, 
         user.missing.indep = "Missing",
         prop.r = TRUE, 
         total.r = TRUE, 
         chisq = TRUE) 



# Repeat for parental class variable
crosstab(wave2$parmprof, wave2$owner, 
         weight = wave2$indinus_xw, 
         user.missing.indep = "Missing",
         prop.r = TRUE, 
         total.r = TRUE, 
         chisq = TRUE) 



############################# PART 2: Describing homeownership transitions ################################



# Read in datafiles from waves 2-5 of the survey one by one
wave2 <- read.csv("ukhls_w2.csv")
wave3 <- read.csv("ukhls_w3.csv")
wave4 <- read.csv("ukhls_w4.csv")
wave5 <- read.csv("ukhls_w5.csv")



# Append files together
long_file <- bind_rows(wave1, wave2, wave3, wave4, wave5) # Pool all rows together in one file

long_file$wave <- as.factor(long_file$wave) # Convert character wave variable to factor for use



# Arrange into person-year file
long_file <- long_file %>% group_by(pidp) %>% 
             arrange(pidp, wave) # Arrange into long format



# Select out cases of interest (young adults in W1 we are tracking)
long_file$tag <- ifelse(long_file$wave=="Wave 1" 
                        & long_file$age<35 & 
                          long_file$owner=="Not owner", 1, 0)

table <- table(long_file$wave, long_file$tag) # Check people are only tagged in wave 1

print(table)



# Carry forward 'tag' to select out only obs from later waves contributed by selected people
long_file <- mutate(long_file, selector = max(tag))

table <- table(long_file$wave, long_file$selector) # Create a table showing the wave breakdown of cases

print(table) # This prints the tabulated wave distribution of tagged cases



# Subset to only retain tracked young adults
sample <- filter(long_file, selector==1)



# Compute ownership rate over survey time
crosstab(sample$wave, sample$owner, 
         prop.r = T, # Switch on row %
         user.missing.indep = "Missing") # Tell R that there are missings in tenure



# Generate variable recording parental background for all sweeps of the dataset
sample$par_back <- ifelse(sample$parmprof == "Managerial/professional" & 
                            sample$wave == "Wave 1", 1, 0)  # Create a dummy identifying wave 1 kids with managerial parents

sample <- mutate(sample, par_back = max(par_back)) # Generalise values across each person's records

sample$par_back <- factor(sample$par_back, # Recode the dummy variable into labelled categories
                          levels = c(0, 1),
                          labels = c("Not managerial/professional", "Managerial/professional"))
                          
                          
                          
# Run chi-square test to assess whether wave 2 homeownership is independent of parental background
wave_five <- filter(sample, wave == "Wave 5")  # Select only wave two records

crosstab(wave_five$par_back, wave_five$owner, 
                            user.missing.indep = "Missing", 
                            prop.r = TRUE,
                            chisq = TRUE)


# Construct 95% CIs for kids from non-managerial professional families
binom.test(295, 3281, conf.level = .95) 

# Repeat for kids from managerial/professional families
binom.test(280, 1634, conf.level = .95)



# Calculate 95% CIs for the difference in proportions (0.0355102 - 0.01931905)
install.packages("PropCIs")

library(PropCIs)

0.1713586 - 0.08991161 # Estimated difference in proportions = 1.62 percentage points

diffscoreci(280, 1634, 295, 3281, conf.level = .95) # 95% CI for the difference = 0.84-2.50



############################# PART 3: Modelling homeownership ################################



# Create new dataset with only wave one and five cases
model_sample <- filter(sample, wave == "Wave 1" |
                               wave == "Wave 5")



# Code homeownership status in wave five onto wave one observations
install.packages("DataCombine") 

library(DataCombine) 

model_sample <- slide(model_sample, Var ='owner', # Slide assigns wave 5 value onto wave 1 case
                NewVar= 'owner5', # This specifies the new variable that has been slid
                GroupVar = 'pidp', # Grouping variable to calculate lead values across
                slideBy = 1) # How long a slide to use - here just one observation


# Recode the slid variable into a dummy, removing NAs (coded 1)
model_sample_nm$owner5 <- recode(model_sample_nm$owner5, 
                              '1' = NA_real_, # Values coded 1 become NAs
                              '2' = 0, # Values coded 2 become 0 (not owners)
                              '3' = 1) # Values coded 3 become 1 (owners) 
model_sample_m <- model_sample[which(model_sample[ ,15] == "Managerial/professional"), ]
                


# Logistic regression model
model <- glm(formula = owner5 ~ age, # Fit a generalised linear model
             data = model_sample_nm, # Which dataset to use
             binomial(link = "logit")) # Tells R to use logit link 

summary(model) # Produce model output 

exp(coef(model)) # Show results in odds ratios (more intuitive to understand)


