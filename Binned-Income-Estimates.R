#4/30/24, initiated by BS
#Goal: Preparing all income data
#To prep and tidy income data, mostly working on binned data calculations


#Libraries
require(tidyverse)
require(tidycensus)
require(sf)
library(scales)

library(binsmooth) #For binned calculations
library(pracma) #For mathematical functions

options(tigris_use_cache = TRUE)

#Create a filepath to OneDrive
onedrivepath="~/OneDrive - The Pennsylvania State University/"

#Set parameters (state and year)
ST = "VA"
GEOG = "tract"
CITY = "Richmond MSA"

YR1 = 1990
YR2 = 2000
YR3 = 2010
YR4 = 2020

#----------------------------------------------------------------------------------------------------------------
#NOTE: Crosswalks on income counts must be performed first

#Load variable selection
Data <- load_variables(YR4, "acs5")

#Using 2020 median household income data as the reference variable
#2020 Median HH income by urban/suburban
Med_hh_inc_2020 <- get_acs(
  geography = GEOG, 
  variables = "B19013_001", 
  state = ST,
  year = YR4,
  output = "wide") 

#------------------------------------------------------------
#Working on the binned data

#Practice bin counts
# binedges <- c(10000,15000,20000,25000,30000,35000,40000,45000,
#               50000,60000,75000,100000,125000,150000,NA)
# bincounts <- c(157532,97369,102673,100888,90835,94191,87688,90481,
#                79816,153581,195430,240948,155139,94527,103217)
# 


##2020
#Load binned data and tidy
Household_Inc_2020 <- get_acs(
  geography = GEOG, 
  table = "B19001", 
  state = ST,
  year = YR4,
  output = "wide") %>%
  rename(Below10000 = "B19001_002E",
         Btw10000_14999 = "B19001_003E",
         Btw15000_19999 = "B19001_004E",
         Btw20000_24999 = "B19001_005E",
         Btw25000_29999 = "B19001_006E",
         Btw30000_34999 = "B19001_007E",
         Btw35000_39999 = "B19001_008E",
         Btw40000_44999 = "B19001_009E",
         Btw45000_49999 = "B19001_010E",
         Btw50000_59999 = "B19001_011E",
         Btw60000_74999 = "B19001_012E",
         Btw75000_99999 = "B19001_013E",
         Btw100000_124999 = "B19001_014E",
         Btw125000_149999 = "B19001_015E",
         Btw150000_199999 = "B19001_016E",
         Above200000 = "B19001_017E") %>%
  select(GEOID, Below10000, Btw10000_14999, Btw15000_19999, Btw20000_24999, Btw25000_29999, Btw30000_34999,
         Btw35000_39999, Btw40000_44999, Btw45000_49999, Btw50000_59999, Btw60000_74999, Btw75000_99999,
         Btw100000_124999, Btw125000_149999, Btw150000_199999, Above200000) %>%
  mutate(Year = YR4,
         GEOID = as.character(GEOID)) %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11))

#Load Richmond Geographies
#2020
Geography <- read_sf(paste0(onedrivepath, "Mapping Richmond/Index/Index_2020/Richmond_Index_2020.shp")) %>%
  rename(Suburban_Index = Sbrbn_I,
         Urban_Index = Urbn_In,
         Landscape_Four = Lndscp_Fr,
         Landscape_Five = Lndscp_Fv,
         Landscape_Six = Lndsc_S,
         Landscape_All = Lndsc_A) %>%
  select(GEOID, Landscape_Five)

#Join hh data to Richmond state, tidy, and pivot
#Create bin boundaries for the function below
#Remove NAs/0 as function fails on NAs and 0 values
Geography <- Geography %>%
  left_join(Household_Inc_2020, by = "GEOID") %>%
  pivot_longer(cols = matches("^B|^A"),
               names_to = "Inc_Threshold",
               values_to = "Count") %>%
  mutate(
    lower_limit = case_when(
      Inc_Threshold == "Below10000" ~ 0,
      Inc_Threshold == "Btw10000_14999" ~ 10000,
      Inc_Threshold == "Btw15000_19999" ~ 15000,
      Inc_Threshold == "Btw20000_24999" ~ 20000,
      Inc_Threshold == "Btw25000_29999" ~ 25000,
      Inc_Threshold == "Btw30000_34999" ~ 30000,
      Inc_Threshold == "Btw35000_39999" ~ 35000,
      Inc_Threshold == "Btw40000_44999" ~ 40000,
      Inc_Threshold == "Btw45000_49999" ~ 45000,
      Inc_Threshold == "Btw50000_59999" ~ 50000,
      Inc_Threshold == "Btw60000_74999" ~ 60000,
      Inc_Threshold == "Btw75000_99999" ~ 75000,
      Inc_Threshold == "Btw100000_124999" ~ 100000,
      Inc_Threshold == "Btw125000_149999" ~ 125000,
      Inc_Threshold == "Btw150000_199999" ~ 150000,
      Inc_Threshold == "Above200000" ~ 200000,
      TRUE ~ NA_real_
    ),
    upper_limit = case_when(
      Inc_Threshold == "Below10000" ~ 9999,
      Inc_Threshold == "Btw10000_14999" ~ 14999,
      Inc_Threshold == "Btw15000_19999" ~ 19999,
      Inc_Threshold == "Btw20000_24999" ~ 24999,
      Inc_Threshold == "Btw25000_29999" ~ 29999,
      Inc_Threshold == "Btw30000_34999" ~ 34999,
      Inc_Threshold == "Btw35000_39999" ~ 39999,
      Inc_Threshold == "Btw40000_44999" ~ 44999,
      Inc_Threshold == "Btw45000_49999" ~ 49999,
      Inc_Threshold == "Btw50000_59999" ~ 59999,
      Inc_Threshold == "Btw60000_74999" ~ 74999,
      Inc_Threshold == "Btw75000_99999" ~ 99999,
      Inc_Threshold == "Btw100000_124999" ~ 124999,
      Inc_Threshold == "Btw125000_149999" ~ 149999,
      Inc_Threshold == "Btw150000_199999" ~ 199999,
      Inc_Threshold == "Above200000" ~ NA,
      TRUE ~ NA_real_
    )) %>%
  group_by(GEOID) %>% #
  filter(!all(Count == 0)) %>% #Filter tracts with no incomes as this breaks the function (Rejoin after)
  ungroup() %>%
  arrange( #Data must be in order for function
    GEOID, 
    lower_limit 
  ) 

#------------------
#RECURSIVE SUBDIVISION MEDIANS

#First, input the income data and geography
# Get unique identifier (in this case GEOIDs from the census tracts)
unique_geoids <- unique(Geography$GEOID)

# Create an empty data frame to store the results
result <- data.frame(GEOID = character(), Median_Income = numeric(), Mean_Income = numeric(),
                     Variance = numeric(), SD = numeric(), Theils = numeric(), Gini = numeric())

# Repeat for every GEOID/Geography
for (geo_id in unique_geoids) {
  # Filter the data for the current GEOID
  object <- Geography %>%
    filter(GEOID == geo_id)
  
  # Create object and parameters
  binedges <- object$upper_limit
  bincounts <- object$Count
  
  # Use RSUB command to calculate midpoint statistics with pareto
  rsb <- rsubbins(binedges, bincounts, tailShape = "pareto")
  
  # Calculate mean of midpoint
  Mean_Income <- integral(function(x) {1 - rsb$rsubCDF(x)}, 0, rsb$E)
  
  # Calculate median of midpoint
  Median_Income <- uniroot(function(x) rsb$rsubCDF(x) - 0.5, lower = 0, upper = rsb$E)$root
  
  # Calculate general statistics (mean, variance, SD, Thiels, Gini)
  stats <- stats_from_distribution(rsb)
  
  # Extract statistics values
  mean_val <- unname(stats["mean"])
  variance_val <- unname(stats["variance"])
  sd_val <- unname(stats["SD"])
  theils_val <- unname(stats["Theil"])
  gini_val <- unname(stats["Gini"])
  
  # Create a data frame with the results for the current GEOID/unique identifier
  result_geo <- data.frame(
    GEOID = geo_id,
    Median_Income = Median_Income,
    Mean_Income = Mean_Income,
    Variance = variance_val,
    SD = sd_val,
    Theils = theils_val,
    Gini = gini_val)
  
  # Append the result for the current GEOID to the overall result data frame
  result <- bind_rows(result, result_geo)
}

# View the resulting data frame
print(result)

#Reload Geography data
#2020
Richmond <- read_sf(paste0(onedrivepath, "Mapping Richmond/Index/Index_2020/Richmond_Index_2020.shp")) %>%
  rename(Suburban_Index = Sbrbn_I,
         Urban_Index = Urbn_In,
         Landscape_Four = Lndscp_Fr,
         Landscape_Five = Lndscp_Fv,
         Landscape_Six = Lndsc_S,
         Landscape_All = Lndsc_A) %>%
  select(GEOID, Landscape_Five)

#Rejoin result to overarching geography to include those tracts with no income
Med_Inc_RSUB_2020 <- result %>%
  rename_with(~ paste0(., "_RSUB"), -GEOID) %>%
  full_join(Richmond, by="GEOID")

#--------------
#LINEAR INTERPOLATION WITH STEP BINS

# Get unique GEOIDs from the census tracts/unique identifiers from observations
unique_geoids <- unique(Geography$GEOID)

# Create an empty data frame to store the results
result <- data.frame(GEOID = character(), Median_Income = numeric(), Mean_Income = numeric(),
                     Variance = numeric(), SD = numeric(), Theils = numeric(), Gini = numeric())

# Repeat for every GEOID/Geography
for (geo_id in unique_geoids) {
  # Filter the data for the current GEOID
  object <- Geography %>%
    filter(GEOID == geo_id)
  
  # Create object and parameters
  binedges <- object$upper_limit
  bincounts <- object$Count
  
  # Use stepbins to calculate stepbin statistics
  #We use onebin as the results are most closest to med_hh_inc at the highest values
  sb <- stepbins(binedges, bincounts,
                 tailShape = c("onebin"))
  
  # Mean of stepbins
  mean_income <- integral(function(x){1-sb$stepCDF(x)}, 0, sb$E)
  
  # Median of stepbins
  median_income <- uniroot(function(x) sb$stepCDF(x) - 0.5, lower = 0, upper = sb$E)$root
  
  # Calculate general statistics (variance, SD, Theils, Gini)
  stats <- stats_from_distribution(sb)
  
  # Extract statistics values
  variance_val <- unname(stats["variance"])
  sd_val <- unname(stats["SD"])
  theils_val <- unname(stats["Theil"])
  gini_val <- unname(stats["Gini"])
  
  # Create a data frame with the results for the current GEOID/unique identifier
  result_geo <- data.frame(
    GEOID = geo_id,
    Median_Income = median_income,
    Mean_Income = mean_income,
    Variance = variance_val,
    SD = sd_val,
    Theils = theils_val,
    Gini = gini_val)
  
  # Append the result for the current GEOID to the overall result data frame
  result <- bind_rows(result, result_geo)
}

#Reload Geography data
#2020
Richmond <- read_sf(paste0(onedrivepath, "Mapping Richmond/Index/Index_2020/Richmond_Index_2020.shp")) %>%
  rename(Suburban_Index = Sbrbn_I,
         Urban_Index = Urbn_In,
         Landscape_Four = Lndscp_Fr,
         Landscape_Five = Lndscp_Fv,
         Landscape_Six = Lndsc_S,
         Landscape_All = Lndsc_A) %>%
  select(GEOID, Landscape_Five)

#Rejoin result to overarching geography to include those tracts with no income
Med_Inc_Stepbins_2020 <- result %>%
  rename_with(~ paste0(., "_Stepbin"), -GEOID) %>%
  full_join(Richmond, by="GEOID") 

#Save binned data
saveRDS(Med_Inc_Stepbins_2020, file.path(onedrivepath, "Mapping Richmond/Binned Income Data/2020/Med_Inc_Stepbins_2020.rds"))

#--------------
#CUBIC SPLINE APPROACH

# Create an empty dataframe to store result
result <- data.frame(GEOID = character(), Median_Income = numeric(), Mean_Income = numeric(),
                     Variance = numeric(), SD = numeric(), Theils = numeric(), Gini = numeric())

# Count the total number of unique GEOIDs
total_geoids <- length(unique(Geography$GEOID))

# Initialize a counter for processed GEOIDs
processed_geoids <- 0

# Iterate over each GEOID
for (geoid_value in unique(Geography$GEOID)) {
  tryCatch({
    # Filter data for the current GEOID
    GEOID <- Geography %>%
      filter(GEOID == geoid_value)
    
    # Create object and parameters
    binedges <- GEOID$upper_limit
    bincounts <- GEOID$Count
    
    # Creating spline bins
    splb <- splinebins(binedges, bincounts)
    
    
    # Mean of spline
    mean_income <- integral(function(x){1-splb$splineCDF(x)}, 0, splb$E) # closer to given mean
    
    # Median of spline
    median_income <- uniroot(function(x) splb$splineCDF(x) - 0.5, lower = 0, upper = splb$E)$root
    
    # Calculate general statistics (variance, SD, Theils, Gini)
    stats <- stats_from_distribution(splb)
    
    # Extract statistics values
    variance_val <- unname(stats["variance"])
    sd_val <- unname(stats["SD"])
    theils_val <- unname(stats["Theil"])
    gini_val <- unname(stats["Gini"])
    
    # Create a data frame with the result for the current GEOID/unique identifier
    result_geo <- data.frame(
      GEOID = geoid_value,
      Median_Income = median_income,
      Mean_Income = mean_income,
      Variance = variance_val,
      SD = sd_val,
      Theils = theils_val,
      Gini = gini_val)
    
    # Add result to the dataframe
    result <- bind_rows(result, result_geo)
    
    # Increment the counter for processed GEOIDs
    processed_geoids <- processed_geoids + 1
    
    # Print diagnostic message for each processed GEOID
    cat("Processed GEOID:", geoid_value, "\n")
  }, error = function(e) {
    # Print error message and GEOID causing the error
    cat("Error for GEOID:", geoid_value, "\n")
    print(e)
  })
}

# # Print total number of processed GEOIDs
# cat("Total processed GEOIDs:", processed_geoids, "\n")
# # Print total number of unique GEOIDs
# cat("Total unique GEOIDs:", total_geoids, "\n")


#Reload Geography data
#2020
Richmond <- read_sf(paste0(onedrivepath, "Mapping Richmond/Index/Index_2020/Richmond_Index_2020.shp")) %>%
  rename(Suburban_Index = Sbrbn_I,
         Urban_Index = Urbn_In,
         Landscape_Four = Lndscp_Fr,
         Landscape_Five = Lndscp_Fv,
         Landscape_Six = Lndsc_S,
         Landscape_All = Lndsc_A) %>%
  select(GEOID, Landscape_Five)

#Rejoin result to overarching geography to include those tracts with no income
Med_Inc_Splinebins_2020 <- result %>%
  rename_with(~ paste0(., "_Splinebin"), -GEOID) %>%
  full_join(Richmond, by="GEOID") 


#-----------
#REJOIN MEDIANS TO GEOGRAPHY OF INTEREST

#object result against median income to measure differences
Median_Income_Binned_Tract_2020 <- Richmond %>%
  left_join(Med_hh_inc_2020, by="GEOID") %>%
  left_join(Med_Inc_RSUB_2020, by="GEOID") %>%
  left_join(Med_Inc_Stepbins_2020, by="GEOID") %>%
  left_join(Med_Inc_Splinebins_2020, by="GEOID") %>%
  rename(Med_hh_inc_E = B19013_001E,
         Med_hh_inc_MoE = B19013_001M) %>%
  mutate(RSUB_Difference = Median_Income_RSUB - Med_hh_inc_E,
         Stepbin_Difference = Median_Income_Stepbin - Med_hh_inc_E,
         Splinebin_Difference = Median_Income_Splinebin - Med_hh_inc_E) %>%
  select(GEOID, RSUB_Difference, Stepbin_Difference, Splinebin_Difference,
         Med_hh_inc_E, Med_hh_inc_MoE, 
         Median_Income_RSUB, Median_Income_Stepbin, Median_Income_Splinebin)

#Draft plot
ggplot(Med_Inc_Stepbins_2020) +
  geom_sf(aes(fill = Median_Income_Stepbin, geometry = geometry))

#To test whether Medians fall within Med HH income Margin of Error
MoE_Test <- Median_Income_Binned_Tract_2020 %>%
  mutate(
    RSUB_beyond_margin = abs(RSUB_Difference) > Med_hh_inc_MoE,
    Stepbin_beyond_margin = abs(Stepbin_Difference) > Med_hh_inc_MoE,
    Splinebin_beyond_margin = abs(Splinebin_Difference) > Med_hh_inc_MoE
  ) %>%
  summarize(
    RSUB_count = sum(RSUB_beyond_margin, na.rm = TRUE),
    Stepbin_count = sum(Stepbin_beyond_margin, na.rm = TRUE),
    Splinebin_count = sum(Splinebin_beyond_margin, na.rm = TRUE)
  )


#Save Income data
#Tract data
# saveRDS(Median_Income_Binned_Tract_2020, file.path(onedrivepath, "Mapping Richmond/Richmond Data/Income in 2020/Med_Binned_Inc_Compared_2020.rds"))

#------------------------------------------------------------------------------
#Working at the larger scale (in this case, the functions above for the whole MSA)
#Load Richmond Geographies

#Using 2020 median household income data as the reference variable
#2020 Median HH income for Richmond
Med_hh_inc_2020 <- get_acs(
  geography = "cbsa", 
  variables = "B19013_001", 
  #state = ST,
  year = YR4,
  output = "wide") %>%
  filter(str_detect(NAME, "Richmond, VA")) 

#2020
Geography <- read_sf(paste0(onedrivepath, "Mapping Richmond/Index/Index_2020/Richmond_Index_2020.shp")) %>%
  rename(Suburban_Index = Sbrbn_I,
         Urban_Index = Urbn_In,
         Landscape_Four = Lndscp_Fr,
         Landscape_Five = Lndscp_Fv,
         Landscape_Six = Lndsc_S,
         Landscape_All = Lndsc_A) %>%
  select(GEOID, Landscape_Five)

#Join hh data to Richmond state, tidy, and pivot
#Create bin boundaries for the function below
#Remove NAs/0 as function fails on NAs and 0 values
Geography <- Geography %>%
  left_join(Household_Inc_2020, by = "GEOID") %>%
  pivot_longer(cols = matches("^B|^A"),
               names_to = "Inc_Threshold",
               values_to = "Count") %>%
  group_by(Inc_Threshold) %>%         #Group here to test on the entire geography rather than subunit (MSA vs tract)
  summarise(Count = sum(Count)) %>%
  mutate(
    lower_limit = case_when(
      Inc_Threshold == "Below10000" ~ 0,
      Inc_Threshold == "Btw10000_14999" ~ 10000,
      Inc_Threshold == "Btw15000_19999" ~ 15000,
      Inc_Threshold == "Btw20000_24999" ~ 20000,
      Inc_Threshold == "Btw25000_29999" ~ 25000,
      Inc_Threshold == "Btw30000_34999" ~ 30000,
      Inc_Threshold == "Btw35000_39999" ~ 35000,
      Inc_Threshold == "Btw40000_44999" ~ 40000,
      Inc_Threshold == "Btw45000_49999" ~ 45000,
      Inc_Threshold == "Btw50000_59999" ~ 50000,
      Inc_Threshold == "Btw60000_74999" ~ 60000,
      Inc_Threshold == "Btw75000_99999" ~ 75000,
      Inc_Threshold == "Btw100000_124999" ~ 100000,
      Inc_Threshold == "Btw125000_149999" ~ 125000,
      Inc_Threshold == "Btw150000_199999" ~ 150000,
      Inc_Threshold == "Above200000" ~ 200000,
      TRUE ~ NA_real_
    ),
    upper_limit = case_when(
      Inc_Threshold == "Below10000" ~ 9999,
      Inc_Threshold == "Btw10000_14999" ~ 14999,
      Inc_Threshold == "Btw15000_19999" ~ 19999,
      Inc_Threshold == "Btw20000_24999" ~ 24999,
      Inc_Threshold == "Btw25000_29999" ~ 29999,
      Inc_Threshold == "Btw30000_34999" ~ 34999,
      Inc_Threshold == "Btw35000_39999" ~ 39999,
      Inc_Threshold == "Btw40000_44999" ~ 44999,
      Inc_Threshold == "Btw45000_49999" ~ 49999,
      Inc_Threshold == "Btw50000_59999" ~ 59999,
      Inc_Threshold == "Btw60000_74999" ~ 74999,
      Inc_Threshold == "Btw75000_99999" ~ 99999,
      Inc_Threshold == "Btw100000_124999" ~ 124999,
      Inc_Threshold == "Btw125000_149999" ~ 149999,
      Inc_Threshold == "Btw150000_199999" ~ 199999,
      Inc_Threshold == "Above200000" ~ NA,
      TRUE ~ NA_real_
    )) %>%
  arrange( #Data must be in order for function
    lower_limit 
  ) 

#-------------
#RECURSIVE SUBDIVISION MEDIANS 

# Create object and parameters
binedges <- Geography$upper_limit
bincounts <- Geography$Count

# Use Pareto command to calculate midpoint statistics
rsb <- rsubbins(binedges, bincounts, tailShape = "pareto")

# Calculate mean of midpoint
Mean_Income <- integral(function(x) {1 - rsb$rsubCDF(x)}, 0, rsb$E)

# Calculate median of midpoint
Median_Income <- uniroot(function(x) rsb$rsubCDF(x) - 0.5, lower = 0, upper = rsb$E)$root

# Calculate general statistics (mean, variance, SD, Thiels, Gini)
stats <- stats_from_distribution(rsb)

# Extract statistics values
mean_val <- unname(stats["mean"])
variance_val <- unname(stats["variance"])
sd_val <- unname(stats["SD"])
theils_val <- unname(stats["Theil"])
gini_val <- unname(stats["Gini"])

# Extract statistics values
variance_val <- unname(stats["variance"])
sd_val <- unname(stats["SD"])
theils_val <- unname(stats["Theil"])
gini_val <- unname(stats["Gini"])

# Create a data frame with the results
Med_Inc_RSUB_2020 <- data.frame(
  Geography = paste(CITY),
  Median_Income = Mean_Income,
  Mean_Income = Median_Income,
  Variance = variance_val,
  SD = sd_val,
  Theils = theils_val,
  Gini = gini_val
)  %>%
  rename_with(~ paste0(., "_RSUB"), -Geography) 


#STEP BINS APPROACH

# Create object and parameters
binedges <- Geography$upper_limit
bincounts <- Geography$Count

# Use stepbins to calculate stepbin statistics
sb <- stepbins(binedges, bincounts,
               tailShape = c("onebin"))

# Mean of stepbins
mean_income <- integral(function(x){1-sb$stepCDF(x)}, 0, sb$E)

# Median of stepbins
median_income <- uniroot(function(x) sb$stepCDF(x) - 0.5, lower = 0, upper = sb$E)$root

# Calculate general statistics (variance, SD, Theils, Gini)
stats <- stats_from_distribution(sb)

# Extract statistics values
variance_val <- unname(stats["variance"])
sd_val <- unname(stats["SD"])
theils_val <- unname(stats["Theil"])
gini_val <- unname(stats["Gini"])

# Create a data frame with the results
Med_Inc_Stepbins_2020 <- data.frame(
  Geography = paste(CITY),
  Median_Income = median_income,
  Mean_Income = mean_income,
  Variance = variance_val,
  SD = sd_val,
  Theils = theils_val,
  Gini = gini_val
)  %>%
  rename_with(~ paste0(., "_Stepbin"), -Geography) 



#CUBIC SPLINE APPROACH

# Create object and parameters
binedges <- Geography$upper_limit
bincounts <- Geography$Count

# Creating spline bins
splb <- splinebins(binedges, bincounts)

# Mean of spline
mean_income <- integral(function(x){1-splb$splineCDF(x)}, 0, splb$E) # closer to given mean

# Median of spline
#median_income <- uniroot(function(x) splb$splineCDF(x) - 0.5, lower = 0, upper = splb$E)$root

# Calculate general statistics (variance, SD, Theils, Gini)
stats <- stats_from_distribution(splb)

# Extract statistics values
variance_val <- unname(stats["variance"])
sd_val <- unname(stats["SD"])
theils_val <- unname(stats["Theil"])
gini_val <- unname(stats["Gini"])

# Create a data frame with the results
Med_Inc_Splinebins_2020 <- data.frame(
  Geography = paste(CITY),
  Median_Income = median_income,
  Mean_Income = mean_income,
  Variance = variance_val,
  SD = sd_val,
  Theils = theils_val,
  Gini = gini_val
)  %>%
  rename_with(~ paste0(., "_Splinebin"), -Geography) 


# #Combine above medians
# Med_Inc_Pareto_2020 <- Med_Inc_Pareto_2020 %>%
#   pivot_longer(cols = ends_with(c("o")),
#                names_to = "Statistic",
#                values_to = "Count") 
#     
# Med_Inc_Stepbins_2020 <- Med_Inc_Stepbins_2020 %>%
#   pivot_longer(cols = ends_with(c("n")),
#                names_to = "Statistic",
#                values_to = "Count") 
# 
# Med_Inc_Splinebins_2020 <- Med_Inc_Splinebins_2020 %>%
#   pivot_longer(cols = ends_with(c("n")),
#                names_to = "Statistic",
#                values_to = "Count") 

#Long form object of above
# Median_Income_Binned_City <- rbind(Med_Inc_Pareto_2020, Med_Inc_Stepbins_2020, Med_Inc_Splinebins_2020)


#object result against median income to measure differences
Med_Binned_Inc_Compared_MSA_2020 <- Med_hh_inc_2020 %>%
  rename(Geography = NAME) %>%
  mutate(Geography = ifelse(Geography == "Richmond, VA Metro Area", "Richmond MSA", Geography)) %>%
  left_join(Med_Inc_RSUB_2020, by="Geography") %>%
  left_join(Med_Inc_Stepbins_2020, by="Geography") %>%
  left_join(Med_Inc_Splinebins_2020, by="Geography") %>%
  rename(Med_hh_inc_E = B19013_001E,
         Med_hh_inc_MoE = B19013_001M) %>%
  mutate(RSUB_Difference = Median_Income_RSUB - Med_hh_inc_E,
         Stepbin_Difference = Median_Income_Stepbin - Med_hh_inc_E,
         Splinebin_Difference = Median_Income_Splinebin - Med_hh_inc_E) %>%
  select(RSUB_Difference, Stepbin_Difference, Splinebin_Difference,
         Med_hh_inc_E, Med_hh_inc_MoE, 
         Median_Income_RSUB, Median_Income_Stepbin, Median_Income_Splinebin)

#To test whether Medians fall within Med HH income Margin of Error
MoE_Test <- Med_Binned_Inc_Compared_MSA_2020 %>%
  mutate(
    RSUB_beyond_margin = abs(RSUB_Difference) > Med_hh_inc_MoE,
    Stepbin_beyond_margin = abs(Stepbin_Difference) > Med_hh_inc_MoE,
    Splinebin_beyond_margin = abs(Splinebin_Difference) > Med_hh_inc_MoE
  ) %>%
  summarize(
    RSUB_count = sum(RSUB_beyond_margin, na.rm = TRUE),
    Stepbin_count = sum(Stepbin_beyond_margin, na.rm = TRUE),
    Splinebin_count = sum(Splinebin_beyond_margin, na.rm = TRUE)
  )

#MSA data
#saveRDS(Med_Binned_Inc_Compared_MSA_2020, file.path(onedrivepath, "Mapping Richmond/Richmond Data/Income in 2020/Med_Binned_Inc_Compared_MSA_2020.rds"))


#Plotting one geography for comparison
#Use Spline as an x reference

#RSUB
# Generate x-values from spline for all three to create a standard
Spline_X <- seq(0, 500000, length.out = 500)

# Generate y-values
RSUB_Y <- rsb$rsubPDF(Spline_X)

# Create a dataframe
RSUB_Plot_Data <- data.frame(x = Spline_X, y = RSUB_Y) %>%
  rename(Income = x,
         Density = y) %>%
  mutate(Measurement = "RSUB")

#STEP
# Generate y-values
Step_Y <- sb$stepPDF(Spline_X)

# Create a dataframe
Step_Plot_Data <- data.frame(x = Spline_X, y = Step_Y) %>%
  rename(Income = x,
         Density = y) %>%
  mutate(Measurement = "Step")

#SPLINE
# Evaluate the spline function at the x-values
Spline_Y <- splb$splinePDF(Spline_X)  

# Create a dataframe with x and y values
Spline_Plot_Data <- data.frame(Income = Spline_X, Density = Spline_Y) %>%
  mutate(Measurement = "Spline")

#Join objects in long form for one plot
Measurement_Plot <- rbind(RSUB_Plot_Data, Step_Plot_Data, Spline_Plot_Data)

# Plot using ggplot2
ggplot(data = Measurement_Plot) +
  annotate("rect", xmin = 250, xmax = 9400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 10600, xmax = 14400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 15600, xmax = 19400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 20600, xmax = 24400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 25600, xmax = 29400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 30600, xmax = 34400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 35600, xmax = 39400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 40600, xmax = 44400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 45600, xmax = 49400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 50600, xmax = 59400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 60600, xmax = 74400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 75600, xmax = 99400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 100600, xmax = 124400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 125600, xmax = 149400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 150600, xmax = 199400, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  annotate("rect", xmin = 200600, xmax = 499000, ymin = 0.00000001, ymax = 0.0000086, fill = "#ffedde", alpha = 0.75) +
  geom_vline(xintercept = 71223, linetype = "longdash", color = "black", linewidth = 0.8, alpha = 0.75) +
  geom_line(aes(x = Income, y = Density, color = Measurement, size = Measurement)) +
  scale_color_manual(name = "Method",
                     values = c("#7570b3", "#66a61e", "#e7298a"),
                     labels =c("Recursive\nsubdivision\nsmoothing", 
                               "Cubic spline\ninterpolation", 
                               "Linear\ninterpolation\nstep function")) +
  scale_size_manual(name = "Method",
                    values = c(2.5, 1.25, 1),
                    guide = "none") +
  theme_minimal() +
  theme(legend.spacing = unit(1.0, 'cm'),
        legend.text = element_text(margin = margin(t = 10)),
        panel.grid.major.x = element_line(size = 0, color = "grey"),
        panel.grid.minor.x = element_line(size = 0),
        panel.grid.major.y = element_line(color = "grey", size = 0),
        panel.grid.minor.y = element_line(color = "lightgrey", size = 0)) +
  labs(y = NULL, 
       x = "Household Income",
       caption = "Note: Background shading represents bins within ACS Table B19001.") +
  scale_x_continuous(labels = label_dollar(),
                     breaks = c(0, 50000, 100000, 200000, 300000, 400000,
                                500000),
                     expand = c(0, 5000)) +
  scale_y_continuous(expand = c(0, 0.00000003),
                     labels = NULL) +
  geom_text(x = 205000, y = 0.0000071, label = "Median household income \nfor Richmond MSA", 
            color = "black", size = 4, fontface = "bold") +
  geom_text(x = 205000, y = 0.0000066, label = "ACS 2016-2020, Table B19013.", 
            color = "darkgrey", size = 4, fontface = "italic") +
  geom_segment(aes(x = 153000, y = 0.0000069, xend = 73000, yend = 0.0000071), 
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  guides(colour = guide_legend(override.aes = list(size=5, alpha = 1))) 

#Save object
ggsave("Binned_Inc_Methods.png",
       path = "~/desktop",
       width = 9,
       height = 6,
       units = "in",
       dpi = 500)

