#3/8/23, initiated by BS
#Goal: Adjusting household income for inflation
#To take a string of household income entries and adjust it to a specific year

#Rdocumentation:
# https://www.rdocumentation.org/packages/priceR/versions/1.0.1

#Libraries
require(tidyverse)
require(priceR)

#Create a filepath to OneDrive
onedrivepath="~/OneDrive - The Pennsylvania State University/"


#--------------------------------------------------------------------------------------------------------
#Show countries included in package
Available_Countries <- show_countries()

#Retreive inflation data
Country <- "US"
Inflation_Data <- retrieve_inflation_data(country)
  
Inflation_Data <- retrieve_inflation_data(country, countries_dataframe)


#----------------------------------------------------------------------
#Load in data
Med_Inc_All_1990 <- readRDS(file.path(onedrivepath, "Mapping Richmond/Binned Income Data", "Med_Inc_1990_Total_Pop.rds"))

#Create new column which adjust the median value for inflation (1990 $ to 2020 $)
#Note: origin year must be included in the dataframe

#In this case, 1990 to 2020 buying power.
Med_Inc_All_1990 <- Med_Inc_All_1990 %>%
  mutate(Med_Income_Adj = adjust_for_inflation(Med_Inc_All_1990$Median_Income_Stepbin, 
                                               Med_Inc_All_1990$Year, "US", to_date = 2020))
