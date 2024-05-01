#3/8/23, initiated by BS
#Goal: Geographic Crosswalk work
#To map the geographic cross walks from 1990 to 2020, firstly at block then aggregate up


#Libraries
require(tidyverse)
require(tidycensus)
require(sf)

options(tigris_use_cache = TRUE)

#Create a filepath to OneDrive
onedrivepath="~/OneDrive - The Pennsylvania State University/"

#Set parameters (state and year)
ST = "VA"
GEOG = "tract"

YR1 = 1990
YR2 = 2000
YR3 = 2010
YR4 = 2020


#----------------------------------------------------------------------------------------------------------------
#POVERTY CROSS WALKS


#1990 POVERTY DATA TO 2020 TRACT BOUNDARIES

#Load 1990 Block Group parts data CSV from NHGIS
#Create GEOID from FIPS codes, then tidy columns names/totals
#Calculate totals before crosswalk work
PovertyLines_1990 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Poverty in 1989/nhgis0059_ds123_1990_blck_grp_598.csv")) %>%
  rename(Under50 = "E1C001",
         Between50_75 = "E1C002",
         Between75_100 = "E1C003",
         Between100_125 = "E1C004",
         Between125_150 = "E1C005",
         Between150_175 = "E1C006",
         Between175_185 = "E1C007",
         Between185_200 = "E1C008",
         Above200 = "E1C009") %>%
  mutate(TotalPop = Under50 + Between50_75 + Between75_100 + Between100_125 + Between125_150 +
                    Between150_175 + Between175_185 + Between185_200 + Above200,
         Under100 = Under50 + Between50_75 + Between75_100,
         Under150 = Under50 + Between50_75 + Between75_100 + Between100_125 + Between125_150,
         Under200 = Under50 + Between50_75 + Between75_100 + Between100_125 + Between125_150 +
           Between150_175 + Between175_185 + Between185_200,
         Between50_100 = Between50_75 + Between75_100,
         Between100_150 = Between100_125 + Between125_150,
         Between150_200 = Between150_175 + Between175_185 + Between185_200) %>%
  select(GISJOIN, TotalPop, Under50, Under100, Under150, Under200, Above200, 
         Between50_100, Between100_150, Between150_200)

#Load in crosswalks
#1990 to 2010 Block Group Parts crosswalks
#Then create a matching GISJOIN column from the NHGIS data
CW_BGparts1990to2010 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bgp1990_bg2010_51.csv")) %>%
  mutate(GISJOIN = bgp1990gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
#After this, we should have 1990 data crosswalked to 2010 block groups
Poverty_1990_Crosswalked <- CW_BGparts1990to2010 %>%
  left_join(PovertyLines_1990, by="GISJOIN") %>%
  mutate(across(c(TotalPop, Under50, Under100, Under150, Under200, Above200, 
                  Between50_100, Between100_150, Between150_200), ~ . * wt_pop)) %>%
  group_by(bg2010gj) %>%
  summarize(
    TotalPop = sum(TotalPop),
    Under50 = sum(Under50),
    Under100 = sum(Under100),
    Under150 = sum(Under150),
    Under200 = sum(Under200),
    Above200 = sum(Above200),
    Between50_100 = sum(Between50_100),
    Between100_150 = sum(Between100_150),
    Between150_200 = sum(Between150_200)) %>%
  mutate(GISJOIN = bg2010gj)

#Load in next crosswalk data
#Load in 2010 block group to 2020 tract crosswalk, and create GISJOIN
CW_Tracts2010to2020 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bg2010_tr2020_51.csv")) %>%
  mutate(GISJOIN = bg2010gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
Poverty_1990_Crosswalked <- CW_Tracts2010to2020 %>%
  left_join(Poverty_1990_Crosswalked, by="GISJOIN") %>%
  mutate(across(c(TotalPop, Under50, Under100, Under150, Under200, Above200, 
                  Between50_100, Between100_150, Between150_200), ~ . * wt_pop)) %>%
  group_by(tr2020ge) %>%
  summarize(
    TotalPop = sum(TotalPop),
    Under50 = sum(Under50),
    Under100 = sum(Under100),
    Under150 = sum(Under150),
    Under200 = sum(Under200),
    Above200 = sum(Above200),
    Between50_100 = sum(Between50_100),
    Between100_150 = sum(Between100_150),
    Between150_200 = sum(Between150_200)) 

#General tidying of Crosswalked data
Poverty_1990_Crosswalked <- Poverty_1990_Crosswalked %>%
  rename(GEOID = tr2020ge) %>%
  mutate(Year = YR1,
         GEOID = as.character(GEOID)) %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11))


#--------------------------------------------

# 2000 POVERTY DATA TO 2020 TRACT BOUNDARIES

#This follows 1990 - 2000 BG Parts to 2010 Block groups, then 2020 tracts
PovertyLines_2000 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Poverty in 1999/nhgis0060_ds152_2000_blck_grp_090.csv")) %>%
  rename(Under50 = "HHG001",
       Between50_75 = "HHG002",
       Between75_100 = "HHG003",
       Between100_125 = "HHG004",
       Between125_150 = "HHG005",
       Between150_175 = "HHG006",
       Between175_185 = "HHG007",
       Between185_200 = "HHG008",
       Above200 = "HHG009") %>%
  mutate(TotalPop = Under50 + Between50_75 + Between75_100 + Between100_125 + Between125_150 +
           Between150_175 + Between175_185 + Between185_200 + Above200,
         Under100 = Under50 + Between50_75 + Between75_100,
         Under150 = Under50 + Between50_75 + Between75_100 + Between100_125 + Between125_150,
         Under200 = Under50 + Between50_75 + Between75_100 + Between100_125 + Between125_150 +
           Between150_175 + Between175_185 + Between185_200,
         Between50_100 = Between50_75 + Between75_100,
         Between100_150 = Between100_125 + Between125_150,
         Between150_200 = Between150_175 + Between175_185 + Between185_200) %>%
  select(GISJOIN, TotalPop, Under50, Under100, Under150, Under200, Above200, 
         Between50_100, Between100_150, Between150_200)


#Load in crosswalks
#2000 to 2010 Block Group Parts crosswalks
#Then create a matching GISJOIN column from the NHGIS data
CW_BGparts2000to2010 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk2000to2020/nhgis_bgp2000_bg2010_51.csv")) %>%
  mutate(GISJOIN = bgp2000gj)

#Join first crosswalks to census data
#Then multiply the data by the population weight
#After this, we should have 1990 data crosswalked to 2010 block groups
Poverty_2000_Crosswalked <- CW_BGparts2000to2010 %>%
  left_join(PovertyLines_2000, by="GISJOIN") %>%
  mutate(across(c(TotalPop, Under50, Under100, Under150, Under200, Above200, 
                  Between50_100, Between100_150, Between150_200), ~ . * wt_pop)) %>%
  group_by(bg2010gj) %>%
  summarize(
    TotalPop = sum(TotalPop),
    Under50 = sum(Under50),
    Under100 = sum(Under100),
    Under150 = sum(Under150),
    Under200 = sum(Under200),
    Above200 = sum(Above200),
    Between50_100 = sum(Between50_100),
    Between100_150 = sum(Between100_150),
    Between150_200 = sum(Between150_200)) %>%
  mutate(GISJOIN = bg2010gj)

#Load in next crosswalk data
#Load in 2010 block group to 2020 tract crosswalk, and create GISJOIN
CW_Tracts2010to2020 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bg2010_tr2020_51.csv")) %>%
  mutate(GISJOIN = bg2010gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
Poverty_2000_Crosswalked <- CW_Tracts2010to2020 %>%
  left_join(Poverty_2000_Crosswalked, by="GISJOIN") %>%
  mutate(across(c(TotalPop, Under50, Under100, Under150, Under200, Above200, 
                  Between50_100, Between100_150, Between150_200), ~ . * wt_pop)) %>%
  group_by(tr2020ge) %>%
  summarize(
    TotalPop = sum(TotalPop),
    Under50 = sum(Under50),
    Under100 = sum(Under100),
    Under150 = sum(Under150),
    Under200 = sum(Under200),
    Above200 = sum(Above200),
    Between50_100 = sum(Between50_100),
    Between100_150 = sum(Between100_150),
    Between150_200 = sum(Between150_200)) 

#General tidying of Crosswalked data
Poverty_2000_Crosswalked <- Poverty_2000_Crosswalked %>%
  rename(GEOID = tr2020ge) %>%
  mutate(Year = YR2,
         GEOID = as.character(GEOID)) %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11))

#--------------------------------------------

#2010 POVERTY DATA TO 2020 TRACT BOUNDARIES
#This aligns 2010 BG data with 2020 tracts

# 2010 POVERTY DATA TO 2020 TRACT BOUNDARIES
PovertyLines_2010 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Poverty in 2010/nhgis0061_ds176_20105_blck_grp.csv")) %>%
  rename(TotalPop = "JOCE001", 
         Under50 = "JOCE002",
         Between50_100 = "JOCE003",
         Between100_125 = "JOCE004",
         Between125_150 = "JOCE005",
         Between150_185 = "JOCE006",
         Between185_200 = "JOCE007",
         Above200 = "JOCE008") %>%
  mutate(Under100 = Under50 + Between50_100,
         Under150 = Under50 + Between50_100 + Between100_125 + Between125_150,
         Under200 = Under50 + Between50_100 + Between100_125 + Between125_150 +
           Between150_185 + Between185_200,
         Between100_150 = Between100_125 + Between125_150,
         Between150_200 = Between150_185 + Between185_200) %>%
  select(GISJOIN, TotalPop, Under50, Under100, Under150, Under200, Above200, 
         Between50_100, Between100_150, Between150_200)


#Load in 2010 block group to 2020 tract crosswalk, and create GISJOIN
CW_Tracts2010to2020 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bg2010_tr2020_51.csv")) %>%
  mutate(GISJOIN = bg2010gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
Poverty_2010_Crosswalked <- CW_Tracts2010to2020 %>%
  left_join(PovertyLines_2010, by="GISJOIN") %>%
  mutate(across(c(TotalPop, Under50, Under100, Under150, Under200, Above200, 
                  Between50_100, Between100_150, Between150_200), ~ . * wt_pop)) %>%
  group_by(tr2020ge) %>%
  summarize(
    TotalPop = sum(TotalPop),
    Under50 = sum(Under50),
    Under100 = sum(Under100),
    Under150 = sum(Under150),
    Under200 = sum(Under200),
    Above200 = sum(Above200),
    Between50_100 = sum(Between50_100),
    Between100_150 = sum(Between100_150),
    Between150_200 = sum(Between150_200)) 

#General tidying of Crosswalked data
Poverty_2010_Crosswalked <- Poverty_2010_Crosswalked %>%
  rename(GEOID = tr2020ge) %>%
  mutate(Year = YR3,
         GEOID = as.character(GEOID)) %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11))





#----------------------------------------------------------------------------------------------------------------
#POVERTY LINE BY RACE CROSSWALKS
#Testing Black/White above/below

####Remains unfinished, there is no poverty by race block group level data in 2010 that can be cross walked to 2020 tract

#1990
#Load in CSV from NHGIS
#Create GEOID from FIPS codes, then tidy columns names/totals
Poverty_1990 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Poverty in 1989/nhgis0062_ds123_1990_blck_grp_598.csv")) %>%
  mutate(WhiteAbove = E09001 + E09002 + E09003 + E09004 + E09005 + E09006 + E09007,
         BlackAbove = E09008 + E09009 + E09010 + E09011 + E09012 + E09013 + E09014,
         NativeAbove = E09015 + E09016 + E09017 + E09018 + E09019 + E09020 + E09021,
         AsianAbove = E09022 + E09023 + E09024 + E09025 + E09026 + E09027 + E09028,
         OtherAbove = E09029 + E09030 + E09031 + E09032 + E09033 + E09034 + E09035,
         WhiteBelow = E09036 + E09037 + E09038 + E09039 + E09040 + E09041 + E09042,
         BlackBelow = E09043 + E09044 + E09045 + E09046 + E09047 + E09048 + E09049,
         NativeBelow = E09050 + E09051 + E09052 + E09053 + E09054 + E09055 + E09056,
         AsianBelow = E09057 + E09058 + E09059 + E09060 + E09061 + E09062 + E09063,
         OtherBelow = E09064 + E09065 + E09066 + E09067 + E09068 + E09069 + E09070) %>%
  mutate(
    TotalPop = WhiteAbove + BlackAbove + NativeAbove + AsianAbove + OtherAbove +
      WhiteBelow + BlackBelow + NativeBelow + AsianBelow + OtherBelow) %>%
  select(GISJOIN, TotalPop, WhiteAbove, BlackAbove,
           WhiteBelow, BlackBelow) 

#Load in crosswalks
#1990 to 2010 Block Group Parts crosswalks
#Then create a matching GISJOIN column from the NHGIS data
CW_BGparts1990to2010 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bgp1990_bg2010_51.csv")) %>%
  mutate(GISJOIN = bgp1990gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
#After this, we should have 1990 data crosswalked to 2010 block groups
Poverty_1990_Crosswalked <- CW_BGparts1990to2010 %>%
  left_join(Poverty_1990, by="GISJOIN") %>%
  mutate(across(c(TotalPop, WhiteAbove, BlackAbove,
                  WhiteBelow, BlackBelow), ~ . * wt_pop)) %>%
  group_by(bg2010gj) %>%
  summarize(
    TotalPop = sum(TotalPop),
    WhiteAbove = sum(WhiteAbove),
    BlackAbove = sum(BlackAbove),
    WhiteBelow = sum(WhiteBelow),
    BlackBelow = sum(BlackBelow)) %>%
  mutate(GISJOIN = bg2010gj)

#Load in next crosswalk data
#Load in 2010 block group to 2020 tract crosswalk, and create GISJOIN
CW_Tracts2010to2020 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bg2010_tr2020_51.csv")) %>%
  mutate(GISJOIN = bg2010gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
Poverty_1990_Crosswalked <- CW_Tracts2010to2020 %>%
  left_join(Poverty_1990_Crosswalked, by="GISJOIN") %>%
  mutate(across(c(TotalPop, WhiteAbove, BlackAbove,
                  WhiteBelow, BlackBelow), ~ . * wt_pop)) %>%
  group_by(tr2020ge) %>%
  summarize(
    TotalPop = sum(TotalPop),
    WhiteAbove = sum(WhiteAbove),
    BlackAbove = sum(BlackAbove),
    WhiteBelow = sum(WhiteBelow),
    BlackBelow = sum(BlackBelow)) 
  
#General tidying of Crosswalked data
Poverty_1990_Crosswalked <- Poverty_1990_Crosswalked %>%
  rename(GEOID = tr2020ge) %>%
  mutate(Year = YR1,
         GEOID = as.character(GEOID)) %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11))


#Black/White above/below 2000

#This follows 1990 - 2000 BG Parts to 2010 Block groups, then 2020 tracts
#Load in CSV from NHGIS
#Create GEOID from FIPS codes, then tidy columns names/totals
Poverty_2000 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Poverty in 1999/nhgis0066_ds152_2000_blck_grp_090.csv")) %>%
  rename(WhiteAbove = HM3002,
         BlackAbove = HM3004,
         NativeAbove = HM3006,
         AsianAbove = HM3008,
         PIAbove = HM3010,
         OtherAbove = HM3012,
         TwoRaceAbove = HM3014,
         WhiteBelow = HM3001,
         BlackBelow = HM3003,
         NativeBelow = HM3005,
         AsianBelow = HM3007,
         PIBelow = HM3009,
         OtherBelow = HM3011,
         TwoRaceBelow = HM3013) %>%
  mutate(
    TotalPop = WhiteAbove + BlackAbove + NativeAbove + AsianAbove + PIAbove + OtherAbove + TwoRaceAbove +
                      WhiteBelow + BlackBelow + NativeBelow + AsianBelow + PIBelow + OtherBelow + TwoRaceBelow) %>%
  select(GISJOIN, TotalPop, TotalPop, WhiteAbove, BlackAbove,
         WhiteBelow, BlackBelow) 

#Load in crosswalks
#2000 to 2010 Block Group Parts crosswalks
#Then create a matching GISJOIN column from the NHGIS data
CW_BGparts2000to2010 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk2000to2020/nhgis_bgp2000_bg2010_51.csv")) %>%
  mutate(GISJOIN = bgp2000gj)

#Join first crosswalks to census data
#Then multiply the data by the population weight
#After this, we should have 1990 data crosswalked to 2010 block groups
Poverty_2000_Crosswalked <- CW_BGparts2000to2010 %>%
  left_join(Poverty_2000, by="GISJOIN") %>%
  mutate(across(c(TotalPop, TotalPop, TotalPop, WhiteAbove, BlackAbove,
                  WhiteBelow, BlackBelow), ~ . * wt_pop)) %>%
  group_by(bg2010gj) %>%
  summarize(
    TotalPop = sum(TotalPop),
    WhiteAbove = sum(WhiteAbove),
    BlackAbove = sum(BlackAbove),
    WhiteBelow = sum(WhiteBelow),
    BlackBelow = sum(BlackBelow)) %>%
  mutate(GISJOIN = bg2010gj)

#Load in next crosswalk data
#Load in 2010 block group to 2020 tract crosswalk, and create GISJOIN
CW_Tracts2010to2020 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bg2010_tr2020_51.csv")) %>%
  mutate(GISJOIN = bg2010gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
Poverty_2000_Crosswalked <- CW_Tracts2010to2020 %>%
  left_join(Poverty_2000_Crosswalked, by="GISJOIN") %>%
  mutate(across(c(TotalPop, TotalPop, TotalPop, WhiteAbove, BlackAbove,
                  WhiteBelow, BlackBelow), ~ . * wt_pop)) %>%
  group_by(tr2020ge) %>%
  summarize(
    TotalPop = sum(TotalPop),
    WhiteAbove = sum(WhiteAbove),
    BlackAbove = sum(BlackAbove),
    WhiteBelow = sum(WhiteBelow),
    BlackBelow = sum(BlackBelow)) 

#General tidying of Crosswalked data
Poverty_2000_Crosswalked <- Poverty_2000_Crosswalked %>%
  rename(GEOID = tr2020ge) %>%
  mutate(Year = YR2,
         GEOID = as.character(GEOID)) %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11))




#2010 POVERTY DATA TO 2020 TRACT BOUNDARIES
#This aligns 2010 BG data with 2020 tracts

# 2010 POVERTY DATA TO 2020 TRACT BOUNDARIES
PovertyLines_2010 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Poverty in 2010/nhgis0061_ds176_20105_blck_grp.csv")) %>%
  rename(TotalPop = "JOCE001", 
         Under50 = "JOCE002",
         Between50_100 = "JOCE003",
         Between100_125 = "JOCE004",
         Between125_150 = "JOCE005",
         Between150_185 = "JOCE006",
         Between185_200 = "JOCE007",
         Above200 = "JOCE008") %>%
  mutate(Under100 = Under50 + Between50_100,
         Under150 = Under50 + Between50_100 + Between100_125 + Between125_150,
         Under200 = Under50 + Between50_100 + Between100_125 + Between125_150 +
           Between150_185 + Between185_200,
         Between100_150 = Between100_125 + Between125_150,
         Between150_200 = Between150_185 + Between185_200) %>%
  select(GISJOIN, TotalPop, Under50, Under100, Under150, Under200, Above200, 
         Between50_100, Between100_150, Between150_200)


#Load in 2010 block group to 2020 tract crosswalk, and create GISJOIN
CW_Tracts2010to2020 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bg2010_tr2020_51.csv")) %>%
  mutate(GISJOIN = bg2010gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
Poverty_2010_Crosswalked <- CW_Tracts2010to2020 %>%
  left_join(PovertyLines_2010, by="GISJOIN") %>%
  mutate(across(c(TotalPop, Under50, Under100, Under150, Under200, Above200, 
                  Between50_100, Between100_150, Between150_200), ~ . * wt_pop)) %>%
  group_by(tr2020ge) %>%
  summarize(
    TotalPop = sum(TotalPop),
    Under50 = sum(Under50),
    Under100 = sum(Under100),
    Under150 = sum(Under150),
    Under200 = sum(Under200),
    Above200 = sum(Above200),
    Between50_100 = sum(Between50_100),
    Between100_150 = sum(Between100_150),
    Between150_200 = sum(Between150_200)) 

#General tidying of Crosswalked data
Poverty_2010_Crosswalked <- Poverty_2010_Crosswalked %>%
  rename(GEOID = tr2020ge) %>%
  mutate(Year = YR3,
         GEOID = as.character(GEOID)) %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11))



#----------------------------------------------------------------------------------------------------------------
#HOUSEHOLD INCOME
#To crosswalk the number by income distribution and then work out medians etc.

#Load 1990 Block Group parts data CSV from NHGIS
#Tidy columns to match 2020 income data (from ACS)
Household_Inc_1989 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Median Income in 1989/nhgis0070_ds123_1990_blck_grp_598.csv")) %>%
  rename(Below_5000 = "E4T001",
         Btw5000_9999 = "E4T002",
         Btw10000_12499 = "E4T003",
         Btw12500_14999 = "E4T004",
         Btw15000_17499 = "E4T005",
         Btw17500_19999 = "E4T006",
         Btw20000_22499 = "E4T007",
         Btw22500_24999 = "E4T008",
         Btw25000_27499 = "E4T009",
         Btw27500_29999 = "E4T010",
         Btw30000_32499 = "E4T011",
         Btw32500_34999 = "E4T012",
         Btw35000_37499 = "E4T013",
         Btw37500_39999 = "E4T014",
         Btw40000_42499 = "E4T015",
         Btw42500_44499 = "E4T016",
         Btw45000_47499 = "E4T017",
         Btw47500_49999 = "E4T018",
         Btw50000_54499 = "E4T019",
         Btw55000_59999 = "E4T020",
         Btw60000_74999 = "E4T021",
         Btw75000_99999 = "E4T022",
         Btw100000_124999 = "E4T023",
         Btw125000_149999 = "E4T024",
         Above150000 = "E4T025") %>%
  mutate(Below10000 = Below_5000 + Btw5000_9999,
         Btw10000_14999 = Btw10000_12499 + Btw12500_14999,
         Btw15000_19999 = Btw15000_17499 + Btw17500_19999,
         Btw20000_24999 = Btw20000_22499 + Btw22500_24999,
         Btw25000_29999 = Btw25000_27499 + Btw27500_29999,
         Btw30000_34999 = Btw30000_32499 + Btw32500_34999,
         Btw35000_39999 = Btw35000_37499 + Btw37500_39999,
         Btw40000_44999 = Btw40000_42499 + Btw42500_44499,
         Btw45000_49999 = Btw45000_47499 + Btw47500_49999,
         Btw50000_59999 = Btw50000_54499 + Btw55000_59999) %>%
  select(GISJOIN, Below10000, Btw10000_14999, Btw15000_19999, Btw20000_24999, Btw25000_29999, Btw30000_34999,
         Btw35000_39999, Btw40000_44999, Btw45000_49999, Btw50000_59999, Btw60000_74999, Btw75000_99999,
         Btw100000_124999, Btw125000_149999, Above150000)

#Load in crosswalks
#1990 to 2010 Block Group Parts crosswalks
#Then create a matching GISJOIN column from the NHGIS data
CW_BGparts1990to2010 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bgp1990_bg2010_51.csv")) %>%
  mutate(GISJOIN = bgp1990gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
#After this, we should have 1990 data crosswalked to 2010 block groups
Household_Inc_1989 <- CW_BGparts1990to2010 %>%
  left_join(Household_Inc_1989, by="GISJOIN") %>%
  mutate(across(c(Below10000, Btw10000_14999, Btw15000_19999, Btw20000_24999, Btw25000_29999, 
                  Btw30000_34999,Btw35000_39999, Btw40000_44999, Btw45000_49999, Btw50000_59999, 
                  Btw60000_74999, Btw75000_99999, Btw100000_124999, Btw125000_149999, Above150000), ~ . * wt_hh)) %>%
  group_by(bg2010gj) %>%
  summarize(
    Below10000 = sum(Below10000),
    Btw10000_14999 = sum(Btw10000_14999),
    Btw15000_19999 = sum(Btw15000_19999),
    Btw20000_24999 = sum(Btw20000_24999),
    Btw25000_29999 = sum(Btw25000_29999),
    Btw30000_34999 = sum(Btw30000_34999),
    Btw35000_39999 = sum(Btw35000_39999),
    Btw40000_44999 = sum(Btw40000_44999),
    Btw45000_49999 = sum(Btw45000_49999),
    Btw50000_59999 = sum(Btw50000_59999),
    Btw60000_74999 = sum(Btw60000_74999),
    Btw75000_99999 = sum(Btw75000_99999),
    Btw100000_124999 = sum(Btw100000_124999),
    Btw125000_149999 = sum(Btw125000_149999),
    Above150000 = sum(Above150000)) %>%
  mutate(GISJOIN = bg2010gj)

#Load in next crosswalk data
#Load in 2010 block group to 2020 tract crosswalk, and create GISJOIN
CW_Tracts2010to2020 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bg2010_tr2020_51.csv")) %>%
  mutate(GISJOIN = bg2010gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
Household_Inc_1989 <- CW_Tracts2010to2020 %>%
  left_join(Household_Inc_1989, by="GISJOIN") %>%
  mutate(across(c(Below10000, Btw10000_14999, Btw15000_19999, Btw20000_24999, Btw25000_29999, 
                  Btw30000_34999,Btw35000_39999, Btw40000_44999, Btw45000_49999, Btw50000_59999, 
                  Btw60000_74999, Btw75000_99999, Btw100000_124999, Btw125000_149999, Above150000), ~ . * wt_hh)) %>%
  group_by(tr2020ge) %>%
  summarize(
    Below10000 = sum(Below10000),
    Btw10000_14999 = sum(Btw10000_14999),
    Btw15000_19999 = sum(Btw15000_19999),
    Btw20000_24999 = sum(Btw20000_24999),
    Btw25000_29999 = sum(Btw25000_29999),
    Btw30000_34999 = sum(Btw30000_34999),
    Btw35000_39999 = sum(Btw35000_39999),
    Btw40000_44999 = sum(Btw40000_44999),
    Btw45000_49999 = sum(Btw45000_49999),
    Btw50000_59999 = sum(Btw50000_59999),
    Btw60000_74999 = sum(Btw60000_74999),
    Btw75000_99999 = sum(Btw75000_99999),
    Btw100000_124999 = sum(Btw100000_124999),
    Btw125000_149999 = sum(Btw125000_149999),
    Above150000 = sum(Above150000)) 

#General tidying of Crosswalked data
Household_Inc_1989_Crosswalked <- Household_Inc_1989 %>%
  rename(GEOID = tr2020ge) %>%
  mutate(Year = YR1,
         GEOID = as.character(GEOID)) %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11))

#--------
#2000

#Load 2000 Block Group parts data CSV from NHGIS
#Tidy columns to match 2020 income data (from ACS)
Household_Inc_1999 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Income in 1999/nhgis0071_ds152_2000_blck_grp_090.csv")) %>%
  rename(Below10000 = "HF5001",
         Btw10000_14999 = "HF5002",
         Btw15000_19999 = "HF5003",
         Btw20000_24999 = "HF5004",
         Btw25000_29999 = "HF5005",
         Btw30000_34999 = "HF5006",
         Btw35000_39999 = "HF5007",
         Btw40000_44999 = "HF5008",
         Btw45000_49999 = "HF5009",
         Btw50000_59999 = "HF5010",
         Btw60000_74999 = "HF5011",
         Btw75000_99999 = "HF5012",
         Btw100000_124999 = "HF5013",
         Btw125000_149999 = "HF5014",
         Btw150000_199999 = "HF5015",
         Above200000 = "HF5016") %>%
  mutate(Above150000 = Btw150000_199999 + Above200000) %>%
  select(GISJOIN, Below10000, Btw10000_14999, Btw15000_19999, Btw20000_24999, Btw25000_29999, Btw30000_34999,
         Btw35000_39999, Btw40000_44999, Btw45000_49999, Btw50000_59999, Btw60000_74999, Btw75000_99999,
         Btw100000_124999, Btw125000_149999, Above150000, Btw150000_199999, Above200000)


#Load in crosswalks
#2000 to 2010 Block Group Parts crosswalks
#Then create a matching GISJOIN column from the NHGIS data
CW_BGparts2000to2010 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk2000to2020/nhgis_bgp2000_bg2010_51.csv")) %>%
  mutate(GISJOIN = bgp2000gj)

#Join first crosswalks to census data
#Then multiply the data by the population weight
#After this, we should have 1990 data crosswalked to 2010 block groups
Household_Inc_1999_Crosswalked <- CW_BGparts2000to2010 %>%
  left_join(Household_Inc_1999, by="GISJOIN") %>%
  mutate(across(c(Below10000, Btw10000_14999, Btw15000_19999, Btw20000_24999, Btw25000_29999, 
                  Btw30000_34999,Btw35000_39999, Btw40000_44999, Btw45000_49999, Btw50000_59999, 
                  Btw60000_74999, Btw75000_99999, Btw100000_124999, Btw125000_149999, Above150000,
                  Btw150000_199999, Above200000), ~ . * wt_hh)) %>%
  group_by(bg2010gj) %>%
  summarize(
    Below10000 = sum(Below10000),
    Btw10000_14999 = sum(Btw10000_14999),
    Btw15000_19999 = sum(Btw15000_19999),
    Btw20000_24999 = sum(Btw20000_24999),
    Btw25000_29999 = sum(Btw25000_29999),
    Btw30000_34999 = sum(Btw30000_34999),
    Btw35000_39999 = sum(Btw35000_39999),
    Btw40000_44999 = sum(Btw40000_44999),
    Btw45000_49999 = sum(Btw45000_49999),
    Btw50000_59999 = sum(Btw50000_59999),
    Btw60000_74999 = sum(Btw60000_74999),
    Btw75000_99999 = sum(Btw75000_99999),
    Btw100000_124999 = sum(Btw100000_124999),
    Btw125000_149999 = sum(Btw125000_149999),
    Above150000 = sum(Above150000),
    Btw150000_199999 = sum(Btw150000_199999),
    Above200000 = sum(Above200000)) %>%
  mutate(GISJOIN = bg2010gj)

#Load in next crosswalk data
#Load in 2010 block group to 2020 tract crosswalk, and create GISJOIN
CW_Tracts2010to2020 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bg2010_tr2020_51.csv")) %>%
  mutate(GISJOIN = bg2010gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
Household_Inc_1999_Crosswalked <- CW_Tracts2010to2020 %>%
  left_join(Household_Inc_1999_Crosswalked, by="GISJOIN") %>%
  mutate(across(c(Below10000, Btw10000_14999, Btw15000_19999, Btw20000_24999, Btw25000_29999, 
                  Btw30000_34999,Btw35000_39999, Btw40000_44999, Btw45000_49999, Btw50000_59999, 
                  Btw60000_74999, Btw75000_99999, Btw100000_124999, Btw125000_149999, Above150000,
                  Btw150000_199999, Above200000), ~ . * wt_hh)) %>%
  group_by(tr2020ge) %>%
  summarize(
    Below10000 = sum(Below10000),
    Btw10000_14999 = sum(Btw10000_14999),
    Btw15000_19999 = sum(Btw15000_19999),
    Btw20000_24999 = sum(Btw20000_24999),
    Btw25000_29999 = sum(Btw25000_29999),
    Btw30000_34999 = sum(Btw30000_34999),
    Btw35000_39999 = sum(Btw35000_39999),
    Btw40000_44999 = sum(Btw40000_44999),
    Btw45000_49999 = sum(Btw45000_49999),
    Btw50000_59999 = sum(Btw50000_59999),
    Btw60000_74999 = sum(Btw60000_74999),
    Btw75000_99999 = sum(Btw75000_99999),
    Btw100000_124999 = sum(Btw100000_124999),
    Btw125000_149999 = sum(Btw125000_149999),
    Above150000 = sum(Above150000),
    Btw150000_199999 = sum(Btw150000_199999),
    Above200000 = sum(Above200000)) 

#General tidying of Crosswalked data
Household_Inc_1999_Crosswalked <- Household_Inc_1999_Crosswalked %>%
  rename(GEOID = tr2020ge) %>%
  mutate(Year = YR1,
         GEOID = as.character(GEOID)) %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11))


#--------
# 2010 HOUSEHOLD income
Household_Inc_2010 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Income in 2010/nhgis0072_ds176_20105_blck_grp.csv")) %>%
  rename(Below10000 = "JOHE002",
         Btw10000_14999 = "JOHE003",
         Btw15000_19999 = "JOHE004",
         Btw20000_24999 = "JOHE005",
         Btw25000_29999 = "JOHE006",
         Btw30000_34999 = "JOHE007",
         Btw35000_39999 = "JOHE008",
         Btw40000_44999 = "JOHE009",
         Btw45000_49999 = "JOHE010",
         Btw50000_59999 = "JOHE011",
         Btw60000_74999 = "JOHE012",
         Btw75000_99999 = "JOHE013",
         Btw100000_124999 = "JOHE014",
         Btw125000_149999 = "JOHE015",
         Btw150000_199999 = "JOHE016",
         Above200000 = "JOHE017") %>%
  mutate(Above150000 = Btw150000_199999 + Above200000) %>%
  select(GISJOIN, Below10000, Btw10000_14999, Btw15000_19999, Btw20000_24999, Btw25000_29999, Btw30000_34999,
         Btw35000_39999, Btw40000_44999, Btw45000_49999, Btw50000_59999, Btw60000_74999, Btw75000_99999,
         Btw100000_124999, Btw125000_149999, Above150000, Btw150000_199999, Above200000)


#Load in 2010 block group to 2020 tract crosswalk, and create GISJOIN
CW_Tracts2010to2020 <- read.csv(paste0(onedrivepath, "Mapping Richmond/Richmond Data/Crosswalk1990to2020/nhgis_bg2010_tr2020_51.csv")) %>%
  mutate(GISJOIN = bg2010gj)

#Join crosswalks to census data
#Then multiply the data by the population weight
Household_Inc_2010_Crosswalked <- CW_Tracts2010to2020 %>%
  left_join(Household_Inc_2010, by="GISJOIN") %>%
  mutate(across(c(Below10000, Btw10000_14999, Btw15000_19999, Btw20000_24999, Btw25000_29999, 
                  Btw30000_34999,Btw35000_39999, Btw40000_44999, Btw45000_49999, Btw50000_59999, 
                  Btw60000_74999, Btw75000_99999, Btw100000_124999, Btw125000_149999, Above150000, 
                  Btw150000_199999, Above200000), ~ . * wt_hh)) %>%
  group_by(tr2020ge) %>%
  summarize(
    Below10000 = sum(Below10000),
    Btw10000_14999 = sum(Btw10000_14999),
    Btw15000_19999 = sum(Btw15000_19999),
    Btw20000_24999 = sum(Btw20000_24999),
    Btw25000_29999 = sum(Btw25000_29999),
    Btw30000_34999 = sum(Btw30000_34999),
    Btw35000_39999 = sum(Btw35000_39999),
    Btw40000_44999 = sum(Btw40000_44999),
    Btw45000_49999 = sum(Btw45000_49999),
    Btw50000_59999 = sum(Btw50000_59999),
    Btw60000_74999 = sum(Btw60000_74999),
    Btw75000_99999 = sum(Btw75000_99999),
    Btw100000_124999 = sum(Btw100000_124999),
    Btw125000_149999 = sum(Btw125000_149999),
    Above150000 = sum(Above150000),
    Btw150000_199999 = sum(Btw150000_199999),
    Above200000 = sum(Above200000)) 

#General tidying of Crosswalked data
Household_Inc_2010_Crosswalked <- Household_Inc_2010_Crosswalked %>%
  rename(GEOID = tr2020ge) %>%
  mutate(Year = YR3,
         GEOID = as.character(GEOID)) %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11))

#--------
##2020
#Household income
Household_Inc_2020 <- get_acs(
  geography = GEOG, 
  table = "B19001A", 
  state = ST,
  year = YR4,
  output = "wide") %>%
  rename(Below10000 = "B19001A_002E",
         Btw10000_14999 = "B19001A_003E",
         Btw15000_19999 = "B19001A_004E",
         Btw20000_24999 = "B19001A_005E",
         Btw25000_29999 = "B19001A_006E",
         Btw30000_34999 = "B19001A_007E",
         Btw35000_39999 = "B19001A_008E",
         Btw40000_44999 = "B19001A_009E",
         Btw45000_49999 = "B19001A_010E",
         Btw50000_59999 = "B19001A_011E",
         Btw60000_74999 = "B19001A_012E",
         Btw75000_99999 = "B19001A_013E",
         Btw100000_124999 = "B19001A_014E",
         Btw125000_149999 = "B19001A_015E",
         Btw150000_199999 = "B19001A_016E",
         Above200000 = "B19001A_017E") %>%
  mutate(Above150000 = Btw150000_199999 + Above200000) %>%
  select(GEOID, Below10000, Btw10000_14999, Btw15000_19999, Btw20000_24999, Btw25000_29999, Btw30000_34999,
         Btw35000_39999, Btw40000_44999, Btw45000_49999, Btw50000_59999, Btw60000_74999, Btw75000_99999,
         Btw100000_124999, Btw125000_149999, Above150000, Btw150000_199999, Above200000) %>%
  mutate(Year = YR4,
         GEOID = as.character(GEOID)) %>%
  mutate(STATE = str_sub(GEOID, 1, 2),
         COUNTY = str_sub(GEOID, 3, 5),
         TRACT = str_sub(GEOID, 6, 11))



