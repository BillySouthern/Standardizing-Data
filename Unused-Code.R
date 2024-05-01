#5/1/24, initiated by BS
#Goal: Script for all unused, rough, scrap code that was removed from testing elsewhere

#Rough and unused code

test <- Richmond %>%
  left_join(Household_Inc_2020, by="GEOID") %>%
  select(-Btw150000_199999, -Above200000) %>%
  pivot_longer(cols = matches("^B|^A"),
               names_to = "Inc_Threshold",
               values_to = "Count") %>%
  group_by(Inc_Threshold) %>%     
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
      Inc_Threshold == "Above150000" ~ 150000,
      TRUE ~ NA_real_
    ),
    upper_limit = case_when(
      Inc_Threshold == "Below10000" ~ 10000,
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
      Inc_Threshold == "Above150000" ~ NA,
      TRUE ~ NA_real_
    )) %>%
  # group_by(GEOID) %>% #
  # filter(!all(Count == 0)) %>% #Filter tracts with no incomes as this breaks the function (Rejoin after)
  # ungroup() %>%
  arrange( #Data must be in order for function
    lower_limit 
  ) 




ggplot() +
  geom_histogram(data = test, aes(x = Count), fill = "yellow", alpha = 0.5) +
  labs(x = "Income Threshold", y = "Count", title = "Bar Chart") +
  
  ggplot(data, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")

library(ggplot2)

# Generate x-values for Pareto
x_values_pareto <- seq(min(rsb$breaks), max(rsb$breaks), length.out = 100)
y_values_pareto <- rsb$rsubPDF(x_values_pareto)
df_pareto <- data.frame(x = x_values_pareto, y = y_values_pareto)

# Generate x-values for Stepbin
x_values_step <- seq(min(sb$breaks), max(sb$breaks), length.out = 100)
y_values_step <- sb$stepPDF(x_values_step)
df_step <- data.frame(x = x_values_step, y = y_values_step)

# Generate x-values for Spline
x_values_spline <- seq(0, 500000, length.out = 500)
y_values_spline <- splb$splinePDF(x_values_spline)
df_spline <- data.frame(Income = x_values_spline, Density = y_values_spline)

# Combine all dataframes
df <- rbind(df_pareto, df_step, df_spline)
df$type <- c(rep("Pareto", nrow(df_pareto)), rep("Stepbin", nrow(df_step)), rep("Spline", nrow(df_spline)))

# Plot using ggplot2
ggplot(df, aes(x = x, y = y, color = type)) +
  geom_line() +
  labs(x = "X-axis label", y = "Y-axis label", title = "Combined Plot")





#----------------
#Filter first tract
object <- Geography %>%
  filter(GEOID == "51087200131") 

#Create object and parameters
binedges <- object$upper_limit
bincounts <- object$Count

#Use Pareto command
rsb <- rsubbins(binedges, bincounts, tailShape = "pareto")

# Calculate mean of midpoint
Mean_Income <- integral(function(x) {1 - rsb$rsubCDF(x)}, 0, rsb$E)

# Calculate median of midpoint
Median_Income <- uniroot(function(x) rsb$rsubCDF(x) - 0.5, lower = 0, upper = rsb$E)$root

#To calculate general statistics (mean, variance, SD, Thiels, Gini)
stats <- stats_from_distribution(rsb)

# Extract statistics values
mean_val <- unname(stats["mean"])
variance_val <- unname(stats["variance"])
sd_val <- unname(stats["SD"])
theils_val <- unname(stats["Theil"])
gini_val <- unname(stats["Gini"])

# Create a data frame with the results
result <- data.frame(
  GEOID = "51007930101",
  mean_midpoint = mean_midpoint,
  median_midpoint = median_midpoint,
  mean = mean_val,
  variance = variance_val,
  sd = sd_val,
  theils = theils_val,
  gini = gini_val
)

# Create a data frame with the results
result <- data.frame(
  GEOID = "51007930101",
  Median_Income = median_midpoint,
  Mean_Income = mean_midpoint,
  Variance = variance_val,
  SD = sd_val,
  Theils = theils_val,
  Gini = gini_val
)

# View the resulting data frame
print(result)




# binedges <- object %>%
#   filter(GEOID == "51007930101") %>%
#   pull(upper_limit)
# 
# bincounts <- object %>%
#   filter(GEOID == "51007930101") %>%
#   pull(Count)

GEOID <- Geography %>%
  filter(GEOID == "51041100505") 

#---------
#MIDPOINT ESTIMATION (robust Pareto midpoint estimator)
#Create object and parameters
binedges <- object$upper_limit
bincounts <- object$Count

#Use Pareto command
rsb <- rsubbins(binedges, bincounts, tailShape = "pareto")

#Mean of midpoint
integral(function(x){1-rsb$rsubCDF(x)}, 0, rsb$E)

#Median of midpoint
uniroot(function(x) rsb$rsubCDF(x) - 0.5, lower = 0, upper = rsb$E)$root

#Visualize PDF and CDF distributions
plot(rsb$rsubPDF, do.points=FALSE)
plot(rsb$rsubCDF, 0, rsb$E)


#--------
#STEP BINS APPROACH
#Creating step bins
sb <- stepbins(binedges, bincounts)

#Visualize stepbin dsitribution
plot(sb$stepPDF)

#Mean of stepbins
integral(function(x){1-sb$stepCDF(x)}, 0, sb$E)

#Median of stepbins
uniroot(function(x) sb$stepCDF(x) - 0.5, lower = 0, upper = sb$E)$root


#--------
#SPLINEBINS APPROACH
#Creating spline bins
splb <- splinebins(binedges, bincounts)


#Plot spline dsitribution
plot(splb$splinePDF, 0, 300000, n=500)

#two mean commands
integral(function(x){1-splb$splineCDF(x)}, 0, splb$E) # should be the mean
integral(function(x){1-splb$splineCDF(x)}, 0, splb$E) # closer to given mean

#Median of spline
uniroot(function(x) splb$splineCDF(x) - 0.5, lower = 0, upper = splb$E)$root

#-------------
#GENERAL STATS
#To calculate general statistics (mean, variance, SD, Thiels, Gini)
stats_from_distribution(splb)

















#objecting median bin work
long_data_2020 <- Household_Inc_2020 %>%
  select(-Btw150000_199999, -Above200000) %>%
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
      Inc_Threshold == "Above150000" ~ 150000,
      TRUE ~ NA_real_
    ),
    upper_limit = case_when(
      Inc_Threshold == "Below10000" ~ 10000,
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
      Inc_Threshold == "Above150000" ~ NA,
      TRUE ~ NA_real_
    )) 
%>%
  select(lower_limit, upper_limit, GEOID, Count)

%>%
  group_by(GEOID) %>%
  mutate(cumulative_freq = cumsum(Count)) %>%
  filter(cumulative_freq >= sum(Count) / 2) %>%
  slice(1) %>%
  summarise(
    median = (lower_limit + upper_limit) / 2
  ) 

object <- Household_Inc_2020 %>%
  select(-Btw150000_199999, -Above200000) %>%
  pivot_longer(cols = starts_with(c("B","A")),
               names_to = "Inc_Threshold",
               values_to = "Count") %>%
  group_by(GEOID) %>%
  summarize(
    Tract_Count = sum(Count),  # Total number of observations
    Cum_Freq_Lower = sum(Count[Inc_Threshold %in% c("Below10000", "Btw10000_14999", 
                                                    "Btw15000_19999", "Btw20000_24999", 
                                                    "Btw25000_29999", "Btw30000_34999", 
                                                    "Btw35000_39999")]),  # Cumulative frequency up to the median group
    Med_Frequency = Count[Inc_Threshold == "Btw40000_44999"],  # Frequency of the median group
    Median_group_Width = 4999  # Width of the median group
  ) %>%
  mutate(
    Binned_Median = 40000 + ((Tract_Count / 2 - Cum_Freq_Lower) / Med_Frequency) * Median_group_Width  # Best estimate of the median
  ) %>%
  ungroup()




https://www.statology.org/histogram-mean-median/
  
  L + ( (n/2 – F) / f ) * w

L: The lower limit of the median group
n: The total number of observations
F: The cumulative frequency up to the median group
f: The frequency of the median group
w: The width of the median group

40000 + (())




binned_income_data <- long_data %>%
  group_by(GEOID) %>%
  arrange(Inc_Threshold) %>%
  mutate(cumulative_count = cumsum(Count))

median_income_by_tract <- binned_income_data %>%
  group_by(GEOID) %>%
  filter(cumulative_count >= sum(Count) / 2) %>%
  slice(1) %>%
  left_join(Med_inc_2020, by = "GEOID")