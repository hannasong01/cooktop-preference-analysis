# Measure the sensitivity of the market simulation outcome for one product to
# changes in attribute levels
install.packages("jph")

# Load libraries & functions
library(tidyverse)
library(here)
library(logitr)
library(jph)

# Load estimated models
load(here("models", "model.RData")) # Load pref space model

# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in *price*

# Create a set of alternatives for which to simulate shares
baseline <- data.frame(
  altID       = c(1, 2, 3), 
  obsID       = c(1, 1, 1),
  final_cost_price      = c(500, 800, 1300),
  average_annual_usage_cost = c(100,50,50),
  cooktop_type_electric = c(0, 1, 0),
  cooktop_type_induction  = c(0, 0, 1),
  number_of_burners_4 = c(1,1,1),
  number_of_burners_5 = c(0,0,0)
)

baseline 

# Define the sensitivity cases
# For this case, let's see how the market share for the Electric and Induction Cooktop 
# (option 2) changes with different EV prices. That is, I'm holding everything
# the same in every simulation except the price for the cooktop

final_cost_prices <- seq(300, 2000) # Define sensitivity price levels
n <- length(final_cost_prices) # Number of simulations (21)
scenarios_price <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_price$obsID <- rep(seq(n), each = 3) # Reset obsIDs

# Set the price for each scenario
scenarios_price$final_cost_price[which(scenarios_price$altID == 2)] <- final_cost_prices 
head(scenarios_price)

# For each case, simulate the market share predictions
sens_price <- predict(
  model,
  newdata = scenarios_price, 
  obsID = 'obsID', 
  level= 0.95,
  interval = 'confidence',
  returnData = TRUE) %>%
  # Keep only EV alternative
  filter(altID == 2) %>% 
  # Keep only prices and predictions
  select(final_cost_price, starts_with("predicted_")) 

sens_price
# The probability shifts from essentially 100% of the market share at 
# a price of $10,000 to 0% at $30,000

# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in multiple attributes

# For these cases, we'll look at how the market share for the Electric Vehicle 
# (option 2) changes with +/- 20% changes price, fuel economy, & acceleration time

# "high" means they result in higher market shares
# "low"  means they result in lower market shares
cases <- tribble(
  ~obsID, ~altID, ~attribute,              ~case,  ~value,
  2,      2,     'final_cost_price',       'high',  500*1.2,
  3,      2,     'final_cost_price',       'low',   500*0.8,
  4,      2,     'average_annual_usage_cost', 'high',  100*1.2,
  5,      2,     'average_annual_usage_cost', 'low',   100*0.8
)

cases

# Define scenarios
n <- 7 # baseline + high & low for each attribute
scenarios_atts <- rep_df(baseline, n) 
scenarios_atts$obsID <- rep(seq(n), each = 3) # Reset obsIDs

# Replace scenarios with case values 
scenarios_atts <- scenarios_atts %>% 
  left_join(cases, by = c("altID", "obsID")) %>% 
  mutate(
    attribute = ifelse(is.na(attribute), "other", attribute),
    case = ifelse(is.na(case), "base", case),
    final_cost_price = ifelse(attribute == 'final_cost_price', value, final_cost_price),
    average_annual_usage_cost = ifelse(attribute == 'average_annual_usage_cost', value, average_annual_usage_cost),
  )

scenarios_atts

# For each case, simulate the market share predictions
sens_atts <- predict(
  model,
  newdata = scenarios_atts, 
  obsID = 'obsID', 
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE) %>%
  # Keep only EV alternative
  filter(altID == 2) %>% 
  # Keep only attributes and predictions
  select(attribute, case, value, predicted_prob)

sens_atts

# -----------------------------------------------------------------------------
# Save simulations

save(
  sens_price,
  sens_atts,
  file = here("sims", "sens_price_model.RData")
)
