# Directly estimate WTP using a "WTP Space" model


# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(fastDummies)
library(janitor)

options(dplyr.width = Inf) # So you can see all of the columns

# Load the data set:

pathToData <- here('data', "choiceData.csv")
data <- read_csv(pathToData)
head(data)


# Create dummy coded variables
data_dummy <- dummy_cols(
  data, c('number_of_burners', 'Cooktop_Type'))
head(data_dummy)

# Clean up names of created variables
data_dummy1 <- clean_names(data_dummy)


# Estimate the model
model_wtp_cooktop <- logitr(
  data    = data_dummy1,
  outcome = "choice",
  obsID   = "obs_id",
  pars    = c("average_annual_usage_cost",
              "number_of_burners_4", "number_of_burners_5",
              "cooktop_type_electric","cooktop_type_induction"),
  scalePar = 'final_cost_price', 
  numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
)

# View summary of results
summary(model_wtp_cooktop)

# Check the 1st order condition: Is the gradient at the solution zero?
model_wtp_cooktop$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_wtp_cooktop$hessian)$values

# Compare computed versus estimated WTP
load(here("models", "model_wtp.RData"))
wtpCompare(model, model_wtp_cooktop, scalePar = 'final_cost_price')

# Save model
save(
  model_wtp_cooktop,
  file = here("models", "model_wtp.RData")
)
# for comparing model, they are similar
# wtpcompare -> dividing market price and comparing it model wtp 
