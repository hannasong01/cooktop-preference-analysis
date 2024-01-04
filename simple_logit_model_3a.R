# Estimate multinomial logit (MNL) models

# Load libraries

library(logitr)
library(tidyverse)
library(fastDummies)
library(janitor)
library(here)
options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:

pathToData <- here('data', "choiceData.csv")
data <- read_csv(pathToData)
head(data)

# Estimate MNL model

# Create dummy coded variables
data_dummy <- dummy_cols(
  data, c('number_of_burners', 'Cooktop_Type'))
head(data_dummy)

# Clean up names of created variables
data_dummy1 <- clean_names(data_dummy)

# Estimate the model
model <- logitr(
  data    = data_dummy1,
  outcome = "choice",
  obsID   = "obs_id",
  pars    = c(
    "final_cost_price","average_annual_usage_cost",
    "number_of_burners_4", "number_of_burners_5",
    "cooktop_type_electric","cooktop_type_induction" )
)


# View summary of results
summary(model)

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values



# -----------------------------------------------------------------------------
# Save model objects 

save(
  model,
  file = here("models", "model.RData")
)

