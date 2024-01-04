# Estimate mixed logit (MXL) models

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(fastDummies)
library(janitor)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
pathToData <- here('data', "choiceData.csv")
data <- read_csv(pathToData)
head(data)


# Variables:
# "respID"      = Identifies each survey respondent
# "qID"         = Identifies each question for each survey respondent
# "altID"       = Identifies the alternative in each unique choice observation
# "obsID"       = Identifies each unique choice observation
# "choice"      = 1 if the alternative is chosen, 0 otherwise
# "price"       = Purchase price in thousands of dollars (15, 20, 25)
# "fuelEconomy" = Fuel economy in miles per gallon of gasoline (20, 25, 30)
# "accelTime"   = 0 to 60 mph acceleration time in seconds (6, 7, 8)
# "powertrain"  = Indicates if the car is electric or gas

# -----------------------------------------------------------------------------
# Estimate preference space MXL model with linear price, fuelEconomy, and accelTime



# Create dummy coded variables
data_dummy <- dummy_cols(
  data, c('number_of_burners', 'Cooktop_Type'))
head(data_dummy)

# Clean up names of created variables
data_dummy1 <- clean_names(data_dummy)


# Estimate the model
mxl_pref <- logitr(
  data    = data_dummy1,
  outcome = "choice",
  obsID   = "obs_id",
  pars    = c("final_cost_price","average_annual_usage_cost", "number_of_burners_4", "number_of_burners_5",
              "cooktop_type_electric","cooktop_type_induction"),
  randPars = c(final_cost_price = 'n', average_annual_usage_cost = 'n', number_of_burners_4 = 'n',
               cooktop_type_induction= 'n')
)

# View summary of results
summary(mxl_pref)

#The coeff estimates for final cost and average annual cost are negative and the *** indicate they
#statistically significant. This indicates that the preference is inversely proportional to these attributes
#i.e if final cost or annual cost increases the the preference decreases. The standard deviation in these
#attributes are not statistically significant which means the heterogeneity in preferences is not
#statistically significant.


# Check the 1st order condition: Is the gradient at the solution zero?
mxl_pref$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mxl_pref$hessian)$values

# -----------------------------------------------------------------------------
# Estimate WTP space MXL model with linear price, fuelEconomy, and accelTime

# Estimate the model
mxl_wtp <- logitr(
  data    = data_dummy1,
  outcome = "choice",
  obsID   = "obs_id",
  pars    = c("average_annual_usage_cost", "number_of_burners_4", "number_of_burners_5",
              "cooktop_type_electric","cooktop_type_induction"),
  scalePar = 'final_cost_price',
  randPars = c(average_annual_usage_cost = 'n', number_of_burners_4 = 'n',number_of_burners_5 ='n',
               cooktop_type_induction= 'n', cooktop_type_electric ='n')
)

# View summary of results
summary(mxl_wtp)

# Check the 1st order condition: Is the gradient at the solution zero?
mxl_wtp$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mxl_wtp$hessian)$values

# -----------------------------------------------------------------------------
# Save model objects 

save(
  mxl_pref,
  mxl_wtp,
  file = here("models", "mxl.RData")
)
