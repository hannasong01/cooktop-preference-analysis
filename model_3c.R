#remotes::install_github("jhelvy/jph")

1# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)
library(fastDummies)
library(lubridate)
library(dplyr)
library(jph)
library(janitor)

# 3c : At least one sub-group analysis where you compare a logit model on 
# different sub-groups in your sample


load(here("models", "model.RData"))

pathToData <- here('data', "choiceData.csv")
data <- read_csv(pathToData)
head(data)

# Creating two groups based on education ---------

nrow(data)


data$education_level <- 0
data$education_level [data$education == 'degree_bs'| data$education == 'degree_grad'] <-'high'
data$education_level [data$education == 'vocational'| data$education == 'college_some'|data$education == 'hs'|
                        data$education == 'degree_associate'|data$education == 'prefer_not_say'] <-'low'


# Create dummy coded variables
data_dummy <- dummy_cols(
  data, c('number_of_burners', 'Cooktop_Type', 'education_level'))

head(data_dummy)

# Clean up names of created variables
data_dummy1 <- clean_names(data_dummy)


# "average_annual_usage_cost", "number_of_burners_4", "number_of_burners_5",
#"cooktop_type_electric","cooktop_type_induction" )

# Create interactions of each variable with group_B
data_dummy1 <- data_dummy1 %>%
  mutate(
    final_cost_price_low = final_cost_price*education_level_low,
    average_annual_usage_cost_low = average_annual_usage_cost*education_level_low,
    number_of_burners_4_low   = number_of_burners_4*education_level_low,
    number_of_burners_5_low = number_of_burners_5*education_level_low,
    cooktop_type_electric_low = cooktop_type_electric*education_level_low,
    cooktop_type_induction_low = cooktop_type_induction*education_level_low
  )
head(data_dummy1)

--------------------------------------
# Estimate the model

multinomial_groups_edu <- logitr(
  data    = data_dummy1,
  outcome = "choice",
  obsID   = "obs_id",
  pars = c(
    "final_cost_price","average_annual_usage_cost","number_of_burners_4", "number_of_burners_5",
    "cooktop_type_electric","cooktop_type_induction",
    'final_cost_price_low', 'average_annual_usage_cost_low', 'number_of_burners_4_low', 'number_of_burners_5_low',
    'cooktop_type_electric_low','cooktop_type_induction_low')
  )


# View summary of results
summary(mnl_groups)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_groups$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_groups$hessian)$values

# Save model objects
save(
  mnl_groups,
  file = here("models", "mnl_groups.RData")
)



# -----------------------------------------------------------------------------


# education




# income


