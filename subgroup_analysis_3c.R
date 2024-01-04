# Estimate multinomial logit (MNL) models for two groups in the data

#install.packages("jph")

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(here)
library(jph)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
#data <- read_csv(here('data', 'mnl_2groups.csv'))
#head(data)

pathToData <- here('data', "cooktop_choiceData.csv")
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
# "group"       = Indicates the respondent group ("A" or "B")

# -----------------------------------------------------------------------------
# Estimate MNL model with final cost price, average annual usage cost

# Creating two groups based on education ---------

nrow(data)


data$education_level <- 0
data$education_level [data$education == 'degree_bs'| data$education == 'degree_grad'] <-'high'
data$education_level [data$education == 'vocational'| data$education == 'college_some'|data$education == 'hs'|
                          data$education == 'degree_associate'|data$education == 'prefer_not_say'] <-'low'


# Create dummy coded variables
data_dummy <- dummy_cols(data, c('number_of_burners','education_level'))
head(data_dummy)

# Create dummy coefficients for the group and powertrain variables
#data_dummy <- fastDummies::dummy_cols(data, c('powertrain', 'group'))
#head(data_dummy)

# Create interactions of each variable with group_B
data_dummy <- data_dummy %>%
    mutate(
        final_cost_price_low = final_cost_price*education_level_low,
        Average_annual_usage_Cost_low = Average_annual_usage_Cost*education_level_low,
        number_of_burners_4_low   = number_of_burners_4*education_level_low,
        number_of_burners_5_low = number_of_burners_5*education_level_low
    )
head(data_dummy)


# Estimate the model
cooktop_mnl_groups <- logitr(
    data    = data_dummy,
    outcome = "choice",
    obsID   = "obsID",
    pars = c(
        'final_cost_price', 'Average_annual_usage_Cost',"number_of_burners_4", "number_of_burners_5",
        # Introduce group interactions with all main effects
        'final_cost_price_low', 'Average_annual_usage_Cost_low','number_of_burners_4_low', 'number_of_burners_5_low'
    )
)

# View summary of results
summary(cooktop_mnl_groups)

# Check the 1st order condition: Is the gradient at the solution zero?
cooktop_mnl_groups$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(cooktop_mnl_groups$hessian)$values

# Save model objects
save(
    cooktop_mnl_groups,
    file = here("models", "cooktop_mnl_groups.RData")
)

# -----------------------------------------------------------------------------
# Generate draws of the model coefficients for each group

# Get the model coefficients and covariance matrix
coefs <- coef(cooktop_mnl_groups)
covariance <- vcov(cooktop_mnl_groups)

# Take 10,000 draws of the coefficients
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
coef_draws_A <- coef_draws %>%
    select(final_cost_price, Average_annual_usage_Cost,number_of_burners_4, number_of_burners_5)
coef_draws_B <- coef_draws %>%
    mutate(
        final_cost_price= final_cost_price + final_cost_price_low,
        Average_annual_usage_Cost = Average_annual_usage_Cost + Average_annual_usage_Cost_low,
        number_of_burners_4 = number_of_burners_4 + number_of_burners_4_low,
        number_of_burners_5 = number_of_burners_5 + number_of_burners_5_low) %>%
    select(final_cost_price, Average_annual_usage_Cost, number_of_burners_4, number_of_burners_5)

# -----------------------------------------------------------------------------
# Compute WTP for each group

wtp_A <- coef_draws_A / (-1* coef_draws_A$final_cost_price)
wtp_B <- coef_draws_B / (-1* coef_draws_B$final_cost_price)
ci(wtp_A)
ci(wtp_B)

# -----------------------------------------------------------------------------
# Compute the market shares of a given market for each group
wtp_edu_high <- ci(wtp_A)
wtp_edu_low <- ci(wtp_B)

df_compare_h <- wtp_edu_high
df_compare_h <- df_compare_h[-1,]
cols <- c('mean', 'lower', 'upper')
df_compare_h[2, cols] <- df_compare_h[2, cols]/10 # number of burner 4
df_compare_h[3, cols] <- df_compare_h[3, cols]/10 # number of burner 4
df_compare_h$label <- c(
    "Average Annual Usage Cost", "number of burners 4(divided by 10)", "number of burners 5(divided by 10)"
)

barplot_mnl_wtp_h <- df_compare_h %>% 
    ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper)) +
    geom_col(width = 0.5, fill = 'gray') +
    geom_errorbar(width = 0.3) +
    labs(x = 'WTP ($)', y = 'Attribute') +
    theme_bw()

# Save plots
ggsave(
  filename = here('figs', 'barplot_mnl_wtp_high.png'),
  plot = barplot_mnl_wtp_h,
  width = 8, height = 2.3
)

# low 

df_compare_l <- wtp_edu_low
df_compare_l <- df_compare_l[-1,]
cols <- c('mean', 'lower', 'upper')
df_compare_l[2, cols] <- df_compare_l[2, cols]/10 # number of burner 4
df_compare_l[3, cols] <- df_compare_l[3, cols]/10 # number of burner 4
df_compare_l$label <- c(
  "Average Annual Usage Cost", "number of burners 4(divided by 10)", "number of burners 5(divided by 10)"
)


barplot_mnl_wtp_l <- df_compare_l %>% 
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper)) +
  geom_col(width = 0.5, fill = 'gray') +
  geom_errorbar(width = 0.3) +
  labs(x = 'WTP ($)', y = 'Attribute') +
  theme_bw()

# Save plots
ggsave(
  filename = here('figs', 'barplot_mnl_wtp_low.png'),
  plot = barplot_mnl_wtp_l,
  width = 8, height = 2.3
)
