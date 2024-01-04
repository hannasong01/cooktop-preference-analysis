library(cbcTools)
library(fastDummies)
library(logitr)
library(ggplot2)
library(here)

# Define profiles with attributes and levels
profiles <- cbc_profiles(
  price       = c(500, 700, 900, 1100, 1300, 1500),
  # Price ($1,00)
  average_annual_usage_cost = c(50, 60, 70, 80, 90, 100),
  # Price(10)
  type  = c("Gas", "Electric", "Induction"),
  burners = c(3 , 4 , 5)
)



# Random Design

design_rand <- cbc_design(
  profiles = profiles,
  n_resp   = 500,
  # Number of respondents
  n_alts   = 3,
  # Number of alternatives per question
  n_q      = 8    # Number of questions per respondent
)

head(design_rand)

cbc_balance(design_rand)
cbc_overlap(design_rand)

# Simulate random choices

data_rand <- cbc_choices(
  design = design_rand,
  obsID = "obsID"
)
head(data_rand)

# Run power analysis

power_rand <- cbc_power(
  nbreaks = 20,
  n_q     = 8,
  n_core  = 12,
  data    = data_rand,
  pars    = c("price", "average_annual_usage_cost", "type", "burners"),
  outcome = "choice",
  obsID   = "obsID"
)

head(power_rand)
tail(power_rand)

# Plot
power_analysis <- ggplot(power_rand) +
  geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
  geom_point(aes(x = sampleSize, y = se, color = coef)) +
  expand_limits(y = 0) +
  scale_y_continuous(limits = c(0, 0.125)) +
  theme_bw() + 
  labs(
    x = "Sample size", 
    y = "Standard error", 
    color = "Coefficient"
  )

ggsave(
  filename = here('figs', 'power_analysis.png'), 
  plot = power_analysis, 
  width = 5, height = 3
)


