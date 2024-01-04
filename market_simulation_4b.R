# Compute expected probabilities of different alternatives

# Load libraries & functions
library(tidyverse)
library(here)
library(logitr)

# Load estimated models
load(here("models", "cooktops.RData"))

# -----------------------------------------------------------------------------
# Single market simulation using the linear model

summary(model)

# Create a set of alternatives for which to simulate shares
baseline <- data.frame(
    altID       = c(1, 2, 3), 
    obsID       = c(1, 1, 1),
    final_cost_price       = c(500, 800, 1300),
    average_annual_usage_cost = c(100,50,50),
    cooktop_type_electric = c(0, 1, 0),
    cooktop_type_induction  = c(0, 0, 1),
    number_of_burners_4 = c(1,1,1),
    number_of_burners_5 = c(0,0,0)
    
)
coef(model)

# Columns are attributes, rows are alternatives
baseline

#write_csv(baseline, here('data', 'scenarios.csv'))

# Use the predict() function to compute the probabilities
sim_cooktop_linear <- predict(
    model,
    newdata = baseline, 
    obsID = 'obsID', 
    level = 0.95, 
    interval = "confidence",
    returnData = TRUE # This returns your data along with predicted values
)

sim_cooktop_linear

# -----------------------------------------------------------------------------
# Multiple simulations using the linear model

# Read in market scenarios
scenarios <- read_csv(here('data', 'scenarios.csv'))
head(scenarios)

# Use the predict() function to compute the probabilities
sim_cooktop_linear_multi <- predict(
    model,
    newdata = scenarios,
    obsID = 'obsID',
    level = 0.95, 
    interval = "confidence",
    returnData = TRUE
)

head(sim_cooktop_linear_multi)



# Save simulations
save(
    sim_cooktop_linear,
    sim_cooktop_linear_multi,
    file = here("sims", "cooktops_linear.RData")
)

##########################################################################

# Plot market simulation results

# Load libraries & functions
#library(tidyverse)
#library(here)

# Load simulation results
load(here("sims", "cooktops_linear.RData"))

# Bar plot of probabilities for single market simulation (with 95% CI) 
sim_cooktop_linear %>% 
    mutate(label = c("Gas", "Electric", "Induction")) %>% 
    ggplot(aes(
        x = label, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Alternative', y = 'Market Share') +
    theme_bw()

# Save plot
ggsave(
    filename = here('figs', 'sim.png'), 
    width = 4, height = 3.5
)





# Bar plot of probabilities for multiple market simulations (with 95% CI) 
sim_cooktop_linear_multi %>%
  mutate(label = c("G $500", "E $800", "I $1300",
                   "G $500", "E $800", "I $800",
                   "G $600", "E $600", "I $600"),
         label2 = ifelse(obsID ==1 , "Baseline", 
                         ifelse(obsID==2, "Induction = Electric", "All Same")
         )
  ) %>%
  ggplot(aes(
    x = label, y = predicted_prob, 
    ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
  geom_col(fill = "grey", width = 0.6) +
  geom_errorbar(width = 0.3) +
  #facet_wrap(~obsID) +
  facet_grid(~label2, scales = "free_x")+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = 'Alternative', y = 'Market Share') +
  theme_bw()

# Save plot
ggsave(
  filename = here('figs', 'sim_multi.png'), 
  width = 8, height = 7
)


