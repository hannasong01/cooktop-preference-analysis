# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)

# Plot results

load(here("models", "model.RData"))
# -----------------------------------------------------------------------------
# Some tips for working with model objects

# If you want to get the resulting model parameters, use the coef() function
coef(model)

# If you want the standard errors, use se()
se(model)

# If you want to get the full summary table of the model coefficients 
# as a data frame, use coef(summary(model)) 
coef(summary(model))

# -----------------------------------------------------------------------------
# Plot results

# Get the estimated coefficients
coefs <- coef(model)

df_final_cost_price <- data.frame(level = c(500, 700, 900, 1100, 1300, 1500)) %>% 
  mutate(
    diff    = level - min(level),
    utility = diff*coefs['final_cost_price'])

df_average_annual_usage_cost <- data.frame(level = c(50, 60, 70, 80, 90, 100)) %>%
  mutate(
    diff  = level - min(level),
    utility = diff*coefs['average_annual_usage_cost'])


df_number_of_burners <- data.frame(level = c("3","4", "5")) %>%
  mutate(utility = c(0, coefs['number_of_burners_4'], coefs['number_of_burners_5']))

df_cooktop_type <- data.frame(level = c("Gas","Electric", "Induction")) %>%
  mutate(utility = c(0, coefs['cooktop_type_electric'],coefs['cooktop_type_induction']))


# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)

# Get upper and lower bounds (plots should have the same y-axis)
utility <- c(
  df_final_cost_price$utility, df_average_annual_usage_cost$utility, 
  df_number_of_burners$utility, df_cooktop_type$utility) 
ymin <- floor(min(utility))
ymax <- ceiling(max(utility))

# Plot the utility for each attribute

plot_final_cost_price <- df_final_cost_price %>% 
  ggplot() +
  geom_line(aes(x = level, y = utility)) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(x = 'Final Cost ($)', y = 'Utility') +
  theme_bw()

plot_average_annual_usage_cost <- df_average_annual_usage_cost %>% 
  ggplot() +
  geom_line(aes(x = level, y = utility)) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(x = 'Usage Cost ($)', y = 'Utility') +
  theme_bw()

plot_number_of_burners <- df_number_of_burners %>% 
  ggplot() +
  geom_point(aes(x = level, y = utility)) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(x = 'Burners', y = 'Utility') +
  theme_bw()

plot_cooktop_type <- df_cooktop_type %>% 
  ggplot() +
  geom_point(aes(x = level, y = utility)) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(x = 'Cooktop Type', y = 'Utility') +
  theme_bw()

# Plot all plots in one figure
plot_model_simple <- plot_grid(
  plot_final_cost_price, plot_average_annual_usage_cost, plot_number_of_burners, plot_cooktop_type,
  nrow = 1
)

# Save plots 
ggsave(
  filename = here('figs', 'model_simple_utility.png'), 
  plot = plot_model, 
  width = 10, height = 3.3
)
