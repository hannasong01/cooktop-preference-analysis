# Plot sensitivity simulation results

# Load libraries & functions
library(tidyverse)
library(here)

# Load simulation results
load(here("sims", "sens_price_model.RData"))

# -----------------------------------------------------------------------------
# Make a line plot of the market sensitivity to price (with uncertainty)

share_price_plot <- 
    sens_price %>% 
    ggplot(aes(
        x = final_cost_price, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_price %>% filter(final_cost_price <=1500 , final_cost_price >= 500), 
        linetype = "solid") +
    expand_limits(x = c(300, 2000), y = c(0, 1)) +
    labs(x = 'Final Cost Price ($)', y = 'Market Share') +
    theme_bw()

share_price_plot

# Save plot
ggsave(
    filename = here('figs', 'share_price_plot.png'), 
    plot = share_price_plot,
    width = 5, height = 4
)

