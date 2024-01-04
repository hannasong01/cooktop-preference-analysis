# Visualize results of estimated WTP space model

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)

# -----------------------------------------------------------------------------
# Get WTP estimates with 95% CI



# Method 2: Estimate WTP in WTP space model:
load(here("models", "mxl.RData")) # Load estimated models
coefs <- coef(mxl_wtp)
covariance <- vcov(mxl_wtp)
wtp_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
wtp_ci2 <- ci(wtp_draws)
wtp_ci2 <- wtp_ci2[-1,] # Drop lambda (we won't plot this)
wtp_ci2

# -----------------------------------------------------------------------------
# Plot results

wtp_ci <- wtp_ci2

# Separate coefficient CIs by attribute 
wtp_ci$par <- row.names(wtp_ci)
wtp_average_annual_usage_cost <- wtp_ci %>% filter(par == 'average_annual_usage_cost')
wtp_number_of_burners_4 <- wtp_ci %>% filter(par == 'number_of_burners_4')
wtp_number_of_burners_5 <- wtp_ci %>% filter(par == 'number_of_burners_5')
wtp_cooktop_type_electric <- wtp_ci %>% filter(par == 'cooktop_type_electric')
wtp_cooktop_type_induction <- wtp_ci %>% filter(par == 'cooktop_type_induction')


# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)

df_average_annual_usage_cost <- data.frame(level = c(50, 60, 70, 80, 90, 100)) %>%
    mutate(
        diff  = level - min(level),
        mean  = diff*wtp_average_annual_usage_cost$mean,
        lower = diff*wtp_average_annual_usage_cost$lower,
        upper = diff*wtp_average_annual_usage_cost$upper)

df_number_of_burners <- data.frame(level = c("3","4", "5")) %>%
    mutate(
        mean  = c(0, wtp_number_of_burners_4$mean, wtp_number_of_burners_5$mean),
        lower = c(0, wtp_number_of_burners_4$lower, wtp_number_of_burners_5$lower),
        upper = c(0, wtp_number_of_burners_4$upper, wtp_number_of_burners_5$upper))

df_cooktop_type <- data.frame(level = c("Gas","Electric", "Induction")) %>%
    mutate(
        mean  = c(0, wtp_cooktop_type_electric$mean, wtp_cooktop_type_induction$mean),
        lower = c(0, wtp_cooktop_type_electric$lower, wtp_cooktop_type_induction$lower),
        upper = c(0, wtp_cooktop_type_electric$upper, wtp_cooktop_type_induction$upper))

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(c(
    df_average_annual_usage_cost$lower, df_number_of_burners$lower, df_cooktop_type$lower)))
ymax <- ceiling(max(c(
    df_average_annual_usage_cost$upper, df_number_of_burners$upper, df_cooktop_type$upper)))



# Plot the WTP for each attribute *with 95% CI*
plot_average_annual_usage_cost <- df_average_annual_usage_cost %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) +
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Annual Usage Cost (usd)', y = 'WTP ($)') +
    theme_bw()

plot_number_of_burners <- df_number_of_burners %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Number of Burners (#)', y = 'WTP ($)') +
    theme_bw()

plot_cooktop_type <- df_cooktop_type %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Cooktop Type', y = 'WTP ($)') +
    theme_bw()

# Plot all plots in one figure
plot_mxl_wtp <- plot_grid(
    plot_average_annual_usage_cost, plot_number_of_burners, plot_cooktop_type,
    nrow = 1
)

# Save plots
ggsave(
    filename = here('figs', 'plot_mxl_wtp.png'),
    plot = plot_mxl_wtp,
    width = 8, height = 2.3
)

# -----------------------------------------------------------------------------
