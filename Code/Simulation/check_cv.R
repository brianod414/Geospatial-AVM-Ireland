library(here)
library(ggplot2)
library(cowplot)
here::i_am("./Code/check_cv.R")

cv_data <- readr::read_csv(here("Model Results/gam.irl2_knot_results.csv"))
cv_data


plot(cv_data$k, cv_data$rmse)
plot(cv_data$k, cv_data$rsq)

plot_rsq <- ggplot(cv_data, aes(x = k, y = rsq)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = cv_data$k) +
  labs(title = "R^2 vs k", x = "k values", y = "R^2 values") +
  theme_minimal()

plot_rmse <- ggplot(cv_data, aes(x = k, y = rmse)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = cv_data$k) +
  labs(title = "RMSE vs k", x = "k values", y = "RMSE values") +
  theme_minimal()

plot_grid(plot_rsq, plot_rmse)



