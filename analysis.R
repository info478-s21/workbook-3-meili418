# Analysis

# Set up - make sure to set your working directory using RStudio
library(tidyr)
library(dplyr)
library(ggplot2)

# Create the `charts/` directory (you can do this from R!)
dir.create("charts", showWarnings = FALSE)

# Load prepped data
health_data <- read.csv("./data/prepped/all_data.csv")

# Are HALE and life expectancy correlated?
# - Plot 2016 life expectancy against 2016 HALE. Save the graph to `charts/`
# - Compute the correlation between 2016 life expectancy against 2016 HALE
data_2016 <- health_data %>%
  filter(year == 2016)

ggplot(data_2016) +
  geom_point(mapping = aes(x = le, y = hale)) +
  labs(
    title = "Life Expectancy vs. HALE",
    x = "Life Expectancy",
    y = "HALE"
  )
ggsave("charts/le_hale_graph.png")

cor(data_2016$hale, data_2016$le)
# The correlation between 2016 life expectancy against 2016 HALE is 0.9957015.
# As the correlation is almost 1, this implies a strong correlation between
# life expectancy and HALE, meaning if life expectancy increases,
# HALE will also increase at a similar rate.


# Are HALE and DALYs correlated?
# - Plot 2016 HALE against 2016 DALYs. Save the graph to `charts/`
# - Compute the correlation between 2016 HALE and DALYs
ggplot(data_2016) +
  geom_point(mapping = aes(x = dalys, y = hale)) +
  labs(
    title = "DALYs vs. HALE",
    x = "DALY",
    y = "HALE"
  )
ggsave("charts/daly_hale_graph.png")

cor(data_2016$hale, data_2016$daly)
# The correlation between 2016 DALYs against 2016 HALE is -0.9859484.
# This implies a strong negative correlation between HALE and DALYs,
# meaning a strong connection between the two variables, but that one
# goes up whenever the other one goes down.
# For example, if DALYs increase, HALE will decrease at a similar rate.


# As people live longer, do they live healthier lives
# (i.e., is a smaller fraction of life spent in poor health)?
# Follow the steps below to attempt to answer this question.

# First, you will need to reshape the data to create columns *by metric-year*
# This will create `hale_2016`, `hale_1990`, `le_2016`, etc.
# To do this, I suggest that you use the `pivot` function in the new
# tidyverse release:https://tidyr.tidyverse.org/articles/pivot.html#wider

# Create columns to store the change in life expectancy, and change in hale
data_wider <- health_data %>%
  pivot_wider(
    names_from = year,
    values_from = c(hale, le, dalys)
  )

data_wider <- data_wider %>%
  mutate(
    hale_diff = hale_2016 - hale_1990,
    le_diff = le_2016 - le_1990
  )

# Plot the *change in hale* against the *change in life expectancy*
# Add a 45 degree line (i.e., where x = y), and save the graph to `charts/`
# What does this mean?!?! Put your interpretation below
ggplot(data_wider) +
  geom_point(mapping = aes(x = le_diff, y = hale_diff)) +
  labs(
    title = "Life Expectancy difference vs. HALE difference",
    x = "Change in Life Expectancy",
    y = "Change in HALE"
  ) +
  geom_abline(intercept = 0, slope = 1) +
  xlim(-15, 20) +
  ylim(-15, 20)
ggsave("charts/change_plot.png")

# The chart shows that as the change in life expectancy increases,
# the change in HALE also increases. Since the linear line has a slope of 1
# and all the data points fall close to the line, this implies that the change
# in life expectancy and the change in HALE are strongly correlated.
# This means as people live longer, they do live healthier lives but at a
# slower rate. HALE increases at a lower rate than life expectancy.
