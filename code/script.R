# instructions ----

install.packages("renv")
# Initialize renv for the project
library(renv)
renv::init()
###say yes

# Install required packages
renv::install(c("ggplot2", "dplyr", "tidyr"))

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Create a dataset with additional variables
set.seed(456)
data <- data.frame(
  x = rnorm(100),
  category = sample(c("A", "B", "C"), 100, replace = TRUE)
) %>% mutate(y = 2 * x + rnorm(100))

# Perform data manipulation using tidyverse
data_summary <- data %>%
  group_by(category) %>%
  summarise(
    mean_x = mean(x),
    sd_y = sd(y),
    n = n()
  )

# Print the summary table
print(data_summary)