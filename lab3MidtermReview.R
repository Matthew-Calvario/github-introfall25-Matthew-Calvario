library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)
library(janitor)
library(scales)

housingdir <- "/Users/lynacalvario/Desktop/SYS3501/Data/"
sourcedir <- "/Users/lynacalvario/Desktop/SYS3501/"

stopifnot(dir.exists(housingdir))
setwd(housingdir)

housing <- read.csv("housing.csv", na.strings = c("", "NA", "N/A"," NA", "NA "), stringsAsFactors = FALSE)
# create a box plot for the price variable with points overlaid to show outliers
ggplot(housing, aes(x = '', y = price)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Box Plot of Housing Prices with Outliers",
       y = "Price") +
  theme_minimal()
# find total numbers of outliers (above and below the whiskers)
Q1 <- quantile(housing$price, 0.25, na.rm = TRUE)
Q3 <- quantile(housing$price, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers <- housing %>%
  filter(price < lower_bound | price > upper_bound)
num_outliers <- nrow(outliers)
num_outliers

# find value of the upper whisker
upper_whisker <- max(housing$price[housing$price <= upper_bound], na.rm = TRUE)
upper_whisker

# find the strongest linear relationship with price (between sqft, baths, City, and bedrooms) with a scatter plot matrix with coefficients and regression lines
housing_subset <- housing %>%
  select(price, sqft, baths, bedrooms) %>%
  drop_na()
pairs(housing_subset, panel = function(x, y) {
  points(x, y)
  abline(lm(y ~ x), col = "red")
  cor_coef <- cor(x, y)
  text(mean(x, na.rm = TRUE), mean(y, na.rm = TRUE), labels = round(cor_coef, 2), pos = 3)
})

# Create a box plot for bedrooms
ggplot(housing, aes(x = factor(bedrooms), y = price)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Box Plot of Housing Prices by Number of Bedrooms",
       x = "Number of Bedrooms",
       y = "Price") +
  theme_minimal()
# how many houses have 6 bedrooms?
num_six_bedrooms <- housing %>%
  filter(bedrooms == 6) %>%
  nrow()
num_six_bedrooms

# Which of the following statements are true about the house(s) with highest price in the data set? (Hint:  Use tidyverse and filter by City)
max_price <- max(housing$price, na.rm = TRUE)
most_expensive_houses <- housing %>%
  filter(price == max_price)
most_expensive_houses

# Total sum of price for homes in Oxnard
total_oxnard_price <- housing %>%
  filter(City == "Oxnard") %>%
  summarize(total_price = sum(price, na.rm = TRUE)) %>%
  pull(total_price)

total_oxnard_price

# Total price for all homes in data set, organized by City (filter by City)
total_price_by_city <- housing %>%
  group_by(City) %>%
  summarize(total_price = sum(price, na.rm = TRUE)) %>%
  arrange(desc(total_price))
total_price_by_city

# find the highest
highest_total_price_city <- total_price_by_city %>%
  filter(total_price == max(total_price, na.rm = TRUE))
highest_total_price_city

# number of levels for City Variable
num_city_levels <- n_distinct(housing$City)
num_city_levels


