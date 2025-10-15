#You work for a real estate company and have been asked to price homes in a city. The company has provided retrospective data for home sales in the area. For the analysis in this exam, these data are below: 

# Import necessary libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)



getwd()

#now let's set our directory where our data is stored manually:
setwd("/Users/lynacalvario/Desktop/SYS3501/Data/")
#read in the data
housing_prices <- read_csv("housing-prices.csv")
summary(housing_prices)
# What is the row index of the house with the lowest price?

which.min(housing_prices$Price)
# What is the square footage of the most expensive house?
housing_prices %>% filter(Price == max(Price)) %>% select(Size)

#Use a box plot to determine whether the "Size" variable has any outliers. If so, how many does it have?

boxplot(housing_prices$Size, main="Boxplot of House Sizes", ylab="Size (sq ft)")
# find the number of outliers in this box plot
outliers <- boxplot.stats(housing_prices$Size)$out
length(outliers)
# create a ggplot of the same thing and identify the outliers using y-max final to verify
ggplot(housing_prices, aes(y = Size)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  labs(title = "Boxplot of House Sizes", y = "Size (sq ft)")
# What is the maximum value of the outliers in Size?

max(outliers)

#Create a new data frame with the outliers from the Size variable, then answer the following two questions.
# What is the mean of Size of these outlier houses (rounded to the nearest integer)?
outlier_houses <- housing_prices %>% filter(Size %in% outliers)
mean(outlier_houses$Size) %>% round()

#what is the maximum price of the outlier houses?
max(outlier_houses$Price)

# Are all of these houses new?
all(outlier_houses$New == TRUE)

# is the  average price of the houses identified as outliers in the Size variable is around $545,000?
mean(outlier_houses$Price) %>% round()

# Most of the houses with outlier sizes have 3 to 5 bedrooms (as indicated by the Rooms Variable)?
table(outlier_houses$Rooms)

# make me a data frame of these outliers
outlier_houses

# create a boxplot for baths
boxplot(housing_prices$Baths, main="Boxplot of House Baths", ylab="Number of Baths")
# tell me the stats on the box
boxplot.stats(housing_prices$Baths)
# create a ggplot of the same thing
ggplot(housing_prices, aes(y = Baths)) +
  geom_boxplot(outlier.colour = "blue", outlier.size = 2) +
  labs(title = "Boxplot of House Baths", y = "Number of Baths")
# what is the median, lower quartile, and upper quartile
median(housing_prices$Baths)
quantile(housing_prices$Baths, 0.25)
quantile(housing_prices$Baths, 0.75)

# lower whisker?
boxplot.stats(housing_prices$Baths)$stats[1]
# upper whisker?
boxplot.stats(housing_prices$Baths)$stats[5]

# create a scatter plot matrix of size compared to all the other variables
pairs(housing_prices)
# create a scatter plot of size vs price
ggplot(housing_prices, aes(x = Price, y = Size)) +
  geom_point() +
  labs(title = "Scatter Plot of Size vs Price", x = "Size (sq ft)", y = "Price ($)") +
  geom_smooth(method = "lm", col = "red")
# create a scatter plot of baths vs price
ggplot(housing_prices, aes(x = Baths, y = Size)) +
  geom_point() +
  labs(title = "Scatter Plot of Size vs Price", x = "Number of Baths", y = "Price ($)") +
  geom_smooth(method = "lm", col = "blue")
# create a scatter plot of rooms vs price
ggplot(housing_prices, aes(x = Rooms, y = Size)) +
  geom_point() +
  labs(title = "Scatter Plot of Rooms vs Price", x = "Number of Rooms", y = "Price ($)") +
  geom_smooth(method = "lm", col = "green")
# give me the ocrrelation coefficients of these
cor(housing_prices$Size, housing_prices$Price)
cor(housing_prices$Size, housing_prices$Baths)
cor(housing_prices$Size, housing_prices$Rooms)

#make me a histogram of size
ggplot(housing_prices, aes(x = Size)) +
  geom_histogram(binwidth = 100, fill = "lightblue", color = "black") +
  labs(title = "Histogram of House Sizes", x = "Size (sq ft)", y = "Frequency")
# Give me the price of all the old houses
old_houses <- housing_prices %>% filter(Age != 'New')
old_houses
# add up the price column
sum(old_houses$Price)

# Make a frequency heat map of Baths and Rooms, use as.factor to make room and baths categorical
ggplot(housing_prices, aes(x = as.factor(Baths), y = as.factor(Rooms))) +
  geom_bin2d() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Baths vs Rooms", x = "Number of Baths", y = "Number of Rooms") +
  theme_minimal()

# # Install It
install.packages("VIM")
# Load the VIM library
library(VIM)

# Load the airquality dataset
data("airquality")

# "airquality" loaded into your working environment. Display the first few rows of the dataset
head(airquality)

# Which of the following variables are 100% complete?

colSums(is.na(airquality))

# Which month (numeric format, e.g., 9 for September) has the highest average temperature across all days?

airquality %>%
  group_by(Month) %>%
  summarize(avg_temp = mean(Temp, na.rm = TRUE)) %>%
  arrange(desc(avg_temp)) %>%
  slice(1) %>%
  pull(Month)

# display that
airquality %>%
  group_by(Month) %>%
  summarize(avg_temp = mean(Temp, na.rm = TRUE)) %>%
  arrange(desc(avg_temp))
# Create a histogram for Solar.R in the airquality dataset. Answer the following questions.

ggplot(airquality, aes(x = Solar.R)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Solar Radiation", x = "Solar Radiation (Solar.R)", y = "Frequency")

# give me the sum of frequencies of Solar Radiation between 0-100, 100-200, 200-300, and 300-400
solar_bins <- cut(airquality$Solar.R, breaks = c(0, 100, 200, 300, 400), right = FALSE)
table(solar_bins)
# maximum observed?
max(airquality$Solar.R, na.rm = TRUE)
# number of solar radiation values below or equal to 100
sum(airquality$Solar.R <= 100, na.rm = TRUE)
# Draw scatterplot matrix for "Ozone", "Solar.R", "Wind", "Temp" in the airquality dataset. Answer the following questions.

pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")])
# give me a table of the correlational coefficients
cor(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], use = "complete.obs")
# Which of the following shows evidence of skew in its distribution?

ggplot(airquality, aes(x = Solar.R)) +
  geom_histogram(binwidth = 25, fill = "lightcoral", color = "black") +
  labs(title = "Histogram of Ozone Levels", x = "Ozone", y = "Frequency")
# Create box plots for "Wind" by "Month" to visualize the distribution of Wind across different Months. 

ggplot(airquality, aes(x = as.factor(Month), y = Wind)) +
  geom_boxplot(fill = "lightyellow", color = "black") +
  labs(title = "Boxplot of Wind by Month", x = "Month", y = "Wind (mph)")
# which month has the smallest IQR?
boxplot_stats <- boxplot.stats(airquality$Wind ~ airquality$Month)
iqr_values <- sapply(split(airquality$Wind, airquality$Month), IQR, na.rm = TRUE)
min_month <- names(which.min(iqr_values))
min_month
# which have outleirs
outlier_months <- sapply(split(airquality$Wind, airquality$Month), function(x) length(boxplot.stats(x)$out))
outlier_months[outlier_months > 0]

