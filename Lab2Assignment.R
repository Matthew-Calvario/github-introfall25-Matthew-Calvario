# Helper to install packages if missing, then load them.
need <- c(
  "ggplot2", "dplyr", "tidyr", "stringr", "readr", "tibble", "purrr",
  "psych", "lattice", "gplots", "RColorBrewer", "corrplot", "maps"
)

install_if_missing <- function(pkgs) {
  to_get <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(to_get)) install.packages(to_get, dependencies = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

install_if_missing(need)
# 1) Paths & file I/O
# ------------------------------

# 1.1 Set working directories -------------------------------------------------
# EDIT THESE for your environment
traindir <- "/Users/lynacalvario/Desktop/SYS3501/Data/"
sourcedir <- "/Users/lynacalvario/Desktop/SYS3501/"

stopifnot(dir.exists(traindir))
setwd(traindir)
message("Working directory set to: ", getwd())

safe_read <- function(file) {

  read.csv(file, na.strings = c("", "NA", "N/A"," NA", "NA "), stringsAsFactors = FALSE)
}

totacts <- safe_read("Railroad_Equipment_Accident_Incident_Source_Data__Form_54__20250907.csv")

# How big is totacts? 222,740-X 145 Variables

# Sanity checks on indices used throughout- check that IYR is 1-25
stopifnot("IYR" %in% names(totacts))
typeof(totacts$IYR)
summary(totacts$IYR)
head(totacts$IYR)
table(totacts$IYR)
stopifnot(all(totacts$IYR %in% 0:99))

# 1. Read in the full dataset
totacts <- read.csv("Railroad_Equipment_Accident_Incident_Source_Data__Form_54__20250907.csv",
                    stringsAsFactors = FALSE)

# 10.

if(!dir.exists("yearly_files")) {
  dir.create("yearly_files")
}

years <- unique(totacts$Year)

for (yr in years) {
  year_data <- totacts[totacts$Year == yr, ]
  filename <- paste0("yearly_files/train_accidents_", yr, ".csv")
  write.csv(year_data, file = filename, row.names = FALSE)
}


list.files("yearly_files")

files <- list.files("yearly_files", full.names = TRUE)

files_info <- file.info(files)

files_info$year <- sub(".*_(\\d+)\\.csv", "\\1", rownames(files_info))

files_info[, c("year", "size")]


oldest_year <- min(totacts$IYR)
newest_year <- max(totacts$IYR)
oldest_year
newest_year

# 11. 

highspd <- totacts$HIGHSPD
summary(highspd)
highspd


# 12.

temp <- totacts$TEMP
summary(temp)

# 13. 

# Filter to IYR >= 1 (2001 and later, since IYR=0 means 2000)
library(ggplot2)

plotdata <- subset(totacts, IYR >= 1)

ggplot(plotdata, aes(x = factor(IYR), y = EQPDMG)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Equipment Damage by Year (2001+)",
       x = "IYR",
       y = "Equipment Damage (log scale)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 14.

ggplot(plotdata, aes(x = factor(IYR), y = TOTKLD)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Total Killed by Year (2001+)",
       x = "IYR",
       y = "Total Killed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 15.


ggplot(plotdata, aes(x = factor(IYR), y = TRKDMG)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Track Damage by Year (2001+)",
       x = "IYR",
       y = "Track Damage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 16.


ggplot(plotdata, aes(x = factor(IYR), y = TOTINJ)) +
  geom_boxplot() +   # <-- this makes it a boxplot
  labs(title = "Total Injured by Year(2001+)",
       x = "IYR",
       y = "Total Injured") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 17.

ggplot(plotdata, aes(x = factor(IYR), y = CARSDMG)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Cars Damaged (2001+)",
       x = "IYR",
       y = "Cars Damaged") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 18. 


incidents <- totacts$INCDTNO
total_injured <- totacts$TOTINJ

injuries <- totacts[, c("INCDTNO", "TOTINJ")]
head(injuries)

most_injuries <- totacts[which.max(totacts$TOTINJ), c("INCDTNO", "TOTINJ")]
most_injuries

summary(total_injured)

# 19.

recent <- subset(totacts, IYR >= 1)  

par(mfrow = c(1, 3))

# 20.

recent <- subset(totacts, IYR >= 1)

n_breaks <- nclass.Sturges(recent$TEMP)

ggplot(recent, aes(x = TEMP)) +
  geom_histogram(bins = 400,  
                 fill = NA, colour = "steelblue") +
  coord_cartesian(xlim = c(0, 120)) +
  labs(title = "Histogram of TEMP (2001–present, 0–120°F)",
       x = "Temperature (°F)",
       y = "Count")

# 21.

# Isolate years between 2001-2024; so no numbers between 25-99
recent <- subset(totacts, IYR >= 1 & IYR <= 24)
# All questions in this part refer to a scatter plot matrix created with the pairs.panels function from the psych library for the variables "TRKDMG", "EQPDMG", "ACCDMG","TOTINJ", and "TOTKLD" using data for all the accidents from 2001 to the present.

psych::pairs.panels(recent[, c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])

# What is the correlation between TRKDMG and ACCDMG rounded to 2 significant digits?
cor <- cor(recent$TRKDMG, recent$ACCDMG, use = "pairwise.complete.obs")
cor

# 24.

# Which year had the accident with the largest value of ACCDMG? (write the year with 4 digits)
max_accdmg <- recent[which.max(recent$ACCDMG), ]
max_accdmg
max_accdmg$YEAR + 2000  # Convert to 4-digit year

# 25. what type of accident was the highest ACCDMG
max_accdmg$TYPE
# accident with most deaths
max_killed <- recent[which.max(recent$TOTKLD), ]
max_killed

# 26. 

# Across ALL years, what is the maximum value of ACCDMG?
max_accdmg_value <- max(totacts$ACCDMG, na.rm = TRUE)
max_accdmg_value

# 27. 
# Across all years, what is the maximum number of deaths in one accident?

max_killed_value <- max(totacts$TOTKLD, na.rm = TRUE)

# 28. 
# Across all years, which year had the largest number of deaths from a train accident? (Write it in 4 digits)
max_killed_year <- totacts[which.max(totacts$TOTKLD), ]
max_killed_year

# 29.
# Across all years, how many accidents have total accident damage (ACCDMG) greater than $1.5M?
num_accidents_gt_1_5M <- sum(totacts$ACCDMG > 1500000, na.rm = TRUE)
num_accidents_gt_1_5M

# 30.
# Across all years, how many accidents have one or more deaths?
num_accidents_with_deaths <- sum(totacts$TOTKLD >= 1, na.rm = TRUE)
num_accidents_with_deaths

# 31.
# Make a barplot of the frequency of each type of accident across all years. What is the most common type of accident?
barplot(table(totacts$TYPE), las = 2, col = "steelblue",
        main = "Frequency of Accident Types",
        ylab = "Number of Accidents")
# 32.
# Make a barplot of the frequency of the five major causes of accidents across all years. The single largest cause falls under what category?

totacts$CAUSE_CAT <- substr(totacts$CAUSE, 1, 1)
cause_labels <- c("T"="Track","H"="Human factor","M"="Mechanical/Electrical",
                  "S"="Signal/Train control","E"="Electrical/Other",
                  "X"="Miscellaneous","Z"="Miscellaneous")
totacts$CAUSE_CAT <- cause_labels[totacts$CAUSE_CAT]
barplot(table(totacts$CAUSE_CAT), las=2, col="steelblue",
        main="Frequency of Accident Cause Categories",
        ylab="Number of Accidents", xlab="Cause Category")

# 33.
# Create a boxplot of the total accident damage across all years using ggplot.
ggplot(totacts, aes(x = "", y = ACCDMG)) +
  geom_boxplot(fill = "steelblue") +
  scale_y_log10() +
  labs(title = "Boxplot of Total Accident Damage (ACCDMG)",
       y = "Total Accident Damage (log scale)") +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))
# What is the value of the upper whisker?
box_stats <- boxplot.stats(totacts$ACCDMG)
box_stats
box_stats$stats[5]  # Upper whisker value
# 34.
# How many accidents that occurred from 2001 to 2023 are greater than the upper whisker of your box plot?
# isolate year 2001 to 2023
totacts_2001_2023 <- subset(totacts, IYR >= 1 & IYR <= 23)
n_accidents_above_whisker_2001_2023 <- sum(totacts_2001_2023$ACCDMG > box_stats$stats[5], na.rm = TRUE)
upper_whisker <- box_stats$stats[5]
n_accidents_above_whisker_2001_2023

# 35. 
# What proportion of accidents are extreme in the total accidents data accross all years? Round to the nearest whole number (percent).
# calculate number of accidents above whisker across all years
n_accidents_above_whisker <- sum(totacts$ACCDMG > upper_whisker, na.rm = TRUE)
total_accidents <- nrow(totacts)
proportion_extreme <- (n_accidents_above_whisker / total_accidents) * 100
round(proportion_extreme)

# 36.
# What cost proportion are these extreme accidents relative to all accidents? Round to the nearest whole number (percent).
total_cost_extreme <- sum(totacts$ACCDMG[totacts$ACCDMG > upper_whisker], na.rm = TRUE)
total_cost_all <- sum(totacts$ACCDMG, na.rm = TRUE)
cost_proportion_extreme <- (total_cost_extreme / total_cost_all) * 100
round(cost_proportion_extreme)
# 37.

extreme_15M <- subset(totacts, ACCDMG > 15000000)
head(extreme_15M)
extreme_15M[["YEAR", "MONTH", "DAY", "STATION", "STATE", "ACCDMG", "TYPE"]]

# 38.
#  Across all years, how many duplicates are in the extreme accident data set (where ACCDMG > upper whisker) using the following variables: "INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN"?
extreme_accidents <- subset(totacts, ACCDMG > upper_whisker)
extreme_accidents_unique <- extreme_accidents[!duplicated(extreme_accidents[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]), ]
n_duplicates_extreme <- nrow(extreme_accidents) - nrow(extreme_accidents_unique)
n_duplicates_extreme