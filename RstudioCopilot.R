#The goal of this assignment Verify you can use Microsoft Copilot / GitHub Copilot Chat alongside RStudio 
#to accelerate—but not replace—your coding.

# ------------------------------
# 1) Paths & file I/O
# ------------------------------

# 1.1 Set working directories -------------------------------------------------
# EDIT THESE for your environment
traindir <- "/Users/lynacalvario/Desktop/SYS3501/Data/"
sourcedir <- "/Users/lynacalvario/Desktop/SYS3501"

stopifnot(dir.exists(traindir))
setwd(traindir)
message("Working directory set to: ", getwd())

# 1.3 Safe CSV read helper ----------------------------------------------------
safe_read <- function(file) {
  # Matches read.csv defaults from class, but robust NA handling
  read.csv(file, na.strings = c("", "NA", "N/A"," NA", "NA "), stringsAsFactors = FALSE)
}

# 1.4 Read the accident CSV ------------------------------------------------
totacts <- safe_read("Railroad_Equipment_Accident_Incident_Source_Data__Form_54__20250907.csv")
summarytotacts <- summary(totacts)
# ------------------------------
# Prompt 1. Filter the data to include only accidents that occurred between the years 2021 and 2024 and name the new dataframe totacts2124.
totacts2124 <- subset(totacts, IYR >= 21 & IYR <= 24)
summarytotacts2124 <- summary(totacts2124)

# The code did not work as intended. First, it called "YEAR" and did not call "IYR", which is the actual
# column for the year in the dataset. Secondly, the years are two digits not four.

# Prompt 2. Create a new column in the totacts2124 dataframe that sums TOTINJ + TOTKLD for each accident. Call the column CASINJ.

totacts2124$CASINJ <- totacts2124$TOTINJ + totacts2124$TOTKLD
names(totacts2124)

# The code did the job; however, the only problem is that the CASINJ column was already part of the Dataset.
# Therefore, it did not append it as a new column but rather overwrote the existing one. Regardless, it created
# the column as intended.

# Prompt 3. Convert the two-digit IYR year column to a four-digit year column. Call the new column YEAR.
totacts2124$YEAR <- ifelse(totacts2124$IYR >= 50, 1900 + totacts2124$IYR, 2000 + totacts2124$IYR)
summary(totacts2124$YEAR)
head(totacts2124$YEAR)

# The code worked as intended by converting the two digit years into four digits.

# Prompt 4. What are the new dimensions of the dataframe totacts2124?
dim(totacts2124)
# The code worked as intended. The new dimensions are 10303 rows and 145 columns.