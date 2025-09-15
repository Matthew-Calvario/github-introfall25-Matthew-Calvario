# 1. 
x<-5
typeof(x)

# 2. 
x<-"a"
type(a)

# 13.
x<- c(1,2,3,4,5)

for(i in x){
  print(i)
}

# 14. 

convert_to_celsius <- function(fahrenheit) {
  celsius <- (fahrenheit - 32) * 5/9
  return(celsius)
}

convert_to_celsius(13)

convert_to_celsius <- function(fahrenheit) {
  return((fahrenheit - 32) * 5/9)
}

convert_to_celsius(13)

# 15. 
sum2 <- function(x, y) {
  return(x+y)
}

sum2(3, 5)

# 16. 
x<-1:5

num_squares <- c()

for (v in x) {
  
  num_squares <-c(num_squares,v^2)
  
}

num_squares

getwd()

setwd("/Users/lynacalvario/Desktop/SYS3501/Data/")

iriscsv <- read.csv("iris.txt", header = TRUE, sep = ",")

summary(iriscsv)

iris[10:20,]
summary(iris)

iris[1,""]

# 23-30.
getwd()

setwd("/Users/lynacalvario/Desktop/SYS3501/Data/")

q1_2.csv <- read.csv("sales_q1-2.csv", header = TRUE, sep = ",")
q2.csv <- read.csv("sales_q2.csv", header = TRUE, sep = ",")
products_lookup.csv <- read.csv("products_lookup.csv", header = TRUE, sep = ",")


names(q2.csv)[names(q2.csv) == "trans_id"] <- "transaction_id"
names(q2.csv)[names(q2.csv) == "Region"] <- "region"
names(q2.csv)[names(q2.csv) == "prod_id"] <- "product_id"
names(q2.csv)[names(q2.csv) == "units"] <- "units_sold"
names(products_lookup.csv)[names(products_lookup.csv) == "product_code"] <- "product_id"


q1_2_merged <- merge(q1_2.csv, lookup, by = "product_id")
sales_q2_merged <- merge(q2.csv, products_lookup.csv, by = "product_id")

sales_q2_merged$revenue_usd <- sales_q2_merged$units_sold * sales_q2_merged$unit_price_usd

q1_2.csv$product_name <- NA
q1_2.csv$category <- NA
q1_2.csv$unit_price_usd <- NA

q1_2_merged$quarter <- "Q1_2"
sales_q2_merged$quarter   <- "Q2"

q1_2_merged <- q1_2_merged[, names(sales_q2_merged)]

full_merged <- rbind(q1_2_merged, sales_q2_merged)
full_merged



# After combining columns with 1 and 2 (2 only had a defficiency), merging.
# with lookup added three additional columns: product_name, category, and unit_price_usd
# thus, 5+3 = 8 total




