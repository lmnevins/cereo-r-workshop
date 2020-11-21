library(tidyverse)

bike_data <- read.csv(file = "data/daily_bike_data.csv", stringsAsFactors = FALSE)

head(bike_data)

# min
col_min <- min(bike_data[, "registered"], na.rm = TRUE)
col_min
# max
col_max <- max(bike_data[, "registered"], na.rm = TRUE)
col_max
# median
col_med <- median(bike_data[, "registered"], na.rm = TRUE)
col_med


summary_fun <- function(data, col_name){
  col_min <- min(data[, col_name], na.rm = TRUE)
  col_max <- max(data[, col_name], na.rm = TRUE)
  col_med <- median(data[, col_name], na.rm = TRUE)
  
  # Return the value 
  return(c(col_min, col_max, col_med))
}

summary_fun(data = bike_data, col_name = "registered")

summary_fun(data = bike_data, col_name = "casual")


# Challenge Question 1
prop_reg <- function(data, col_one, col_two){
  
  prop <- data[, col_one] / (data[, col_one] + data[, col_two])
  
  return(prop)
  
}
  
prop_reg(data = bike_data,
         col_one = "registered",
         col_two = "casual")
  
  
  
  
  




