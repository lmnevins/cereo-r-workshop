# Explore bike data to see if there is a relationship between weather and ridership 

library(tidyverse)

##### Load and check data --------
df <- read_csv("data/daily_bike_data.csv")

df

#### Exploration of data relationships ----

# Time trend of ridership
p <- ggplot(data = df) +
  geom_line(aes(x = dteday, y = cnt))


# Relationship between ridership and temperature
ggplot(data = df, aes(x = temp, y = cnt)) +       
  geom_point() +
  geom_smooth()
  
# If you put the data here the baselayer and all of the other layers would use it 
# Adding the data here lets you just use the data for that layer


# What is weathersit?
summary(df$weathersit)
unique(df$weathersit)

# Dplyr verbs (some of them):
# mutate: adds new columns to your data frame (adds new variables to your dataset)
# transmute: keeps only the new columns added 
# select: selects columns from the dataset that you want to view or put into a new dataset 
# filter: filters the rows according to a logical specification

df2 <- df %>%
  dplyr::mutate(
    weather_fac = factor(weathersit,
                         levels = c(1,2,3,4),
                         labels = c("Clear", "Cloudy", "Rainy", "Heavy Rain"))
  )

df2 %>% dplyr::select(dteday, weathersit, weather_fac) 

# Two equal signs changes to testing each item against the assignment

df2 %>% 
  dplyr::filter(weather_fac == "Clear") %>%
  ggplot(aes(x = temp, y =cnt)) +
  geom_point() +
  geom_smooth()

# dplyr::select, you can drop variables in the same way you select them

df3 <- df2 %>%
  dplyr::select (-weathersit)

# can also use character lists

keep_vars <- c("dteday", "weather_fac", "temp", "cnt")
df4 <- df2 %>% select(all_of(keep_vars))

# Can do looks of keeping and moving things without having to list them all out each time 

# Other ways of filtering, both of these do the same thing, the second one is faster than listing

weather_factors_we_like <- c("Rainy", "Cloudy")
df2 %>% dplyr::filter(weather_fac == "Rainy" | weather_fac == "Cloudy")
df2 %>% dplyr::filter(weather_fac %>% weather_factors_we_like)
df2 %>% dplyr::filter(weather_fac =! "Rainy") # =! is not equal to
df2 %>% dplyr::filter(!(weather_fac %in% weather_factors_we_like)) # Selects all of the factors not in that dataset

##dplyr::summarize
df2 %>% 
  dplyr::group_by(weather_fac) %>%
  dplyr::summarize(
    cnt_mean = mean(cnt)
  )