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

##dplyr::summarize   #Take the dataframe and apply whatever you're doing separately to each group
df2 %>% 
  dplyr::group_by(season, weather_fac) %>%
  dplyr::summarize(
    cnt_mean = mean(cnt)
  )


### Transforming data format from long to wide or vice-versa ----

#Transform data to create separate temp variables for each month  = practice making it wide
tidyr::pivot_wider(names_prefix = "temp_", names_from = mnth, values_from = temp)

df2_a <- df2 %>% dplyr::select(season, mnth, temp)
df2_b <- df2_a %>%
  dplyr::group_by(season, mnth) %>%
  dplyr::summarize(temp_mean = mean(temp)) %>%
  dplyr::ungroup()%>%
  dplyr::select(-season)

df2_b # Need to add year variable 

# Wanted to get means for temperatures by mean and year

months <- c("January", "February", "March", "April", "May", "June", 
           "July", "August", "September", "October", "November", "December")
df_wide <- df2 %>%
  dplyr::mutate(mnth = factor(mnth, levels = months, labels = months)) %>%
  dplyr::rename(year = yr) %>%
  dplyr::select(year, mnth, temp) %>%
  dplyr::group_by(year, mnth) %>%
  dplyr::summarize(temp_mean = mean(temp)) %>%
  tidyr::pivot_wider(names_prefix = "temp_", names_from = mnth, values_from = temp_mean) %>%
  dplyr::rename_with(tolower)

## Pivoting longer  = from lots of columns to lots of roads
# cols = what columns we want to stack 
# next line is the name for the values column, and the names for the variables column
df_long <- df2 %>%
  tidyr::pivot_longer(cols = c(temp, atemp, hum, windspeed), 
                      values_to = "value", names_to = "variable")

#Pivoting wider
df_wide2 <- df_long %>%
  tidyr::pivot_wider(names_prefix = "v_", names_from = variable, values_from = value)

 
# Shorter example by Matt - literally just flips columns and rows

df3 %>%
  group_by(weekday) %>%
  summarize(mean_temp = mean(temp)) %>%
  pivot_wider(names_from = weekday, 
              values_from = mean_temp)

## Facetting 
# Creating multiple plots for each category of data that you want to look at 

p <- ggplot(data = df2, aes(x = temp, y = cnt)) +
  geom_point(shape = 21, color = "orangered") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "steelblue", se = FALSE) +
  facet_wrap(~ weather_fac, scales = "free_y") +
  labs(x = "Temperature", y = "Ridership Count") +
  ggtitle("Relationship between temperature and ridership") +
  theme_linedraw() +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(color = "black"))

ggsave(plot = p, filename = "temp_count_scatter.png")


## Plotting with a longer data frame 

ggplot(data = df_long, aes(x = value, y = cnt, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ weather_fac)
  