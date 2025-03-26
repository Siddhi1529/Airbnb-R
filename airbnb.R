# install and packages and libraries needed

install.packages("tidyverse")
install.packages("ggplot")
install.packages("dplyr")  

library(tidyverse)
library(ggplot2)
library(dplyr) 
library(gtExtras) # to neaten our tables
library(stringr)
##################################################################
# STEP 1 EXPLORE THE DATA-

# Read the dataset
data <- read.csv("Airbnb_Open_Data.csv")

View(data) # gives the view of entire dataset

head(data) # first 6 rows 

tail(data) # last 6 rows

dim(data) # shape - 102599, 26

names(data) # names of all the columns

str(data) # to check the datatype

glimpse(data) # better than str to see the data

sum(duplicated(data)) # there are 541 duplicate values


## with the glimpse we can see all the datatypes for the columns. 
## we can see that price, service fee have character dataype rather than int and last review is also character instead of date


##################################################################
# STEP 2 - DATA CLEANING 

# next lets drop our dupliacted rows

data <- data %>% 
  distinct() 

dim(data) # 102058 , 26

# next, lets deal with our null / misisng values
names(data)

# lets look at the missing values - 
# lets convert all blank rows to NA

data <- data %>% 
  mutate(across(where(is.character), ~na_if(., ""))) 
View(data)

data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Missing Values") %>%
  gt() %>% # neaten the table
  tab_header(title = "Missing Values Across the Dataset") %>%
  cols_align(align = 'left') %>%
  gt_theme_dark()

# the number of missing data differs across the dataset with missing values in 23 columns
# We cannot directly remove these, we will perform data manipulation on this 


# the license column is almost empty. There are 102597 rows are missing, hence this isnt a column that would be useful for us.
# there are just 2 rows. We can see what they are, but they are of no use, so lets drop

data %>%
  filter(!is.na(license)) %>%
  select(license) 

# drop column license- 
data <- data %>% 
  select(-license)

View(data)


# next lets look at the house rules column, there are 51842, almost half missing values
# these rules dont account to any specific need. We can check an individual rule, but its not important for our purpose, so we will drop
data <- data %>% 
  select(-house_rules)

# for remaining columns, let do the cleaning based on the questions we are planning to answer
# data cleaning is based on your problem statement. Whether to drop or do imputation differs according to the question.
# For this dataset we will will tackle rest missing values based on the following problems

# STEP 3 - DATA MANIPULATION
# first lets start with dealing with our datatype conversion

data <-data %>% 
  mutate(price = as.numeric(str_remove(price, "\\$"))) %>% 
  mutate(service.fee = as.numeric(str_remove(service.fee, "\\$"))) %>% 
  mutate(last.review = as.Date(last.review, format = "%m/%d/%Y"))
View(data)


########################################################################
# STEP 4 - DATA DESCRIBE AND SUMMARY

# lets check the summary for our numeric data columns
data %>% 
  select(price, service.fee, minimum.nights, number.of.reviews,
         review.rate.number, reviews.per.month,
         calculated.host.listings.count, availability.365) %>% 
  summary()

## this summary is useful for us to look at the distribution of the dataset
# minimum nights, number of reviews, review rate number and review rate month and calulated.host.listing.count, doesnt have any spread. 
##################################################################
# STEP 5 - DATA VISUALIZATION - QUESTIONS

# QS1 - Let a gt plot summary table for all the numeric columns to see the summary 
data %>%
  select(price, service.fee, minimum.nights, number.of.reviews,
         review.rate.number, reviews.per.month,
         calculated.host.listings.count, availability.365) %>%
  drop_na() %>%  # Drop only for visualization
  gt_plt_summary()

## if we drop all nas, this is how the distribution looks like


##################################################################
# QS 2 - How does the average service fee vary by room type?

data %>% 
  select(service.fee, room.type) %>% 
  filter(!is.na(service.fee)) %>% 
  group_by(room.type) %>% 
  summarise(avg_fee = round(mean(service.fee, na.rm = TRUE), 2)) %>% 
  gt() 

##################################################################
# QS 3 - What is the distribution of listing prices? 

# lets see what the date range looks like to decide a suitable bin width
range(data$Construction.year, na.rm = TRUE)

data %>% 
  ggplot(aes(Construction.year)) +
  geom_histogram(binwidth = 1, fill = 'steelblue', color = "black") +
  theme_bw() + 
  labs(x = 'Construction years',
       y = "Count",
       title = "Histogram for years when the property was constructed")


##################################################################
# QS 4 - How many listings exist for each neighbourhood group?

data %>% 
  ggplot(aes(neighbourhood.group))+
  geom_bar(fill = "pink") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3) +  
  theme_classic() +
  labs(x = "Neighbourhood Group",
       y = "Count",
       title = "Listings exist for each neighbourhood group")


##################################################################
# QS 5 - How do service fees vary across different room types? 

data %>% 
  filter(!is.na(service.fee)) %>%  # Remove NAs
  ggplot(aes(x = service.fee)) +
  geom_area(stat = "density", fill = "steelblue",colour = "blue", alpha = 0.6) +
  facet_wrap(~room.type)+
  theme_bw() +
  labs(title = "Area Plot of Service Fees",
       x = "Service Fee",
       y = "Density")

##################################################################
# QS 6 - What is the distribution of listing prices across different neighborhoods?

data %>% 
  filter(!is.na(price))%>% 
  ggplot(aes(room.type, price)) +
  geom_boxplot() +
  coord_flip()+
  theme_grey() +
  labs(x = "Price",
       y = "Room Type",
       title = "Price according to the Type of Room")

##################################################################
# QS 7 - Do hosts with more listings charge higher prices?

data %>% 
  filter(!is.na(calculated.host.listings.count) & !is.na(price)) %>%
  ggplot(aes(calculated.host.listings.count, price)) +
  geom_point(colour = "steelblue") +
  labs(x = "Listings", 
       y = "Price", 
       title = "Scatter Plot of Host Listings vs. Price")


##################################################################
# QS 8 - How does the price distribution differ across cancellation policy types?

data %>%
  filter(!is.na(price) & !is.na(cancellation_policy)) %>%  # Remove missing values
  ggplot(aes(x = cancellation_policy, y = price, fill = cancellation_policy)) +
  geom_violin(alpha = 0.6) +  # Violin plot with transparency
  scale_y_continuous(limits = c(0, 500), oob = scales::squish) +  # Adjust scale to remove extreme outliers
  theme_minimal() +
  labs(
    title = "Price Distribution Across Room Types",
    x = "Room Type",
    y = "Price")


##########################################################################
# QS 9 - How has the number of reviews per month changed over time across different room types?

data %>% 
  filter(!is.na(last.review) & !is.na(reviews.per.month)) %>% 
  ggplot(aes(last.review, reviews.per.month)) +
  geom_point(size = 3) +
  geom_smooth() + 
  facet_wrap(~room.type)+
  theme_bw() +
  labs(title = "Line Graph for reviews based on last review year")

##################################################################
# QS 10 - How do verified vs. unverified hosts vary across boroughs?

data %>% 
  filter(!is.na(host_identity_verified) & !is.na(neighbourhood.group)) %>% 
  ggplot(aes(x = neighbourhood.group, fill = host_identity_verified)) +
  geom_bar(position = "fill") +  # Use "stack" for absolute counts, "fill" for proportions
  theme_minimal() +
  labs(title = "Proportion of Verified vs. Unverified Hosts Across Boroughs",
       x = "Borough",
       y = "Proportion",
       fill = "Host Verification Status")

##################################################################
# QS 11 - How does the average number of reviews per month vary across different neighborhood groups?

# lets start by fixing the names of 2 mis spelled names
data %>%
  mutate(neighbourhood.group = case_when(
    neighbourhood.group == "manhatan" ~ "Manhattan",
    neighbourhood.group == "Brooklyn" ~ "brookln",
    TRUE ~ neighbourhood.group )) %>% 
  group_by(neighbourhood.group) %>% 
  summarise(avg_review_per_month = round(mean(reviews.per.month, na.rm = T),2)) %>% 
  gt() 

##################################################################
# QS 12 - How do average price and service fees vary across different neighborhoods in the United States, and are there any geographical patterns visible on the map?

library(dplyr)
library(leaflet)
View(data)
# Aggregate data
map_data <- data %>%
  filter(country == "United States" & !is.na(neighbourhood)) %>%
  group_by(neighbourhood) %>% 
  summarise(
    avg_price = round(mean(price, na.rm = TRUE), 2),
    avg_service_fee = round(mean(service.fee, na.rm = TRUE), 2),
    lat = mean(lat, na.rm = TRUE),
    lon = mean(long, na.rm = TRUE)
  ) %>%
  filter(!is.na(lat) & !is.na(lon))  # Ensure valid coordinates

# Create a Leaflet map

library(leaflet)

leaflet(map_data) %>%
  addTiles() %>%  # Base map
  addAwesomeMarkers(
    ~lon, ~lat,  # Longitude and Latitude
    icon = awesomeIcons(
      icon = "map-marker-alt",  # FontAwesome location pin icon
      library = "fa",  # Use FontAwesome
      markerColor = "blue"  # Set the marker color to blue
    ),
    popup = ~paste0("<b>Neighbourhood: </b>", neighbourhood,
                    "<br><b>Avg Price: $</b>", avg_price,
                    "<br><b>Avg Service Fee: $</b>", avg_service_fee)
  ) %>%
  fitBounds(
    lng1 = min(map_data$lon, na.rm = TRUE), 
    lat1 = min(map_data$lat, na.rm = TRUE), 
    lng2 = max(map_data$lon, na.rm = TRUE), 
    lat2 = max(map_data$lat, na.rm = TRUE)
  )  # Auto-zoom to data bounds

##################################################################