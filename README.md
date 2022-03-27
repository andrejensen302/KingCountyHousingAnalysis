# King County Housing Data - Exploration and Analysis #
Analysis of King County Housing Dataset

This dataset contains house sale prices for King County, which includes Seattle. It includes homes sold between May 2014 and May 2015. The purpose of this analysis was to evaluate pricing patterns and create predictions using various prediction models (after data exploration and wrangling). Models generated include linear regression, Random Forest Tree Models, and BAGGED Tree models.

Libraries used

```
#Load libraries

library(dplyr)
library(ggplot2)
library(ggdendro)
library(stringr)
library(corrgram)
library(randomForest)
library(GGally)
library(caret)
library(tidyverse)
library(readxl)
library(rpart)
library(rpart.plot)
library(rsample)

```
- The data includes 20, independent variable column and 1 ID variable.

```
Rows: 21,613
Columns: 21
$ id            <dbl> 7129300520, 6414100192, 5631500400, 2487200875, 1954400510, 7237…
$ date          <chr> "20141013T000000", "20141209T000000", "20150225T000000", "201412…
$ price         <dbl> 221900, 538000, 180000, 604000, 510000, 1225000, 257500, 291850,…
$ bedrooms      <dbl> 3, 3, 2, 4, 3, 4, 3, 3, 3, 3, 3, 2, 3, 3, 5, 4, 3, 4, 2, 3, 4, 3…
$ bathrooms     <dbl> 1.00, 2.25, 1.00, 3.00, 2.00, 4.50, 2.25, 1.50, 1.00, 2.50, 2.50…
$ sqft_living   <dbl> 1180, 2570, 770, 1960, 1680, 5420, 1715, 1060, 1780, 1890, 3560,…
$ sqft_lot      <dbl> 5650, 7242, 10000, 5000, 8080, 101930, 6819, 9711, 7470, 6560, 9…
$ floors        <dbl> 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 2.0, 1.0, 1.0, 1.5,…
$ waterfront    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ view          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 4…
$ condition     <dbl> 3, 3, 3, 5, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 4, 4, 4, 4, 3…
$ grade         <dbl> 7, 7, 6, 7, 8, 11, 7, 7, 7, 7, 8, 7, 7, 7, 7, 9, 7, 7, 7, 7, 7, …
$ sqft_above    <dbl> 1180, 2170, 770, 1050, 1680, 3890, 1715, 1060, 1050, 1890, 1860,…
$ sqft_basement <dbl> 0, 400, 0, 910, 0, 1530, 0, 0, 730, 0, 1700, 300, 0, 0, 0, 970, …
$ yr_built      <dbl> 1955, 1951, 1933, 1965, 1987, 2001, 1995, 1963, 1960, 2003, 1965…
$ yr_renovated  <dbl> 0, 1991, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ zipcode       <dbl> 98178, 98125, 98028, 98136, 98074, 98053, 98003, 98198, 98146, 9…
$ lat           <dbl> 47.5112, 47.7210, 47.7379, 47.5208, 47.6168, 47.6561, 47.3097, 4…
$ long          <dbl> -122.257, -122.319, -122.233, -122.393, -122.045, -122.005, -122…
$ sqft_living15 <dbl> 1340, 1690, 2720, 1360, 1800, 4760, 2238, 1650, 1780, 2390, 2210…
$ sqft_lot15    <dbl> 5650, 7639, 8062, 5000, 7503, 101930, 6819, 9711, 8113, 7570, 89…

```

## Main Problem Questions ##

1. What are the most relevant correlating variables to the price of a house in King County, WA?
2. Which variables have the biggest correlation to the price of a house in King County, WA?

### Data Wrangling ###

- Variables Removed
  - ID: Removed because the transaction ID had no relevance to the price.
  - Zipcode: Removed because for the purposes of this analysis, the model will focus on a generalized prediction rather than price differentials between specific zip codes.
  - Year Renovated: Removed because the primary focus on build date of the property as part of the analysis and prediction
  - Date: Removed because for the purposes of this analysis, the model will focus on a generalized prediction rather than seasonal trends.

```

#View class types of variables for possible data type conversion
class(kc_house_data_1$id)
class(kc_house_data_1$view)
class(kc_house_data_1$grade)
class(kc_house_data_1$condition)
class(kc_house_data_1$lat)
class(kc_house_data_1$long)
class(kc_house_data_1$price)

#Remove columns from dataset that won't contribute to a good predictation model
house_data_set$id <- NULL
house_data_set$zipcode <- NULL
house_data_set$yr_renovated <- NULL

#In the interest of creating a simpler model, we are removing the date from the dataset and not look at seasonal trends
house_data_set$date <- NULL

#Change variable types
house_data_set = house_data_set %>% mutate(waterfront = as.factor(waterfront),
                                           view = as.factor(view),
                                           grade = as.factor(grade), 
                                           condition = as.factor(condition), 
                                           lat = as.factor(lat), long = as.factor(long))

```

### Data Exploration ###

- Reviewing Correlation between variables. The closer the correlation between two variables is to 1, the more closely correlated they are. As expected, the sqft_living and number of bathrooms are the two most closely correlated variables to the price.

<img src ="https://github.com/andrejensen302/KingCountyHousingAnalysis/blob/1ba5aa65adc5f4802db5e794e07ab5d093fd1f08/KC_Housing_RMD_files/figure-gfm/unnamed-chunk-3-1.png" width="800" height="500">

- To get a rough idea of the housing prices in this dataset. I also created the following categorical variables to group the data and create averages.

```
#create categorical variables to analyze mean price against various criteria in the dataset.

#Create variable for Newer Construction (2004 or newer) criteria
house_data_set = house_data_set %>% mutate(new_construction = ifelse(yr_built >= 2004, TRUE, FALSE))

#Create new variable for four bedroom criteria
house_data_set = house_data_set %>% mutate(four_bedroom = ifelse(bedrooms >= 4, TRUE, FALSE))

#Create new variable for 3 bathroom criteria
house_data_set= house_data_set %>% mutate(three_bathroom = ifelse(bathrooms >= 3.00, TRUE, FALSE))

#Create new variable for 4,000 sqft living criteria
house_data_set= house_data_set %>% mutate(sqft_living_criteria = ifelse(sqft_living >= 4000, TRUE, FALSE))

#Create new variable for 4,000 sqft15 living criteria
house_data_set= house_data_set %>% mutate(sqft_living15_criteria = ifelse(sqft_living15 >= 4000, TRUE, FALSE))

#Create new variable for Good grade (7+) criteria
#summary shows that housing grades range from 1 to 13
house_data_set$grade %>% summary()
good_grade_values <- c('7', '8', '9', '10', '11', '12', '13')
house_data_set= house_data_set %>% mutate(good_grade = ifelse(grade %in% good_grade_values, TRUE, FALSE))

#Create variable for good condition (5+) criteria
house_data_set$condition %>% summary
good_condition_values <- c('5')
house_data_set = house_data_set %>% mutate(good_condition = ifelse(condition %in% good_condition_values, TRUE, FALSE))

#Create new variable for 5,000 sqft lot criteria
house_data_set= house_data_set %>% mutate (sqft_lot_criteria = ifelse(sqft_lot >= 5000, TRUE, FALSE))

#Create new variable for 5,000 sqft_lot15 criteria
house_data_set= house_data_set %>% mutate (sqft_lot15_criteria = ifelse(sqft_lot15 >= 5000, TRUE, FALSE))

#Add column for log of sqft_living

house_data_set = house_data_set %>% mutate (log_sqft_living = log10(sqft_living))

#Add column for square root of sqft_living

house_data_set = house_data_set %>% mutate(sqrt_sqft_living = sqrt(sqft_living))

#Show quick summary of average prices grouped by the individual criteria above 
#(four bedroom, three bathroom, 4,000 sqft living, 7+ good grade, 5+ good condition, & 5,000 sqft lot)

#Average price grouped by four bedroom houses
avg_price_of_four_bedrooms = house_data_set %>% na.omit() %>% 
  group_by(four_bedroom) %>% 
  summarize(mean_price = mean(price))

#Average price grouped by three bathrooms houses
avg_price_of_three_bathrooms = house_data_set %>% na.omit() %>%
  group_by(three_bathroom) %>% 
  summarize(mean_price = mean(price)) 

#Average price grouped by 4,000 sqft_living houses
avg_price_of_4000_sqft = house_data_set %>% na.omit() %>%
  group_by(sqft_living_criteria) %>% 
  summarize(mean_price = mean(price)) 

#Average price grouped by 4,000 sqft_living15 houses
avg_price_of_4000_sqft15 = house_data_set %>% na.omit() %>%
  group_by(sqft_living15_criteria) %>% 
  summarize(mean_price = mean(price)) 

#Average price grouped by 5,000 sqft lot houses
avg_price_of_5000_sqft_lot = house_data_set %>% na.omit() %>%
  group_by(sqft_lot_criteria) %>% 
  summarize(mean_price = mean(price)) 

#Average price grouuped by 5,000 sqft_lot15 houses
avg_price_of_5000_sqft_lot = house_data_set %>% na.omit() %>%
  group_by(sqft_lot15_criteria) %>% 
  summarize(mean_price = mean(price)) 

#Average prices grouped by good condition (5+) houses
avg_price_of_good_condition = house_data_set %>% na.omit() %>%
  group_by(good_condition) %>% 
  summarize(mean_price = mean(price)) 

#Average prices grouped by new construction (>2004 construction) houses
avg_price_of_new_construction = house_data_set %>% na.omit() %>%
  group_by(new_construction) %>% 
  summarize(mean_price = mean(price)) 

#Average prices grouped by good grade (7+)
avg_price_of_good_grade = house_data_set %>% na.omit() %>%
  group_by(good_grade) %>% 
  summarize(mean_price = mean(price)) 
```


