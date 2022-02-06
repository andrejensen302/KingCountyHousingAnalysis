KC\_Housing\_Final
================

``` r
#Load libraries

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(ggdendro)
library(stringr)
library(corrgram)
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'lattice'

    ## The following object is masked from 'package:corrgram':
    ## 
    ##     panel.fill

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ tibble  3.1.6     ✓ purrr   0.3.4
    ## ✓ tidyr   1.1.4     ✓ forcats 0.5.1
    ## ✓ readr   2.1.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x randomForest::combine() masks dplyr::combine()
    ## x dplyr::filter()         masks stats::filter()
    ## x dplyr::lag()            masks stats::lag()
    ## x purrr::lift()           masks caret::lift()
    ## x randomForest::margin()  masks ggplot2::margin()

``` r
library(readxl)
library(rpart)
library(rpart.plot)
library(rsample)

#Set seed for consistent output
set.seed(1234)

#Data wrangling completed in Excel
#All of the data was in one column separted by commas, used delimeter to separate the values into their individual columns

#Import dataset
setwd("~/Documents/RStudio_Uploads/King_County_Housing_Analysis")
kc_house_data_1 <- read_excel("kc_house_data (1).xls")

kc_house_data_1 %>% glimpse()
```

    ## Rows: 21,613
    ## Columns: 21
    ## $ id            <dbl> 7129300520, 6414100192, 5631500400, 2487200875, 19544005…
    ## $ date          <chr> "20141013T000000", "20141209T000000", "20150225T000000",…
    ## $ price         <dbl> 221900, 538000, 180000, 604000, 510000, 1225000, 257500,…
    ## $ bedrooms      <dbl> 3, 3, 2, 4, 3, 4, 3, 3, 3, 3, 3, 2, 3, 3, 5, 4, 3, 4, 2,…
    ## $ bathrooms     <dbl> 1.00, 2.25, 1.00, 3.00, 2.00, 4.50, 2.25, 1.50, 1.00, 2.…
    ## $ sqft_living   <dbl> 1180, 2570, 770, 1960, 1680, 5420, 1715, 1060, 1780, 189…
    ## $ sqft_lot      <dbl> 5650, 7242, 10000, 5000, 8080, 101930, 6819, 9711, 7470,…
    ## $ floors        <dbl> 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 2.0, 1.0, 1…
    ## $ waterfront    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ view          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0,…
    ## $ condition     <dbl> 3, 3, 3, 5, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 4, 4,…
    ## $ grade         <dbl> 7, 7, 6, 7, 8, 11, 7, 7, 7, 7, 8, 7, 7, 7, 7, 9, 7, 7, 7…
    ## $ sqft_above    <dbl> 1180, 2170, 770, 1050, 1680, 3890, 1715, 1060, 1050, 189…
    ## $ sqft_basement <dbl> 0, 400, 0, 910, 0, 1530, 0, 0, 730, 0, 1700, 300, 0, 0, …
    ## $ yr_built      <dbl> 1955, 1951, 1933, 1965, 1987, 2001, 1995, 1963, 1960, 20…
    ## $ yr_renovated  <dbl> 0, 1991, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ zipcode       <dbl> 98178, 98125, 98028, 98136, 98074, 98053, 98003, 98198, …
    ## $ lat           <dbl> 47.5112, 47.7210, 47.7379, 47.5208, 47.6168, 47.6561, 47…
    ## $ long          <dbl> -122.257, -122.319, -122.233, -122.393, -122.045, -122.0…
    ## $ sqft_living15 <dbl> 1340, 1690, 2720, 1360, 1800, 4760, 2238, 1650, 1780, 23…
    ## $ sqft_lot15    <dbl> 5650, 7639, 8062, 5000, 7503, 101930, 6819, 9711, 8113, …

``` r
house_data_set = kc_house_data_1
```

## R Markdown make a difference

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
#Prepare dataset

#View class types of variables for possible data type conversion
class(kc_house_data_1$id)
```

    ## [1] "numeric"

``` r
class(kc_house_data_1$view)
```

    ## [1] "numeric"

``` r
class(kc_house_data_1$grade)
```

    ## [1] "numeric"

``` r
class(kc_house_data_1$condition)
```

    ## [1] "numeric"

``` r
class(kc_house_data_1$lat)
```

    ## [1] "numeric"

``` r
class(kc_house_data_1$long)
```

    ## [1] "numeric"

``` r
class(kc_house_data_1$price)
```

    ## [1] "numeric"

``` r
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

#Verify variable type changes
is.factor(house_data_set$waterfront)
```

    ## [1] TRUE

``` r
is.factor(house_data_set$view)
```

    ## [1] TRUE

``` r
is.factor(house_data_set$grade)
```

    ## [1] TRUE

``` r
is.factor(house_data_set$condition)
```

    ## [1] TRUE

``` r
is.factor(house_data_set$lat)
```

    ## [1] TRUE

``` r
is.factor(house_data_set$long)
```

    ## [1] TRUE

## Including Plots

You can also embed plots, for example:

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
