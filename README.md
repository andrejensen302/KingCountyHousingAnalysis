# King County Housing Analysis #
Analysis of King County Housing Dataset

This dataset contains house sale prices for King County, which includes Seattle. It includes homes sold between May 2014 and May 2015. The purpose of this analysis was to evaluate pricing patterns and create predictions using various prediction models (after data exploration and wrangling). Models generated include linear regression, Random Forest Tree Models, and BAGGED Tree models.

- The data includes 20, independent variable column and 1 ID variable.
<img width="800" height="800"  src="https://github.com/andrejensen302/KingCountyHousingAnalysis/blob/69b8fc3ac5f013c0e3c112a6a5f54c9528d97085/misc_images/Glimpse%20KC%20dataset.png">

## Main Problem Questions ##

1. What are the most relevant correlating variables to the price of a house in King County, WA?
2. Which variables have the biggest correlation to the price of a house in King County, WA?

### Data Wrangling ###

- Variables Removed
  - ID: Removed because the transaction ID had no relevance to the price.
  - Zipcode: Removed because for the purposes of this analysis, the model will focus on a generalized prediction rather than price differentials between specific zip codes.
  - Year Renovated: Removed because the primary focus on build date of the property as part of the analysis and prediction
  - Date: Removed because for the purposes of this analysis, the model will focus on a generalized prediction rather than seasonal trends.

