# Holiday homes
Data and methods to analayse to identify factors associated with planning of holiday home areas in Norwegian municipalities. 

The repository include the following files: 

## Data
*holiday_home_data_variable_description.csv* Metadata: description of the variables included in the analysis

*holiday_home_data.csv* 
  Holiday home data and co-variates for 356 municipalities in Norway.

*mountain_data.csv* Holiday home data and co-variates for 97 municipalities in Norway

*z_mountain_data.csv* Holiday home data and co-variates for 97 municipalities in Norway. Predictor variables are transformed to z-scores.

*zs_mountain_data.csv* Holiday home data and co-variates for 97 municipalities in Norway. Skewed predictor variables are transformed to reduce skewness. Predictor variables are then transformed to z-scores.

## Analysis

*data_wrangling_and_exploration.R* General script to import and explore the data, and prepare the data for modelling. 

*multivariate_analysis_mountains.R* Multivariate analysis of data for mountain municipalities

*explorative_modeling_mountain_municipalities.R* First attempt to explore associations.

