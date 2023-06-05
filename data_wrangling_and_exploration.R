library(tidyverse) # data wrangling
library(tidymodels) # transformations, etc
library(car) #logit-transformations
library(corrplot) # correlation plots
library(summarytools) # summary of dataframes
library(kableExtra) # to make tables

Sys.setlocale("LC_CTYPE", "norwegian")

# Reading in data
data <- read.csv("holiday_home_data.csv", 
	header = TRUE, sep = ";", encoding = "latin") |> as_tibble()

# Recoding variables and variable types
data$region <- as.character(data$region)
data$region[data$region == "301"] <- "0301"
glimpse(data)

data$hh_category = as.factor(data$hh_category)
levels(data$hh_category) <- c("Coastal", "Forest", "Mountain")
data$hh_category

# Keeping copy of original data
data_orig <- data

# Making subset for mountain municipalities
data <- data |> filter(hh_category == "Mountain")

# Use the summarize() and across() functions to count NAs for each column
na_counts <- data %>% summarize(across(everything(), ~ sum(is.na(.))))
glimpse(na_counts)

# remove municipality with errors or missing data in one of the two reponse variables
data <- data |> filter(municipality != "Rana", municipality != "Snåase - Snåsa")

# Correlation matrix ------------------------------------------------------
# Omitting non-numerical variables
data_num <- data |> dplyr::select(-hh_category, -holiday_homes, -future_hh_area,
	-region, -municipality, -county, -coast) |> na.omit() 

correlations <- round(cor(data_num, method = "kendall"),2)
correlations
corrplot(correlations, method = 'number', order = "hclust")
# write.csv(correlations, file = "correlations_mountains.csv", row.names = FALSE)

# Extract response variables ---------------------------------------------------------
response1 <- data |> dplyr::select(holiday_homes) 
response2 <- data |> dplyr::select(future_hh_area) 

# data for correlation analysis
dat <- data |> dplyr::select(-hh_category, -region, -municipality, -county, -coast)
dat

# Correlation with response 1: holiday homes ---------------------------------------------

# Create an empty dataframe to store the results
cor_summary <- data.frame(variable = character(),
                          cor_with_hh = character(),
                          p_value_t1 = character(),
                          stringsAsFactors = FALSE)

# Perform Kendall correlation test of response with each variable
for (col_name in names(dat)) {
  if (col_name != "holiday_homes") {
    result <- cor.test(dat$holiday_homes, dat[[col_name]], method = "kendall")
    p_value <- ifelse(result$p.value < 0.001, "<0.001", sprintf("%.3f", result$p.value))
    cor_summary <- cor_summary %>%
      add_row(variable = col_name,
              cor_with_hh = sprintf("%.3f", result$estimate),
              p_value_t1 = p_value)
  }
}

# Print the summary dataframe
cor_summary1 <- (cor_summary)
cor_summary1

# Correlation with response 2 future holiday home areas ---------------------------------------------

# Create an empty dataframe to store the results
cor_summary <- data.frame(variable = character(),
                          `cor_with_fha` = character(),
                          p_value_t2 = character(),
                          stringsAsFactors = FALSE)

# Perform Kendall correlation test of respons with each variable
for (col_name in names(dat)) {
  if (col_name != "future_hh_area") {
    result <- cor.test(dat$future_hh_area, dat[[col_name]], method = "kendall")
    p_value <- ifelse(result$p.value < 0.001, "<0.001", sprintf("%.3f", result$p.value))
    cor_summary <- cor_summary %>%
      add_row(variable = col_name,
              `cor_with_fha` = sprintf("%.3f", result$estimate),
              p_value_t2 = p_value)
  }
}

# Print the summary dataframe
cor_summary2 <- (cor_summary)
cor_summary2

# Join tables to obtain one table: correlations with response 1 and response 2 ---------------
cor_summary1
cor_summary2

df1 <- cor_summary1 |> filter(variable != "future_hh_area")
df1
df2 <- cor_summary2 |> filter(variable != "holiday_homes")
df2

# Check that the number of rows are equal and join
nrow(df1) == nrow (df2)
summary_cor_with_response <- left_join(df1, df2, by = "variable")
summary_cor_with_response


# Print the updated dataframe
print(summary_cor_with_response)

# Write the output from summary_cor_with_response to the clipboard
write.table(summary_cor_with_response, file = "clipboard", sep = "\t", row.names = FALSE)

# Recommended preprocessing outline
# Order of datawrangling steps before modeling:
# 
# Impute
# Handle factor levels
# Individual transformations for skewness and other issues
# Discretize (if needed and if you have no other choice)
# Create dummy variables
# Create interactions
# Normalization steps (center, scale, range, etc)

# Completa data: c_data: complete data, without missing data

c_data <- data
c_data


# Explore data ------------------------------------------------------------


c_data
glimpse(c_data)

na_counts <- c_data %>% summarize(across(everything(), ~ sum(is.na(.))))
glimpse(na_counts)

# Impute missing data in predictor variables
c_data <- c_data %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

na_counts <- c_data %>% summarize(across(everything(), ~ sum(is.na(.))))
glimpse(na_counts)

# Check data
glimpse(c_data)
colnames(c_data)

c_data <- c_data %>%
  rename(dummy_coast = coast)

# Histograms ---------------------------------------------------------

c_data [,1:9] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)

c_data [,10:18] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)

c_data [,19:27] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)

c_data [,28:36] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)

c_data [,37:39] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)


# Transformations ---------------------------------------------------------

# Looking at skewed variables and check transformation
# Transform if very skewed, or proportion data
# Most skewed variables: log-transform
# Proportion data: logit-transform

par(mfrow = c(1,2))

hist(c_data$holiday_homes)
hist(log(c_data$holiday_homes))
hist(sqrt(c_data$holiday_homes))

hist(c_data$future_hh_area)
hist(log(c_data$future_hh_area))
hist(sqrt(c_data$future_hh_area))
log(c_data$future_hh_area)


hist(c_data$land_area)
hist(log(c_data$land_area))
log(c_data$land_area)

hist(c_data$holiday_homes_1970)
hist(log(c_data$holiday_homes_1970))
log(c_data$holiday_homes_1970)

hist(c_data$hh_dwelling_ratio)
hist(log(c_data$hh_dwelling_ratio))
log(c_data$hh_dwelling_ratio)

hist(c_data$prop_montainous)
hist(logit(c_data$prop_montainous))

hist(c_data$prop_wetlands)
hist(logit(c_data$prop_wetlands))

hist(c_data$grazing_livestock)
hist(log(c_data$grazing_livestock))
log(c_data$grazing_livestock)

hist(c_data$pop_2hr_drive)
hist(log(c_data$pop_2hr_drive))

hist(c_data$pop_4hr_drive)
hist(log(c_data$pop_4hr_drive))
log(c_data$pop_4hr_drive)

hist(c_data$population)
hist(log(c_data$population))
log(c_data$population)

hist(c_data$prop_protected)
hist(logit(c_data$prop_protected))

hist(c_data$prop_private_sector)
hist(logit(c_data$prop_private_sector))

hist(c_data$education)
hist(logit(c_data$education))

hist(c_data$employment_rate)
hist(logit(c_data$employment_rate))

hist(c_data$urban_settlement_ratio)
hist(logit(c_data$urban_settlement_ratio))

hist(c_data$off_road_motor)
hist(log(c_data$off_road_motor+1))
log(c_data$off_road_motor+1)

hist(c_data$ski_lifts)
hist(log(c_data$ski_lifts+1))
log(c_data$ski_lifts+1)

hist(c_data$activity_data)
hist(log(c_data$activity_data))
log(c_data$activity_data)

hist(c_data$activity_pr_person)
hist(log(c_data$activity_pr_person))
log(c_data$activity_pr_person)

hist(c_data$hiker_cabins)
hist(log(c_data$hiker_cabins+1))
log(c_data$hiker_cabins+1)

hist(c_data$prop_farm_residents)
hist(logit(c_data$prop_farm_residents))

# Now prepare transformed variables based on the exploration

# Complete, transformed data = ct_data
ct_data <- c_data |> mutate(
	region = region, 
	municipality = municipality, 
	county = county, 
	dummy_coast = dummy_coast, 
	#hh_category = hh_category, 
	ln_land_area = log(land_area), 
	holiday_homes = holiday_homes, 
	future_hh_area = future_hh_area, 
	sqrt_future_hh_area = sqrt(future_hh_area), 
	ln_holiday_homes = log(holiday_homes), 
	ln_holiday_homes_1970 = log(holiday_homes_1970), 
	ln_hh_dwelling_ratio = log(hh_dwelling_ratio), 
	median_hh_price = median_hh_price, 
	prop_hh_area = car::logit(prop_hh_area), 
	prop_wetlands = car::logit(prop_wetlands), 
	prop_montainous = car::logit(prop_montainous), 
	altitudinal_range = altitudinal_range, 
	dist_to_coast = dist_to_coast, 
	veg_sections = veg_sections, 
	veg_zones = veg_zones, 
	prop_protected = car::logit(prop_protected), 
	ln_grazing_livestock = log(grazing_livestock), 
	infrastructure_index = infrastructure_index, 
	ln_population = log(population), 
	population_change = population_change, 
	median_age = median_age, 
	urban_settlement_ratio = car::logit(urban_settlement_ratio), 
	prop_farm_residents = car::logit(prop_farm_residents), 
	ln_pop_2hr_drive = log(pop_2hr_drive), 
	ln_pop_4hr_drive = log(pop_4hr_drive), 
	median_income = median_income, 
	education = car::logit(education), 
	employment_rate = car::logit(employment_rate), 
	prop_private_sector = car::logit(prop_private_sector), 
	municipal_economy = municipal_economy, 
	unrestricted_revenues = unrestricted_revenues, 
	ln_off_road_motor = off_road_motor, 
	ln_ski_lifts = log(ski_lifts+1), 
	ln_hiker_cabins = log(hiker_cabins+1), 
	ln_activity_data = log(activity_data), 
	ln_activity_pr_person = log(activity_pr_person), 
	culture_index = culture_index,
	.keep = "none")

# Coast as dummy variable
ct_data <- ct_data %>%
  mutate(dummy_coast = as.integer(dummy_coast == 1))

# Remove variables without variance
# ct_data <- ct_data |> dplyr::select(-hh_category)

# Z-score transformation --------------------------------------------------
glimpse(ct_data)

transform_recipe <- recipe( ~ ., data = ct_data) #CRN: package missing for this function

# Z-score-transformation (except outcome and dummy variables)
model_recipe_steps <- transform_recipe %>%
	update_role(region, new_role = "id") %>% 
  step_center(all_numeric(), -holiday_homes, -future_hh_area, -dummy_coast) %>%
  step_scale(all_numeric(), -holiday_homes, -future_hh_area, -dummy_coast) %>%
  # 6 Remove predictors that have zero variance or are nearly constant
  step_nzv(all_predictors()) |> 
	prep()

# # fit recipe to data and preprocess
# z-score transformed and skewness-transformed data:
# zs_data
zs_data <- bake(model_recipe_steps, new_data = NULL)
zs_data
glimpse(zs_data)

zs_data [,1:19] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)

zs_data [,25:32] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)

data_zs

# Only z-score-transformed data

transform_recipe <- recipe( ~ ., data = c_data)

# only Z-score-transformation (except outcome and dummy variables)
model_recipe_steps <- transform_recipe %>%
	update_role(region, new_role = "id") %>% 
  step_center(all_numeric(), -holiday_homes, -future_hh_area, -dummy_coast) %>%
  step_scale(all_numeric(), -holiday_homes, -future_hh_area, -dummy_coast) %>%
  # 6 Remove predictors that have zero variance or are nearly constant
  step_nzv(all_predictors()) |> 
	prep()

# # fit recipe to data and preprocess
# z-score transformed data:
# z_data
z_data <- bake(model_recipe_steps, new_data = NULL)
z_data
glimpse(z_data)

z_data [,1:9] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)

z_data [,30:38] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)

z_data

# Now we have three datasets for furter modeling
# 1) c_data, complete data, without missing data
# 2) z_data, where the response variables (except dummy variables), are transformed to z-scores
# 3) zs_data, where some of the response variables are  skewness-transformed (log), 
# proportions are logit-transformed, and all responses finally transformed to z-scores


# Data for mountain municipalities
write.csv(c_data, file = "C:/Users/trond.simensen/OneDrive - NINA/Documents/R/holiday_homes/mountain_data.csv", row.names = FALSE)
write.csv(z_data, file = "C:/Users/trond.simensen/OneDrive - NINA/Documents/R/holiday_homes/z_mountain_data.csv", row.names = FALSE)
write.csv(zs_data, file = "C:/Users/trond.simensen/OneDrive - NINA/Documents/R/holiday_homes/zs_mountain_data.csv", row.names = FALSE)


# Exploring ---------------------------------------------------------------

c_data [,1:9] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)

zs_data [,20:30] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)

# Checking which distribution is the best ---------------------------------

library(fitdistrplus)
library(logspline)

library(gamlss)
library(gamlss.dist)
library(gamlss.add)

# Choose variable at stake (most important - the response)
x <- as.numeric(na.omit(c_data$holiday_homes))
par(mfrow=c(1,1))
summary(x)
hist(x)
log_x <- log(x)
hist(log_x)
sqrt_x <- sqrt(x)
hist(sqrt_x)

par(mfrow=c(1,1))

descdist(x, discrete = FALSE)
descdist(x, boot = 1000)

fit.norm <- fitdist(x, "norm")
plot(fit.norm)

fit.lognormal <- fitdist(x, "lnorm")
plot(fit.lognormal)

fit.weibull <- fitdist(x, "weibull")
plot(fit.weibull)

fit.gamma <- fitdist(x, "gamma")
plot(fit.gamma)

empirical <- ecdf(x)
plot(empirical, xlab="x variable", ylab="probability", datacol="black")
cdfcomp(fit.norm, datacol="gray", fitcol="blue", add = TRUE)
cdfcomp(fit.lognormal, datacol="gray", fitcol="blue", add = TRUE)
cdfcomp(fit.weibull, datacol="navy", fitcol="red", add = TRUE)
cdfcomp(fit.gamma, datacol="navy", fitcol="blue", add = TRUE)

fit.norm$aic
fit.lognormal$aic
fit.weibull$aic
fit.gamma$aic



# Automatic distribution fitting with GAMLSS
# The gamlss package for R offers the ability to try many different 
# distributions and select the "best" according to the GAIC (the generalized 
# Akaike information criterion). 

fit <- fitDist(x+1, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
summary(fit)
fit$fits

# Choose variable at stake (most important - the response)
x <- as.numeric(na.omit(c_data$future_hh_area))
par(mfrow=c(1,1))
summary(x)
hist(x)
log_x <- log(x)
hist(log_x)
sqrt_x <- sqrt(x)
hist(sqrt_x)

par(mfrow=c(1,1))

descdist(x, discrete = FALSE)
descdist(x, boot = 1000)

fit.norm <- fitdist(x, "norm")
plot(fit.norm)

fit.lognormal <- fitdist(x, "lnorm")
plot(fit.lognormal)

fit.weibull <- fitdist(x, "weibull")
plot(fit.weibull)

fit.gamma <- fitdist(x, "gamma")
plot(fit.gamma)

empirical <- ecdf(x)
plot(empirical, xlab="x variable", ylab="probability", datacol="black")
cdfcomp(fit.norm, datacol="gray", fitcol="blue", add = TRUE)
cdfcomp(fit.lognormal, datacol="gray", fitcol="blue", add = TRUE)
cdfcomp(fit.weibull, datacol="navy", fitcol="red", add = TRUE)
cdfcomp(fit.gamma, datacol="navy", fitcol="blue", add = TRUE)

fit.norm$aic
fit.lognormal$aic
fit.weibull$aic
fit.gamma$aic



# Automatic distribution fitting with GAMLSS
# The gamlss package for R offers the ability to try many different 
# distributions and select the "best" according to the GAIC (the generalized 
# Akaike information criterion). 

fit <- fitDist(x+1, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
summary(fit)
fit$fits
