library(tidyverse) # data wrangling
library(readr) # read csv
library(tidymodels) # transformations, etc
library(car) #logit-transformations
library(corrplot) # correlation plots
library(summarytools) # summary of dataframes
library(kableExtra) # to make tables
library(corrr)
library(tidytext)
library(dotwhisker)
library(gridExtra)
library(ggpubr)
library(sf)
#library(tmap)
library(factoextra)#derive results from prcomp, pca
#theme_set(theme_minimal())

Sys.setlocale("LC_CTYPE", "norwegian")

# Reading in data
data <- read.csv("data/holiday_home_data.csv", 
	header = TRUE, sep = ",", encoding = "latin") |> as_tibble()

# Recoding variables and variable types
data$region <- as.character(data$region)
data$region[data$region == "301"] <- "0301"
glimpse(data)

data$hh_category = as.factor(data$hh_category)
levels(data$hh_category) <- c("Coastal", "Forest", "Mountain")
data$hh_category

# Keeping copy of original data
data_orig <- data

# Use the summarize() and across() functions to count NAs for each column
na_counts <- data %>% summarize(across(everything(), ~ sum(is.na(.))))
glimpse(na_counts)

# remove municipality with errors or missing data in one of the two reponse variables
data <- data |> filter(municipality != "Rana", municipality != "Snåase - Snåsa")

# Summary statistics
data
glimpse(data)
glimpse(na_counts)

# Create table with summary statistics ------------------------------------

# Assign the input data frame to summary_data_frame
summary_data_frame <- data

# List of variables to put on top
variables_on_top <- c("holiday_homes", "future_hh_area")

# List of categorical variables
categorical_variables <- c("municipality", "county", "coast", "hh_category")

# Function to compute summary statistics
compute_summary <- function(data, var_name) {
  if (var_name %in% categorical_variables) {
    categories <- n_distinct(data[[var_name]])
  } else {
    categories <- NA
  }
  na_counts <- sum(is.na(data[[var_name]]))
  min_val <- ifelse(is.na(categories), round(min(data[[var_name]], na.rm = TRUE), 2), NA)
  max_val <- ifelse(is.na(categories), round(max(data[[var_name]], na.rm = TRUE), 2), NA)
  mean_val <- ifelse(is.na(categories), round(mean(data[[var_name]], na.rm = TRUE), 2), NA)
  sd_val <- ifelse(is.na(categories), round(sd(data[[var_name]], na.rm = TRUE), 2), NA)
  return(tibble(variable = var_name, categories = categories, na_counts = na_counts, min = min_val, max = max_val, mean = mean_val, sd = sd_val))
}

# List of other variables excluding 'region' and the ones to put on top
other_variables <- setdiff(names(summary_data_frame), c("region", variables_on_top))

# Compute summary statistics for 'variables_on_top'
summary_stats_top <- bind_rows(lapply(variables_on_top, compute_summary, data = summary_data_frame))

# Compute summary statistics for other variables
summary_stats_other <- bind_rows(lapply(other_variables, compute_summary, data = summary_data_frame))

# Combine the two tibbles
summary_table <- bind_rows(summary_stats_top, summary_stats_other)

# Save to CSV
#write.csv(summary_table, "summary_statistics.csv", row.names = FALSE)

# Print the summary table
print(summary_table)
glimpse(summary_table)

# Assign the input data frame to summary_data_frame
summary_data_frame <- data

# List of variables to put on top
variables_on_top <- c("holiday_homes", "future_hh_area")

# List of categorical variables
categorical_variables <- c("municipality", "county", "coast", "hh_category")

# Function to compute summary statistics
compute_summary <- function(data, var_name) {
  if (var_name %in% categorical_variables) {
    categories <- n_distinct(data[[var_name]])
  } else {
    categories <- NA
  }
  na_counts <- sum(is.na(data[[var_name]]))
  min_val <- ifelse(is.na(categories), round(min(data[[var_name]], na.rm = TRUE), 2), NA)
  max_val <- ifelse(is.na(categories), round(max(data[[var_name]], na.rm = TRUE), 2), NA)
  mean_val <- ifelse(is.na(categories), round(mean(data[[var_name]], na.rm = TRUE), 2), NA)
  sd_val <- ifelse(is.na(categories), round(sd(data[[var_name]], na.rm = TRUE), 2), NA)
  return(tibble(variable = var_name, categories = categories, na_counts = na_counts, min = min_val, max = max_val, mean = mean_val, sd = sd_val))
}

# List of other variables excluding 'region' and the ones to put on top
other_variables <- setdiff(names(summary_data_frame), c("region", variables_on_top))

# Compute summary statistics for 'variables_on_top'
summary_stats_top <- bind_rows(lapply(variables_on_top, compute_summary, data = summary_data_frame))

# Compute summary statistics for other variables
summary_stats_other <- bind_rows(lapply(other_variables, compute_summary, data = summary_data_frame))

# Combine the two tibbles
summary_table <- bind_rows(summary_stats_top, summary_stats_other)
summary_table <- summary_table |> dplyr::rename(Abbreviation = variable, Categories = categories, NA_count = na_counts, 
  Min = min, Max = max, Mean = mean, SD = sd)

summary_table

# Read in variable descriptions
variable_description <- readr::read_delim("data/holiday_home_data_short_variable_description.csv", 
    delim = ",", escape_double = FALSE, trim_ws = TRUE)
View(variable_description)

summary_table_1 <- dplyr::left_join(variable_description, summary_table, by = "Abbreviation")
View(summary_table_1)

# Filter out rows with specific Abbreviations
filtered_table <- summary_table_1 %>%
  filter(!(Abbreviation %in% c("region", "municipality", "county", "coast", "hh_category")))

# Reordering the rows by first removing the row with Abbreviation "land_area"
# and then adding it back after the row with Abbreviation "prop_hh_area"
land_area_row <- filtered_table %>%
  filter(Abbreviation == "land_area")

without_land_area <- filtered_table %>%
  filter(Abbreviation != "land_area")

# Finding the index where Abbreviation is "prop_hh_area"
index_prop_hh_area <- which(without_land_area$Abbreviation == "prop_hh_area")

# Adding the land_area_row back in the desired position
reordered_table <- without_land_area %>%
  add_row(land_area_row, .before = index_prop_hh_area + 1)

# Removing the "Categories" column
final_table <- select(reordered_table, -Categories)

# The final_table is the filtered, reordered table without the "Categories" column
View(final_table)


# Save to CSV
write.csv(final_table, "tables/summary_statistics.csv", row.names = FALSE)

# Print the summary table
print(summary_table)
glimpse(summary_table)

# Correlation matrix ------------------------------------------------------
# Omitting non-numerical variables
data_num <- data |> dplyr::select(-hh_category, -holiday_homes, -future_hh_area,
	-region, -municipality, -county, -coast) |> na.omit() 

correlations <- round(cor(data_num, method = "kendall"),2)
correlations
corrplot(correlations, method = 'number', order = "hclust")
write.csv(correlations, file = "tables/correlations_norway.csv", row.names = FALSE)

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
write.csv(summary_cor_with_response, file = "tables/correlations_with_response_variables_norway.csv", row.names = FALSE)


