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
#View(variable_description)

summary_table_1 <- dplyr::left_join(variable_description, summary_table, by = "Abbreviation")
#View(summary_table_1)

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
# View(final_table)


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

pdf(file = "tables/correlations_all_variables_Norway.pdf", width = 16.5, height = 11.7)
corrplot(correlations, method = 'number', order = "hclust", type = "lower", 
title = "Kendall Correlation Matrix, all variables", 
mar = c(0,0,1,0), number.cex = 0.5, number.digits = 2, tl.cex = 0.7)

dev.off()

write.csv(correlations, file = "tables/correlation_all_variables_norway.csv", row.names = FALSE)

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


# Multivariate analysis ---------------------------------------------------

# PCA



# Base R PCA
data_num <- data |> dplyr::select(-region, -municipality, -county, -coast, -hh_category)
hytter_clean <- na.omit(data_num)
#hytter_prcomp <- prcomp(hytter_clean[,-(1:3)], scale = TRUE)
hytter_prcomp <- prcomp(hytter_clean, scale = TRUE)
load <- hytter_prcomp$x

summary(hytter_prcomp)

screeplot(hytter_prcomp)
biplot(hytter_prcomp)

var <- factoextra::get_pca_var(hytter_prcomp)
var

head(var$cor)
pca_cor <- var$cor
pca_cor
cor_pc1 <- pca_cor


# Tidyverse PCA

data$coast <- as.factor(data$coast)

glimpse(data)
#data$sentralitetsindeks <- as.factor(data$sentralitet)

hytter_recipe <-
  recipe(~., data = data) %>% 
  update_role(region, municipality, county, coast, hh_category, new_role = "id") %>% 
	#step_impute_mean(all_numeric()) |> 
	# 1 impute or omit na
	step_naomit(all_numeric()) %>% 
	# mean impute numeric variables
  # step_impute_mean(all_numeric()) |> 
	# 2 Handle factor levels
  # convert specific variable(s) to dummy variables
  #step_dummy(kyst_f) %>%
	# 3 Individual transformations for skewness and other issues
	step_YeoJohnson(all_numeric()) |> 
	# 4 Create interactions
	# 5 Normalization steps (center, scale, range, etc)
  step_center(all_numeric()) |>
	step_scale(all_numeric()) |>
	#step_normalize(all_numeric()) %>%
	# rescale all numeric variables except ..., ... and ... to lie between 0 and 1
  # step_range(all_numeric(), min = 0, max = 1, -vanilla, -salt, -baking_powder) %>%
	#step_range(all_numeric(), min = 0, max = 1) %>%
	# remove predictor variables that are almost the same for every entry
  step_nzv(all_numeric()) |> 
  step_pca(all_numeric(), id = "pca") %>% 
  prep() # CRN: package for function "prep" missing?

#data_t <- data |> select(-region, -municipality, -county, -kyst_f, -hyttekomtype, -KDP_natur)
data_t <- data |> dplyr::select(where(is.numeric)) 

hytter_recipe2 <-
  recipe(~., data = data_t) |> 
	#update_role(region, municipality, county, new_role = "id") %>% 
	step_naomit(all_numeric()) |> 
	step_YeoJohnson(all_numeric()) |> 
	step_normalize(all_numeric())  |> 
  step_nzv(all_predictors()) |> 
	prep()

# fit recipe to data and preprocess
preprocessed_data <- bake(hytter_recipe2, new_data = NULL)

# perform PCA on preprocessed data
pca_results <- prcomp(preprocessed_data, scale. = FALSE)

# ind_pca <- factoextra::get_pca_ind(pca_results)
# ind_pca
# ind_pca$coord
# ind <- ind_pca$coord |> as_tibble()
# 
# pca_cor[,1:4]
# cor(data_num$fribygg, ind$Dim.1)
# cor(data_num$fribygg, ind$Dim.2)
# cor(data_num$fribygg, ind$Dim.1, method = "kendall")
# cor.test(data_num$fribygg, ind$Dim.2, method = "kendall")

# extract loadings for variables #factoextra-package
var_pca <- get_pca_var(pca_results)
var <- var_pca$coord
var <- var[,1:4]

# extract correlations between variables and PC axes
#pca_cor <- cor(preprocessed_data, ind, use = "pairwise.complete.obs", method = "kendall") 

# extract correlations between variables and PC axes
pca_cor <- cor(preprocessed_data, pca_results$x, use = "pairwise.complete.obs", method = "kendall")

# create separate data frame for axis 1
pca_cor_axis1 <- data.frame(variable = colnames(preprocessed_data), cor_coef = pca_cor[,1], p_value = rep(NA, ncol(preprocessed_data)))

# # calculate p-values for axis 1
# for(i in 1:ncol(preprocessed_data)) {
#   pca_cor_axis1$p_value[i] <- cor.test(pca_results$x[,1], preprocessed_data[,i], method = "kendall")$p.value
# }


# create empty data frame for axis 1 correlations and p-values
pca_cor_axis1 <- data.frame(variable = colnames(preprocessed_data),
                            cor = numeric(ncol(preprocessed_data)),
                            p_value = numeric(ncol(preprocessed_data)),
                            stringsAsFactors = FALSE)

# calculate correlations and p-values for axis 1
for (i in seq_along(pca_cor_axis1$variable)) {
  # calculate correlation and p-value
  cor_res <- cor.test(pca_results$x[, 1], as.matrix(preprocessed_data)[, i], method = "kendall")
  # store results in data frame
  pca_cor_axis1$cor[i] <- cor_res$estimate
  pca_cor_axis1$p_value[i] <- cor_res$p.value
}

# sort by absolute correlation values (descending)
pca_cor_axis1 <- pca_cor_axis1[order(abs(pca_cor_axis1$cor), decreasing = TRUE), ]

# print the results
pca_cor_axis1 |> slice(1:10)


# Axis two ----------------------------------------------------------------

# create empty data frame for axis 2 correlations and p-values
pca_cor_axis2 <- data.frame(variable = colnames(preprocessed_data),
                            cor = numeric(ncol(preprocessed_data)),
                            p_value = numeric(ncol(preprocessed_data)),
                            stringsAsFactors = FALSE)

# calculate correlations and p-values for axis 2
for (i in seq_along(pca_cor_axis2$variable)) {
  # calculate correlation and p-value
  cor_res <- cor.test(pca_results$x[, 2], as.matrix(preprocessed_data)[, i], method = "kendall")
  # store results in data frame
  pca_cor_axis2$cor[i] <- cor_res$estimate
  pca_cor_axis2$p_value[i] <- cor_res$p.value
}

# sort by absolute correlation values (descending)
pca_cor_axis2 <- pca_cor_axis2[order(abs(pca_cor_axis2$cor), decreasing = TRUE), ]

# print the results
pca_cor_axis2

# sort by absolute correlation values (descending)
pca_cor_axis2 <- pca_cor_axis2[order(abs(pca_cor_axis2$cor), decreasing = TRUE), ]
pca_cor_axis2 

# print the results
pca_cor_axis2 |> slice(1:10)

# Axis three ----------------------------------------------------------------

# create empty data frame for axis 3 correlations and p-values
pca_cor_axis3 <- data.frame(variable = colnames(preprocessed_data),
                            cor = numeric(ncol(preprocessed_data)),
                            p_value = numeric(ncol(preprocessed_data)),
                            stringsAsFactors = FALSE)

# calculate correlations and p-values for axis 3
for (i in seq_along(pca_cor_axis3$variable)) {
  # calculate correlation and p-value
  cor_res <- cor.test(pca_results$x[, 3], as.matrix(preprocessed_data)[, i], method = "kendall")
  # store results in data frame
  pca_cor_axis3$cor[i] <- cor_res$estimate
  pca_cor_axis3$p_value[i] <- cor_res$p.value
}

# sort by absolute correlation values (descending)
pca_cor_axis3 <- pca_cor_axis3[order(abs(pca_cor_axis3$cor), decreasing = TRUE), ]

# print the results
pca_cor_axis3

# sort by absolute correlation values (descending)
pca_cor_axis3 <- pca_cor_axis3[order(abs(pca_cor_axis3$cor), decreasing = TRUE), ]
pca_cor_axis3 

# print the top 10 results
pca_cor_axis3 |> slice(1:10)

# Axis four ----------------------------------------------------------------

# create empty data frame for axis 4 correlations and p-values
pca_cor_axis4 <- data.frame(variable = colnames(preprocessed_data),
                            cor = numeric(ncol(preprocessed_data)),
                            p_value = numeric(ncol(preprocessed_data)),
                            stringsAsFactors = FALSE)

# calculate correlations and p-values for axis 4
for (i in seq_along(pca_cor_axis4$variable)) {
  # calculate correlation and p-value
  cor_res <- cor.test(pca_results$x[, 4], as.matrix(preprocessed_data)[, i], method = "kendall")
  # store results in data frame
  pca_cor_axis4$cor[i] <- cor_res$estimate
  pca_cor_axis4$p_value[i] <- cor_res$p.value
}

# sort by absolute correlation values (descending)
pca_cor_axis4 <- pca_cor_axis4[order(abs(pca_cor_axis4$cor), decreasing = TRUE), ]

# print the results
pca_cor_axis4

# sort by absolute correlation values (descending)
pca_cor_axis4 <- pca_cor_axis4[order(abs(pca_cor_axis4$cor), decreasing = TRUE), ]
pca_cor_axis4 

# print the top 10 results
pca_cor_axis4 |> slice(1:10)

pca_cor_axis1

# Make one table with output for first four axes

# Function to format p-values
format_p_value <- function(p_value) {
  ifelse(p_value < 0.001, "<0.001", round(p_value, 3))
}

# Format p-values
pca_cor_axis1$p_value <- sapply(pca_cor_axis1$p_value, format_p_value)
pca_cor_axis2$p_value <- sapply(pca_cor_axis2$p_value, format_p_value)
pca_cor_axis3$p_value <- sapply(pca_cor_axis3$p_value, format_p_value)
pca_cor_axis4$p_value <- sapply(pca_cor_axis4$p_value, format_p_value)

# Round correlations
pca_cor_axis1$cor <- round(pca_cor_axis1$cor, 3)
pca_cor_axis2$cor <- round(pca_cor_axis2$cor, 3)
pca_cor_axis3$cor <- round(pca_cor_axis3$cor, 3)
pca_cor_axis4$cor <- round(pca_cor_axis4$cor, 3)

# Rename columns before joining
colnames(pca_cor_axis1) <- c("variable", "cor_pca1", "p_value1")
colnames(pca_cor_axis2) <- c("variable", "cor_pca2", "p_value2")
colnames(pca_cor_axis3) <- c("variable", "cor_pca3", "p_value3")
colnames(pca_cor_axis4) <- c("variable", "cor_pca4", "p_value4")

# Merge all the results
final_result_pca <- full_join(pca_cor_axis1, pca_cor_axis2, by = "variable") %>%
  full_join(., pca_cor_axis3, by = "variable") %>%
  full_join(., pca_cor_axis4, by = "variable") %>%
  arrange(as.numeric(row.names(pca_cor_axis1)))

# Print the results
final_result_pca

# Save to CSV
write.csv(final_result_pca, "tables/cor_pca_axes_1_2_3_4_holiday_homes_norway.csv", row.names = FALSE)

# Key outputs of the PCA --------------------------------------------------

# create a scree plot of the eigenvalues
fviz_eig(pca_results, addlabels = TRUE)

# calculate the total variance explained by each principal component
var_exp <- pca_results$sdev^2 / sum(pca_results$sdev^2)
var_exp_df <- data.frame(PC = paste0("PC", 1:length(var_exp)), 
                          Variance.Explained = var_exp,
                          Cumulative.Variance.Explained = cumsum(var_exp))

# print the table of variance explained
print(var_exp_df, row.names = FALSE)

var_exp_pca1 <- round(var_exp_df [1,2]*100, 1)
var_exp_pca1

var_exp_pca2 <- round(var_exp_df [2,2]*100, 1)
var_exp_pca2


# plot the cumulative variance explained
ggplot(var_exp_df [1:9,], aes(x = PC, y = Cumulative.Variance.Explained)) + 
  geom_line() + 
  geom_point() + 
  #scale_y_continuous(labels = scales::percent_format()) +
  ylim(0,1)+
  labs(title = "Cumulative Variance Explained",
       x = "Principal Component",
       y = "Cumulative Variance Explained")


# Continue with plotting, etc ---------------------------------------------

hytter_pca <- 
  hytter_recipe %>% 
  tidy(id = "pca") 

hytter_recipe$term_info
hytter_pca

pca_summary <- hytter_recipe %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "percent variance") 

pca_summary

hytter_recipe %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  #geom_col(fill = "#b6dfe2") + 
	geom_col(fill = "#56B4E9", alpha = 0.8) +
  xlim(c(0, 10)) + 
  ylab("% of total variance")+
	theme_minimal()



# Selecting the first four axes
hytter_pc1_pc2_pc3_pc4 <-  hytter_pca |> filter(component == "PC1"|component == "PC2"|component == "PC3"|component == "PC4")

# plot these loadings by principal component
hytter_pc1_pc2_pc3_pc4 %>%
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  ) 



# Open PDF device
pdf("plots/PCA_loadings_Holiday_Homes_in_Norway.pdf", width = 8.27, height = 11.69) # A4 size in inches

# plot these loadings by principal component

hytter_pc1_pc2_pc3_pc4 %>%
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL,
    fill = "Positive?",
    title = "Loadings by principal component 1-4\nHoliday homes in Norway"
  ) 


# Close PDF device
dev.off()




# Selecting the first four axes
hytter_pc1_pc2 <-  hytter_pca |> filter(component == "PC1"|component == "PC2")

# plot these loadings by principal component
hytter_pc1_pc2 %>%
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  ) 

hytter_pc3_pc4 <-  hytter_pca |> filter(component == "PC3"|component == "PC4")

# plot these loadings by principal component
hytter_pc3_pc4 %>%
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  ) 


# get pca loadings into wider format
pca_wider <- hytter_pca %>% 
  tidyr::pivot_wider(names_from = component, id_cols = terms)

pca_wider
glimpse(pca_wider)

# Save to CSV
write.csv(pca_wider, "tables/pca__loadings_holiday_homes_norway.csv", row.names = FALSE)


hytter_pca

# # sort by absolute contribution values (descending)
# hytter_pc1_pc2_pc3_pc4 <- hytter_pc1_pc2_pc3_pc4 %>%
#   arrange(desc(abs(value))) %>%
#   select(terms, value, component)
# 
# hytter_pc1_pc2_pc3_pc4
# 
# hytter_pc1_pc2_pc3_pc4_wide <- hytter_pc1_pc2_pc3_pc4 %>%
#   pivot_wider(names_from = "component", values_from = "value") |> 
# 	select(terms, PC1, PC2, PC3, PC4)
# 
# # print the results
# hytter_pc1_pc2_pc3_pc4_wide
# 
# # sort by absolute contribution values (descending)
# hytter_pc1_pc2_pc3_pc4 <- hytter_pc1_pc2_pc3_pc4 %>%
#   arrange(desc(abs(value)))
# 
# # pivot the data frame into wide format
# hytter_pc1_pc2_pc3_pc4_wide <- hytter_pc1_pc2_pc3_pc4 %>%
#   pivot_wider(names_from = "component", values_from = "value")
# 
# # print the results
# hytter_pc1_pc2_pc3_pc4_wide
# 
# 
# 
# # print the results
# hytter_pc1_pc2_pc3_pc4

arrows_all <- pca_wider[2:36,]
arrows_holiday_homes <- pca_wider[2:8,]
arrows_geography <- pca_wider[9:18,]
arrows_socioeconomic <- pca_wider[18:30,]
arrows_outdoor <- pca_wider[31:36,]

pca_wider[1,]

arrows <- pca_wider [2:58,] #|> filter(terms == "fribygg")


hytter_recipe

# PCA biplot

juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2, label = municipality)) +
  geom_point(aes(color = county), alpha = 0.7, size = 2) +
  geom_text(check_overlap = FALSE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)+
	  theme_minimal()

cbp2 <- c("#ca0020", "#f4a582", "#92c5de", "#0571b0")

#Three colours
cbp1 = c("#0072B2", "#009E73", "#E69F00")
# cbp1 = c("#0072B2", "#E69F00", "red")

juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2, label = municipality)) +
  geom_point(aes(PC1, PC2, colour = county), alpha = 0.7, size = 2) +
	#scale_colour_manual(values=cbp1)+
  #geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)+
	  theme_minimal()

juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2, label = municipality)) +
  geom_point(aes(PC1, PC2, colour = hh_category), alpha = 0.7, size = 2) +
	#scale_colour_manual(values=cbp1)+
  #geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)+
	  theme_minimal()

juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = hh_category), size = 2, alpha = 0.9) +
  geom_segment(data = arrows_all,
               aes(x = 0, y = 0, xend = PC1 * 20, yend = PC2 * 20), # control arrow length
               arrow = arrow(length = unit(1/2, "picas")),
               color = "blue") +
  annotate("text",
           x = arrows_all$PC1 * 20.2, # control placement of text
           y = arrows_all$PC2 * 20.2, # control placement of text
           label = arrows_all$terms) +
	#geom_text(aes(PC1, PC2, label = municipality), colour = "grey", check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  theme_minimal() +
  xlab("PC1 (nn %)") +
  ylab("PC2 (nn %)")

juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = hh_category), size = 2, alpha = 0.9) +
  geom_segment(data = arrows_holiday_homes,
               aes(x = 0, y = 0, xend = PC1 * 20, yend = PC2 * 20), # control arrow length
               arrow = arrow(length = unit(1/2, "picas")),
               color = "blue") +
  annotate("text",
           x = arrows_holiday_homes$PC1 * 20.2, # control placement of text
           y = arrows_holiday_homes$PC2 * 20.2, # control placement of text
           label = arrows_holiday_homes$terms) +
	#geom_text(aes(PC1, PC2, label = municipality), colour = "grey", check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  theme_minimal() +
  xlab("PC1 (nn %)") +
  ylab("PC2 (nn %)")

juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = hh_category), size = 2, alpha = 0.6) +
  geom_segment(data = arrows_geography,
               aes(x = 0, y = 0, xend = PC1 * 20, yend = PC2 * 20), # control arrow length
               arrow = arrow(length = unit(1/2, "picas")),
               color = "blue") +
  annotate("text",
           x = arrows_geography$PC1 * 20.2, # control placement of text
           y = arrows_geography$PC2 * 20.2, # control placement of text
           label = arrows_geography$terms) +
  theme_minimal() +
  xlab("PC1 (nn %)") +
  ylab("PC2 (nn %)")


juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = county), size = 2, alpha = 0.6) +
  geom_segment(data = arrows_geography,
               aes(x = 0, y = 0, xend = PC1 * 20, yend = PC2 * 20), # control arrow length
               arrow = arrow(length = unit(1/2, "picas")),
               color = "blue") +
  annotate("text",
           x = arrows_geography$PC1 * 20.2, # control placement of text
           y = arrows_geography$PC2 * 20.2, # control placement of text
           label = arrows_geography$terms) +
  theme_minimal() +
  xlab("PC1 (nn %)") +
  ylab("PC2 (nn %)")


juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = county), size = 2, alpha = 0.6) +
  geom_segment(data = arrows_socioeconomic,
               aes(x = 0, y = 0, xend = PC1 * 20, yend = PC2 * 20), # control arrow length
               arrow = arrow(length = unit(1/2, "picas")),
               color = "blue") +
  annotate("text",
           x = arrows_socioeconomic$PC1 * 20.2, # control placement of text
           y = arrows_socioeconomic$PC2 * 20.2, # control placement of text
           label = arrows_socioeconomic$terms) +
  theme_minimal() +
  xlab("PC1 (nn %)") +
  ylab("PC2 (nn %)")

juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = county), size = 2, alpha = 0.6) +
  geom_segment(data = arrows_outdoor,
               aes(x = 0, y = 0, xend = PC1 * 20, yend = PC2 * 20), # control arrow length
               arrow = arrow(length = unit(1/2, "picas")),
               color = "blue") +
  annotate("text",
           x = arrows_outdoor$PC1 * 20.2, # control placement of text
           y = arrows_outdoor$PC2 * 20.2, # control placement of text
           label = arrows_outdoor$terms) +
  theme_minimal() +
  xlab("PC1 (nn %)") +
  ylab("PC2 (nn %)")



pca_loadings <- pca_wider |> dplyr::select(terms, PC1, PC2, PC3, PC4)
pca_loadings


arrows_all <- pca_wider[2:30,]


# Publication ready plots -------------------------------------------------

pca_cor_axis1 |> slice(1:10)

pca_cor_axis2 |> slice(1:10)

arrows <- pca_wider |> filter(terms == "holiday_homes")
arrows <- pca_wider |> 
  filter(terms %in% c(
  #"population_change",
  "infrastructure_index",
  "unrestricted_revenues",
  "population",
  "activity_data",
  "veg_zones",
  #"urban_settlement_ratio",
  "prop_farm_residents",
  "median_age",
  "pop_2hr_drive",
  "pca_cor_axis2",
  "variable",
  "holiday_homes",
  "holiday_homes_1970",
  "activity_pr_person",
  "hiker_cabins",
  "ski_lifts",
  "dist_to_coast",
  "land_area",
  "future_hh_area",
  "altitudinal_range"))

arr <- pca_wider |> 
  filter(terms %in% c(#"population_change",
"infrastructure_index",
"unrestricted_revenues",
"population",
"activity_data",
"veg_zones",
#"urban_settlement_ratio",
"prop_farm_residents",
"median_age",
"pop_2hr_drive",
"pca_cor_axis2",
"variable",
"holiday_homes",
"holiday_homes_1970",
"activity_pr_person",
#"hiker_cabins",
"ski_lifts",
"dist_to_coast",
#"land_area",
"future_hh_area",
"altitudinal_range"))


# create a new column with the updated terms
arrows_updated <- arr %>%
  mutate(terms_updated = case_when(
    terms == "population_change" ~  "Population growth",
    terms == "infrastructure_index" ~  "Infrastructure",
    terms == "unrestricted_revenues" ~  "Revenues",
    terms == "population" ~  "Population",
    terms == "activity_data" ~  "Activity",
    terms == "veg_zones" ~  "Lowland vegetation",
    #terms == "urban_settlement_ratio" ~  "Urban",
    terms == "prop_farm_residents" ~  "Farm residents",
    terms == "median_age" ~  "Median age",
    terms == "pop_2hr_drive" ~  "Pop. 2hr drive",
    terms == "holiday_homes" ~  "Holiday homes",
    terms == "holiday_homes_1970" ~  "HH 1970",
    terms == "activity_pr_person" ~  "Activity pr person",
    #terms == "hiker_cabins" ~  "Hiker cabins",
    terms == "ski_lifts" ~  "Ski lifts",
    terms == "dist_to_coast" ~  "Inland",
    #terms == "land_area" ~  "Land area",
    terms == "future_hh_area" ~  "Future HH area",
    terms == "altitudinal_range" ~  "Altitudinal range",
    TRUE ~ as.character(terms)
  ))

cbp1 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", 
	"#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")

#Three colours
cbp1 = c("#0072B2", "#009E73", "#E69F00")
cbp1 = c("#0072B2", "#009E73", "red")

var_exp_pca1
var_exp_pca2

# plot the updated arrows
juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2)) +
	geom_vline(xintercept = 0, linetype="dashed", color = "darkgrey")+
	geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  geom_point(aes(color = hh_category, shape=hh_category), size = 2, alpha = 0.8) +
  geom_point(aes(color = hh_category), size = 2, alpha = 0.6) +
  scale_colour_manual(values=cbp1) +
  scale_shape_manual(values = c(16, 15, 17)) + 
  geom_segment(data = arrows_updated,
               aes(x = 0, y = 0, xend = PC1 * 20, yend = PC2 * 20), # control arrow length
               arrow = arrow(length = unit(1/2, "picas")),
               color = "blue") +
  annotate("text",
           x = arrows_updated$PC1 * 21.2, # control placement of text
           y = arrows_updated$PC2 * 21.2, # control placement of text
           label = arrows_updated$terms_updated) +
  theme_minimal() +
  xlab("Principal component 1 (28.1 % variation explained)") +
  ylab("Principal component 2 (15.2 % variation explained)")

fig_pca <- juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_point(aes(color = hh_category, shape = hh_category), size = 2.5, alpha = 1) +
  scale_colour_manual(values = cbp1) +
  scale_shape_manual(values = c(16, 15, 17)) +
  geom_segment(data = arrows_updated,
               aes(x = 0, y = 0, xend = PC1 * 16, yend = PC2 * 16),
               arrow = arrow(length = unit(1/2, "picas")),
               color = "grey39") +
  annotate("text",
           x = arrows_updated$PC1 * 17,
           y = arrows_updated$PC2 * 17,
           label = arrows_updated$terms_updated, size = 4) +
  labs(fill = "New Legend Title") +  # Change the legend title
  theme_bw()+
  xlab("Principal component 1 (28.1 % variation explained)") +
  ylab("Principal component 2 (15.2 % variation explained)") +
  theme(axis.title = element_text(size = 1.1 * rel(1)),
        legend.title = element_blank(),
        legend.text = element_text(size = 1.1 * rel(1)))  # Increase the size of legend text

fig_pca


ggsave("plots/fig_pca.png", 
	fig_pca, scale = 2, bg = "white", width = 15, height = 10, units = "cm", dpi = 600)


fig_s_pca_with_names <- juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2, label = municipality)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_point(aes(fill = county), shape = 21, color = "grey30", size = 3, alpha = 1) +
  scale_fill_brewer(palette = "Paired") + # Using "Paired" palette from RColorBrewer
  geom_text(color = "grey1", check_overlap = TRUE,  vjust = "bottom", nudge_y = 0.1, family = "IBMPlexSans") +
  labs(fill = "New Legend Title") +  # Change the legend title
  theme_bw() +
  xlab("Principal component 1 (28.1 % variation explained)") +
  ylab("Principal component 2 (15.2 % variation explained)") +
  theme(axis.title = element_text(size = 1.1 * rel(1)),
        legend.title = element_blank(),
        legend.text = element_text(size = 1.1 * rel(1)))  # Increase the size of legend text

fig_s_pca_with_names

ggsave("plots/fig_s_pca_with_names.png", 
	fig_s_pca_with_names, scale = 2, bg = "white", width = 15, height = 10, units = "cm", dpi = 600)


