library(tidyverse)
library(corrplot)
library(tidymodels)
#library(palmerpenguins)
library(corrr)
#library(GGally)
#library(recipes)
library(tidytext)
#library(dplyr)
#library(tidyr)
#library(ggplot2)
#theme_set(theme_minimal())
library(dotwhisker)
#install.packages(rnorsk)
library(gridExtra)
library(ggpubr)
library(sf)
library(tmap)
library(factoextra)#derive results from prcomp, pca

Sys.setlocale("LC_CTYPE", "norwegian")

# Original data, entire country
data_complete <- read.csv("holiday_home_data.csv", 
	header = TRUE, sep = ";", encoding = "latin") |> as_tibble()

data_complete$region <- as.character(data_complete$region)
data_complete$region[data_complete$region == "301"] <- "0301"
glimpse(data_complete)

# Data for mountain municipalities


# Now we have three datasets for furter modeling
# 1) data, complete data, without missing data
# 2) z_data, where the response variables (except dummy variables), are transformed to z-scores
# 3) zs_data, where some of the response variables are  skewness-transformed (log), 
# proportions are logit-transformed, and all responses finally transformed to z-scores

data <- read.csv("mountain_data.csv", 
	header = TRUE, sep = ",", encoding = "latin") |> as_tibble()
data$region <- as.character(data$region)
glimpse(data)
data <- data |> dplyr::select(-hh_category)

# PCA



# Base R PCA
data_num <- data |> dplyr::select(-region, -municipality, -county, -dummy_coast)
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

glimpse(data)
#data$sentralitetsindeks <- as.factor(data$sentralitet)

hytter_recipe <-
  recipe(~., data = data) %>% 
  update_role(region, municipality, county, dummy_coast, new_role = "id") %>% 
	#step_impute_mean(all_numeric()) |> 
	# 1 impute or omit na
	step_naomit(all_numeric()) %>% 
	# mean impute numeric variables
  # step_impute_mean(all_numeric()) |> 
	# 2 Handle factor levels
  # convert specific variable(s) to dummy variables
  #step_dummy(kyst_f) %>%
	# 3 Individual transformations for skewness and other issues
	# step_YeoJohnson(all_numeric()) |> 
	# 4 Create interactions
	# 5 Normalization steps (center, scale, range, etc)
#   step_center(all_numeric()) |> 
# 	step_scale(all_numeric()) |> 
	step_normalize(all_numeric()) %>%
	# rescale all numeric variables except ..., ... and ... to lie between 0 and 1
  # step_range(all_numeric(), min = 0, max = 1, -vanilla, -salt, -baking_powder) %>%
	#step_range(all_numeric(), min = 0, max = 1) %>%
	# remove predictor variables that are almost the same for every entry
  step_nzv(all_predictors()) |> 
  step_pca(all_numeric(), id = "pca") %>% 
  prep()

#data_t <- data |> select(-region, -municipality, -county, -kyst_f, -hyttekomtype, -KDP_natur)
data_t <- data |> dplyr::select(where(is.numeric)) 

hytter_recipe2 <-
  recipe(~., data = data_t) |> 
	#update_role(region, municipality, county, new_role = "id") %>% 
	step_naomit(all_numeric()) |> 
	#step_YeoJohnson(all_numeric()) |> 
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

# plot the cumulative variance explained
ggplot(var_exp_df, aes(x = PC, y = Cumulative.Variance.Explained)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = scales::percent_format()) +
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
cbp1 = c("#0072B2", "#E69F00", "#009E73")
# cbp1 = c("#0072B2", "#E69F00", "red")

juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2, label = municipality)) +
  geom_point(aes(PC1, PC2, colour = county), alpha = 0.7, size = 2) +
	#scale_colour_manual(values=cbp1)+
  #geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)+
	  theme_minimal()

juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = county), size = 2, alpha = 0.9) +
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
  geom_point(aes(color = county), size = 2, alpha = 0.9) +
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



pca_loadings <- pca_wider |> dplyr::select(terms, PC1, PC2, PC3, PC4)
pca_loadings


arrows_all <- pca_wider[2:36,]



# Axis 3
juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC3)) +
  geom_point(aes(color = county), size = 2, alpha = 0.6) +
	#scale_colour_manual(values=cbp2)+
  geom_segment(data = pca_wider,
               aes(x = 0, y = 0, xend = PC1 * 20, yend = PC3 * 20), # control arrow length
               arrow = arrows(length = unit(1/2, "picas")),
               color = "blue") +
  annotate("text",
           x = pca_wider$PC1 * 20.2, # control placement of text
           y = pca_wider$PC3 * 20.2, # control placement of text
           label = pca_wider$terms) +
  theme_minimal() +
  xlab("PC1 (nn %)") +
  ylab("PC3 (nn %)")




# Publication ready plots -------------------------------------------------

pca_cor_axis1 |> slice(1:10)
pca_cor_axis2 |> slice(1:10)

arrows <- pca_wider |> filter(terms == "holiday_homes")
arrows <- pca_wider |> 
  filter(terms %in% c("holiday_homes", 
  	"infrastructure_index", "dist_to_coast", "holiday_homes_1970", "pop_4hr_drive", 
  	"future_hha", "median_settlement_ratio"))

arrows$terms

#arrows <- pca_wider [2:37,] 

arrows <- pca_wider |>
	filter(terms == "holiday_homes", "future_hh_areas")

arrows <- pca_wider |>
  filter(terms %in% c("holiday_homes", "future_hh_areas"))


arr <- pca_wider |>
	filter(terms %in% c("holiday_homes",
    "infrastructure_index",
  	"population_change",
  	"dist_to_coast",
  	"veg_zones",
  	"future_hh_area"))



# create a new column with the updated terms
arrows_updated <- arr %>%
  mutate(terms_updated = case_when(
    terms == "holiday_homes" ~ "Holiday homes",
  	terms == "Future_hh_area" ~ "Future holiday home area",
    terms == "infrastructure_index" ~ "Infrastructure",
  	terms == "population_change" ~ "Population increase",
  	terms == "dist_to_coast" ~ "Inland",
  	terms == "veg_zones" ~ "Vegetation zones",
    TRUE ~ as.character(terms)
  ))

cbp1 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", 
	"#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")

# plot the updated arrows
juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2)) +
	geom_vline(xintercept = 0, linetype="dashed", color = "darkgrey")+
	geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  geom_point(aes(color = county, shape=county), size = 2, alpha = 0.8) +
  geom_point(aes(color = county), size = 2, alpha = 0.6) +
  scale_colour_manual(values=cbp1) +
  geom_segment(data = arrows_updated,
               aes(x = 0, y = 0, xend = PC1 * 20, yend = PC2 * 20), # control arrow length
               arrow = arrow(length = unit(1/2, "picas")),
               color = "blue") +
  annotate("text",
           x = arrows_updated$PC1 * 21.2, # control placement of text
           y = arrows_updated$PC2 * 21.2, # control placement of text
           label = arrows_updated$terms_updated) +
  theme_minimal() +
  xlab("Principal component 1 (nn %)") +
  ylab("Principal component 2 (nn %)")

fig_3 <- 
juice(hytter_recipe) %>%
  ggplot(aes(PC1, PC2)) +
	geom_vline(xintercept = 0, linetype="dashed", color = "darkgrey")+
	geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  geom_point(aes(color = county, shape=county), size = 2, alpha = 0.8) +
  geom_point(aes(color = county), size = 2, alpha = 0.6) +
  scale_colour_manual(values=cbp1) +
  geom_segment(data = arrows_updated,
               aes(x = 0, y = 0, xend = PC1 * 20, yend = PC2 * 20), # control arrow length
               arrow = arrow(length = unit(1/2, "picas")),
               color = "blue") +
  annotate("text",
           x = arrows_updated$PC1 * 21.2, # control placement of text
           y = arrows_updated$PC2 * 21.2, # control placement of text
           label = arrows_updated$terms_updated) +
  theme_minimal() +
  xlab("Principal component 1 (nn %)") +
  ylab("Principal component 2 (nn %)")

fig_3


ggsave("P:/15022000_egenutvikling_trond_simensen/Spatial_regression/Paper/illustrations/figure3.png", 
	fig_3, scale = 2, bg = "white", width = 16, height = 8, units = "cm", dpi = 600)



