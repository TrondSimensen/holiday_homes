library(tidyverse)
library(tidymodels)
library(tidytext)
library(dotwhisker)
library(ggpubr)
library(sf)
library(tmap)
library(rnorsk)
library(leaps)
library(car) #logit-transformations
library(MASS)
library(summarytools)
library(kableExtra)
library(corrplot)
library(summarytools)
library(kableExtra)
library(tidymodels)
library(rnorsk) # for regression diagnostics

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

# z_data, where the response variables (except dummy variables), are transformed to z-scores
z_data <- read.csv("z_mountain_data.csv", 
	header = TRUE, sep = ",", encoding = "latin") |> as_tibble()
z_data$region <- as.character(z_data$region)
glimpse(z_data)

# zs_data, where some of the response variables are  skewness-transformed (log), 
# proportions are logit-transformed, and all responses finally transformed to z-scores
zs_data <- read.csv("zs_mountain_data.csv", 
	header = TRUE, sep = ",", encoding = "latin") |> as_tibble()
zs_data$region <- as.character(zs_data$region)
glimpse(zs_data)


# Correlation matrix ------------------------------------------------------
data_num <- data |> dplyr::select(-holiday_homes, -future_hh_area,
	-region, -municipality, -county, -dummy_coast) |> na.omit()

data_num

correlations <- round(cor(data_num, method = "kendall"),2)
correlations
corrplot(correlations, method = 'number', order = "hclust")
# write.csv(correlations, file = "correlations_mountains.csv", row.names = FALSE)

# Extract response variables ---------------------------------------------------------
response1 <- data |> dplyr::select(holiday_homes) 
response2 <- data |> dplyr::select(future_hh_area) 

# data for correlation analysis
dat <- data |> dplyr::select(-region, -municipality, -county, -dummy_coast)
dat

# Correlation with response 1: holiday homes ---------------------------------------------

# Create an empty dataframe to store the results
cor_summary <- data.frame(variable = character(),
                          cor_with_hh = character(),
                          p_value_t1 = character(),
                          stringsAsFactors = FALSE)

# Perform Kendall correlation test for each variable
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

# Perform Kendall correlation test for each variable
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
write.table(summary_cor_with_responses, file = "clipboard", sep = "\t", row.names = FALSE)

# Explore data ------------------------------------------------------------

data [,1:9] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)

zs_data [,1:9] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)


# Modeling ----------------------------------------------------------------

hist(sqrt(data$future_hh_area))

# Linear model 
ggplot(data, aes(x = log(holiday_homes), y = log(future_hh_area))) +
	geom_point(pch = 21, fill = "steelblue", alpha = 1/2, size = 4) +
  #geom_abline(linetype = 2, color = "red3")+
	geom_smooth(method=lm , color="red", se=TRUE)+
	stat_regline_equation(p.accuracy = 0.001,
   aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  label.x = 5
)+
	theme_classic()

ggplot(data, aes(x = log(holiday_homes), y = log(future_hh_area), label = municipality)) +
	geom_point(pch = 21, fill = "steelblue", alpha = 1/2, size = 4) +
		geom_text(check_overlap = FALSE, hjust = "inward", family = "IBMPlexSans") +
  #geom_abline(linetype = 2, color = "red3")+
	geom_smooth(method=lm , color="red", se=TRUE)+
	stat_regline_equation(p.accuracy = 0.001,
   aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  label.x = 5
)+
	theme_classic()

m1 <- lm(future_hh_area ~ holiday_homes, data = data)
m2 <- lm(future_hh_area ~ sqrt(holiday_homes), data = data)
m3 <- lm(future_hh_area ~ log(holiday_homes), data = data)
m4 <- lm(sqrt(future_hh_area) ~ holiday_homes, data = data)
m5 <- lm(sqrt(future_hh_area) ~ sqrt(holiday_homes), data = data)
m6 <- lm(sqrt(future_hh_area) ~ log(holiday_homes), data = data)
m7 <- lm(log(future_hh_area) ~ holiday_homes, data = data)
m8 <- lm(log(future_hh_area) ~ sqrt(holiday_homes), data = data)
m9 <- lm(log(future_hh_area) ~ log(holiday_homes), data = data)

glance(m1)
glance(m2)
glance(m3)
glance(m4)
glance(m5)
glance(m6)
glance(m7)
glance(m8)
glance(m9)

tidy(m6)
glance(m6)
hist(m6$residuals)
par(mfrow = c(2,2))
plot(m6)
regression.diagnostics(m6)


# Linear model, multiple
ggplot(zs_data, aes(x = ln_holiday_homes_1970, y = sqrt(holiday_homes))) +
	geom_point(pch = 21, fill = "steelblue", alpha = 1/2, size = 4) +
  #geom_abline(linetype = 2, color = "red3")+
	geom_smooth(method=lm , color="red", se=TRUE)+
	stat_regline_equation(p.accuracy = 0.001,
   aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  label.x = -2.5
)+
	theme_classic()

# Testing modeling procedures
m6 <- lm(sqrt(future_hh_area) ~ holiday_homes + ln_land_area , data = zs_data)
tidy(m6)
glance(m6)
hist(m6$residuals)
par(mfrow = c(2,2))
plot(m6)
regression.diagnostics(m6)

# Linear model, multiple
ggplot(data, aes(x = veg_zones, y = sqrt(future_hh_area))) +
	geom_point(pch = 21, fill = "steelblue", alpha = 1/2, size = 4) +
  #geom_abline(linetype = 2, color = "red3")+
	geom_smooth(method=lm , color="red", se=TRUE)+
	stat_regline_equation(p.accuracy = 0.001,
   aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  label.x = 0
)+
	theme_classic()

plot(data$future_hh_area ~ data$dist_to_dummy_coast)

#m7 <- lm(sqrt(future_hh_area) ~ log(holiday_homes) + population_change, data = data)
m7 <- lm(sqrt(future_hh_area) ~ log(holiday_homes) + veg_zones , data = data)
glance(m7)
m7 <- lm(sqrt(future_hh_area) ~ log(holiday_homes) + log(land_area) + population_change + dist_to_dummy_coast, data = data)
glance(m7)
m7 <- lm(sqrt(future_hh_area) ~ log(holiday_homes) + population_change + veg_sections +log(dist_to_dummy_coast) + pop_2hr_drive, data = data)
glance(m7)
m7 <- lm(sqrt(future_hh_area) ~ log(holiday_homes) + population_change + ln_land_area + veg_zones + dummy_coast, data = zs_data)
glance(m7)
m7 <- lm(sqrt(future_hh_area) ~ log(holiday_homes) + population_change + log(land_area+2) + veg_zones +dummy_coast, data = zs_data)
glance(m7)
m7 <- lm(sqrt(future_hh_area) ~ ln_holiday_homes + population_change + ln_land_area + veg_zones + dummy_coast, data = zs_data)
glance(m7)
m7 <- lm(sqrt(future_hh_area) ~ log(holiday_homes) + population_change + log(land_area) + veg_zones + dist_to_dummy_coast, data = data)
glance(m7)
#m7 <- lm(sqrt(holiday_homes) ~ sqrt(future_hh_area) + population_change + log(land_area+2) + veg_zones +dummy_coast, data = zs_data)

glance(m7)
tidy(m7)
hist(m7$residuals)
par(mfrow = c(2,2))
plot(m7)
par(mfrow = c(1,1))
regression.diagnostics(m7)

tidy(m7) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))+
		theme_bw()

library(broom)
library(kableExtra)

options(scipen=999)

# Extract the regression coefficients and standard errors using the tidy() function from the broom package
m7_results <- tidy(m7)

# Get the confidence intervals for the coefficients
m7_ci <- confint(m7)

# Merge the coefficient table with the confidence interval table
m7_results <- cbind(m7_results[,-1], m7_ci)

m7_results

is.num <- sapply(m7_results, is.numeric)
m7_results[is.num] <- lapply(m7_results[is.num], round, 4)

# Rename the columns
colnames(m7_results) <- c("Coefficients", "Standard_Error", "t_Stat", "P_value", "Lower_95%", "Upper_95%")

# # Check if any column has non-numeric values
# non_numeric_cols <- sapply(m7_results, function(x) any(!is.numeric(x)))
# 
# # Convert non-numeric columns to numeric
# if (any(non_numeric_cols)) {
#   m7_results[, non_numeric_cols] <- apply(m7_results[, non_numeric_cols], 2, function(x) as.numeric(as.character(x)))
# }
# 
# # Format the numerical columns to four decimal places and the confidence intervals to two decimal places
# num_cols <- names(m7_results)[2:5]
# m7_results[, num_cols] <- lapply(m7_results[, num_cols], function(x) format(round(x, 4), nsmall = 4))
# m7_results[, c("Lower 95%", "Upper 95%")] <- lapply(m7_results[, c("Lower 95%", "Upper 95%")], function(x) {
#   if (is.numeric(x)) {
#     format(round(x, 2), nsmall = 2)
#   } else {
#     x
#   }
# })
# 
# m7_results
# 
# # Format P-values
# m7_results$P.value <- ifelse(m7_results$P.value < 0.001, "<0.0001", sprintf("%.4f", m7_results$P.value))

# Print the results
m7_results

# Format the table using kable() and save to clipboard
clipr::write_clip(kable(m7_results[, -1], align = "cccccc", caption = "Table 1. Regression Results for future_hh_area"), format = "html")
# Format the table using kable() and save to clipboard
kable(m7_results, format = "html") %>% 
  kable_styling(full_width = FALSE) %>% 
  column_spec(1, bold = TRUE) %>% 
  column_spec(2:6, width = "100px") %>% 
  as.character() %>% 
  clipr::write_clip()

glance(m7)

glance_m7 <- glance(m7)

summary_string <- paste("R-squared:", round(glance_m7$r.squared, 2),
                        "Adjusted R-squared:", round(glance_m7$adj.r.squared, 2),
                        "Sigma:", round(glance_m7$sigma, 2),
                        "F-statistic:", round(glance_m7$statistic, 2),
                        "P-value:", formatC(glance_m7$p.value, format = "e", digits = 4),
                        "Degrees of freedom (df):", glance_m7$df,
                        "Log-likelihood (logLik):", round(glance_m7$logLik, 2),
                        "Akaike Information Criterion (AIC):", round(glance_m7$AIC, 0),
                        "Bayesian Information Criterion (BIC):", round(glance_m7$BIC, 0),
                        "Deviance:", round(glance_m7$deviance, 0),
                        "Degrees of freedom for residuals (df.residual):", glance_m7$df.residual,
                        "Number of observations (nobs):", glance_m7$nobs,
                        sep = " ")

summary_string


# Check specific variables ------------------------------------------------

head(data)

ggplot(data, aes(x = veg_zones, y = sqrt(future_hh_area), label = municipality)) +
	geom_point(pch = 21, fill = "steelblue", alpha = 1/2, size = 4) +
	geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +

  #geom_abline(linetype = 2, color = "red3")+
	geom_smooth(method=lm , color="red", se=TRUE)+
	stat_regline_equation(p.accuracy = 0.001,
   aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  label.x = -3
)+
	theme_classic()

# Robust regression -------------------------------------------------------

m8 <- rlm(sqrt(future_hh_area) ~ log(holiday_homes) + population_change + ln_land_area + veg_zones + dummy_coast, data = zs_data)
m8
glance(m8)

#num <- data |> dplyr::select(holiday_homes, land_area, population_change, dummy_coast)


# Mixed model -------------------------------------------------------------
library(lme4)
m8 <- lmer(sqrt(future_hh_area) ~ ln_holiday_homes + population_change + ln_land_area + veg_zones + dummy_coast + (1|county), data = zs_data)

plot(m8)
summary(m8)
ranef(m8)

anova(m7, m8)
AIC(m7, m8)
BIC(m7, m8)


# Linear model, multiple
ggplot(zs_data, aes(y = ln_population, x = sqrt(holiday_homes))) +
	geom_point(pch = 21, fill = "steelblue", alpha = 1/2, size = 4) +
  #geom_abline(linetype = 2, color = "red3")+
	geom_smooth(method=lm , color="red", se=TRUE)+
	stat_regline_equation(p.accuracy = 0.001,
   aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  label.x = 0
)+
	theme_classic()

# Linear model, multiple
ggplot(data, aes(y = population_change, x = holiday_homes)) +
	geom_point(pch = 21, fill = "steelblue", alpha = 1/2, size = 4) +
  #geom_abline(linetype = 2, color = "red3")+
	geom_smooth(method=lm , color="red", se=TRUE)+
	stat_regline_equation(p.accuracy = 0.001,
   aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  label.x = 0
)+
	theme_classic()

# Linear model, multiple
ggplot(data, aes(y = population_change, x = future_hh_area)) +
	geom_point(pch = 21, fill = "steelblue", alpha = 1/2, size = 4) +
  #geom_abline(linetype = 2, color = "red3")+
	geom_smooth(method=lm , color="red", se=TRUE)+
	stat_regline_equation(p.accuracy = 0.001,
   aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  label.x = 0
)+
	theme_classic()

# Obtain predicted values from the model
data_pred <- zs_data
data_pred$predicted <- predict(m7, newdata = data_pred)

# Reverse the square root transformation of the predicted values
data_pred$predicted2 <- data_pred$predicted^2

# Print the first 10 rows of the original and predicted values for comparison
head(data_pred[, c("future_hh_area", "predicted")], 10)
head(data_pred[, c("future_hh_area", "predicted2")], 10)


# Output:
#    future_hh_area predicted
# 1        33.810  33.23396
# 2        17.606  18.08945
# 3         3.609   3.35484
# 4        19.758  21.34677
# 5         1.982   1.78596
# 6        23.188  22.64723
# 7         4.987   4.35013
# 8         2.691   2.58272
# 9         2.064   2.02436
# 10       12.893  12.79817


# Model selection with GLMnet ---------------------------------------------

#define response variable
y <- data_num$future_hh_area

#define matrix of predictor variables
x <- data.matrix(data_num [, 2:35])

library(glmnet)
par(mfrow=c(1,1))

# Fit a gamma regression model with L1 regularization and cross-validation
cv_model <- cv.glmnet(x, y, family = Gamma(link = "identity"), alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
# Fit the best model using the optimal value of lambda
best_model <- glmnet(x, y, family = Gamma(link = "inverse"), alpha = 1, lambda = best_lambda)
coef(best_model)
best <- best_model$beta
best
best_mod <- as.matrix(best)
plot(best)

best_mod <- as.data.frame(best_mod)|> 
	rownames_to_column(var = "rowname") |> 
	rename (value = s0) 

best_mod <- best_mod |> filter(value > 0)

paste(best_mod$rowname, collapse = " + ")

m_best <- lm(sqrt(future_hh_area) ~ median_hh_price + prop_hh_area + dist_to_coast + infrastructure_index + population_change + median_age + prop_farm_residents + municipal_economy + ln_pop_2hr_drive + ln_off_road_motor + ln_ski_lifts + ln_activity_tracking_data,
	data = zs_data)

glance(m_best)
library(tidymodels)
library(tidyverse)
library(dotwhisker)
tidy(m_best) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))+
		theme_bw()

Fri_egenkap_drift_pst + ln_fribygg + ln_dnt_hytter + ln_km_fot + ln_befolkning_2h_avg + ln_disp_motor

# Convert the glmnet object to an lm object
best_model <- predict(cv_model, newx = x, s = "lambda.min", type="coefficients")
best_model
glance(best_model)

# Extract and organize model information using broom functions
glance(best_model)
tidy(best_model)



df5 <- as.data.frame(summary.out$which[5,]) |> 
	rownames_to_column(var = "rowname") |> 
	filter(summary.out$which[5,] == TRUE)
paste(df5$rowname, collapse = " + ")


fit <- glmnet(x, y)
plot(fit)
plot(fit, xvar = "lambda", label = TRUE)
print(fit)

coef(fit, s = 4)
coef(fit, s = 3)
coef(fit, s = 2)
coef(fit, s = 1)
coef(fit, s = 0.5)
coef(fit, s = 0.1)

cvfit <- cv.glmnet(x, y)
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")

plot(fit, xvar = "dev", label = TRUE)
Column_Names_x<-colnames(x)
Column_Names_x[10]
Column_Names_x[20]
Column_Names_x[19]
Column_Names_x[51]
Column_Names_x[21]
Column_Names_x[11]
Column_Names_x[4]
Column_Names_x[55]

# Fit the GLM with Gamma family and the inverse link function, providing starting values
gamma_glm_inverse <- glm(future_hh_area ~ ln_fribygg + logit_hytter_daa_avg + logit_frP + logit_EU + logit_v,
	data = data_num, family = Gamma(link = "inverse"))

gamma_glm_inverse

gamma_glm_log <- glm(future_hh_area ~ ln_fribygg + logit_hytter_daa_avg + logit_frP + logit_EU + logit_v,
	data = data_num, family = Gamma(link = "log"))

gamma_glm_log

# More selection methods
# https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html

# Snippet -----------------------------------------------------------------
detach(package:tidyverse)
detach(package:tidymodels)
#detach(package:MIAmaxent)
detach(package:corrplot)
detach(package:corrr)
detach(package:tidytext)
detach(package:dotwhisker)
detach(package:gridExtra)
detach(package:ggpubr)
detach(package:sf)
detach(package:tmap)
detach(package:rnorsk)
#detach(package:factoextra)
#detach(package:caret)
detach(package:leaps)
detach(package:car)

library(MASS)
library(leaps)



v <- matrix(0.9,11,11)
diag(v) <- 1

set.seed(15)
mydat <- mvrnorm(100, rep(0,11), v)
mydf <- as.data.frame( mydat )

fit1 <- lm( V1 ~ 1, data=mydf )
fit2 <- lm( V1 ~ ., data=mydf )

library(MASS)
library(leaps)
data_num_reduced <- data_num[,1:30]
X = model.matrix(future_hh_area ~ ., data = data_num_reduced)
future_hh_area.leaps = leaps(X, X$future_hh_area, nbest=3, method='r2')
best.model.r2 = election.leaps$which[which((election.leaps$r2 == 
                                            max(election.leaps$r2))),]



# Snippet end -------------------------------------------------------------

#https://web.stanford.edu/class/stats191/notebooks/Selection.html

url = 'http://stats191.stanford.edu/data/election.table'
election.table = read.table(url, header=T)
pairs(election.table[,2:ncol(election.table)], cex.labels=3, pch=23,
      bg='orange', cex=2)

election.lm = lm(V ~ I + D + W + G:I + P + N, election.table)
election.lm

X = model.matrix(election.lm)[,-1]
library(leaps)
election.leaps = leaps(X, election.table$V, nbest=3, method='r2')
best.model.r2 = election.leaps$which[which((election.leaps$r2 == 
                                            max(election.leaps$r2))),]
best.model.r2

plot(election.leaps$size, election.leaps$r2, pch=23, bg='orange', cex=2)

election.leaps = leaps(X, election.table$V, nbest=3, method='adjr2')
best.model.adjr2 = election.leaps$which[which((election.leaps$adjr2 == max(election.leaps$adjr2))),]
best.model.adjr2

plot(election.leaps$size, election.leaps$adjr2, pch=23, bg='orange', 
     cex=2)

election.leaps = leaps(X, election.table$V, nbest=3, method='Cp')
best.model.Cp = election.leaps$which[which((election.leaps$Cp == 
                                            min(election.leaps$Cp))),]
best.model.Cp

plot(election.leaps$size, election.leaps$Cp, pch=23, bg='orange', cex=2)

n = nrow(X)
p = 7 + 1 
c(n * log(2*pi*sum(resid(election.lm)^2)/n) + n + 2*p, AIC(election.lm))

election.step.forward = step(lm(V ~ 1, election.table), 
                             list(upper = ~ I + D + W + G + G:I + P + N), 
                             direction='forward', 
                             k=2, trace=FALSE)
election.step.forward

summary(election.step.forward)


election.step.forward.BIC = step(lm(V ~ 1, election.table), 
                                 list(upper = ~ I + D + W +G:I + P + N), 
                                 direction='forward', k=log(nrow(X)))

summary(election.step.forward.BIC)

election.step.backward = step(election.lm, direction='backward')

summary(election.step.backward)

# more at page if this works...





library(MASS)
library(leaps)

fit1 <- lm( future_hh_area ~ 1, data=data_num )
fit2 <- lm( future_hh_area ~ ., data=data_num )
summary(fit1)

fit <- step( fit1, formula(fit2), direction='forward' )
summary(fit)$r.squared

all <- leaps(mydat[,-1], mydat[,1], method='r2')
max(all$r2[ all$size==length(coef(fit)) ])

plot( all$size, all$r2 )
points( length(coef(fit)), summary(fit)$r.squared, col='red' )



# Plot a model as a map ---------------------------------------------------

fgdb <- "C:/Users/trond.simensen/OneDrive - NINA/Documents/GIS_Trond/N5000_Norge/Basisdata_0000_Norge_25833_N5000Kartdata_FGDB.gdb"
st_layers(fgdb, options = character(0), do_count = FALSE)
norske_kommuner <- st_read(fgdb,layer = "N5000_AdministrativeOmråder_omrade") %>% 
   rename(region = kommunenummer) %>% 
   dplyr::select(region) #%>% st_simplify(preserveTopology = TRUE, dTolerance = 1000)

plot(norske_kommuner)
nrow(norske_kommuner)

land <- st_read(fgdb,layer = "N5000_Arealdekke_omrade") 
#plot(land)
hav <- land |> filter(objtype == "Havflate")
hav  <-  hav |> 
  group_by(objtype) |>
  summarize(area = sum(SHAPE_Area, na.rm = TRUE))

h <- st_union(hav)
plot(h, col = "blue")

plot(norske_kommuner)
nrow(norske_kommuner)

komm <- left_join(norske_kommuner, data, by = "region")
plot(komm, max.plot = 3)
plot(komm$land_area)
land <- dplyr::select(komm, land)
plot(land)

tm_shape(komm, unit = "m") +
  tm_polygons(col = "veg_zones", style = "equal", palette = "Greens", 
              border.col = "white", border.alpha = 0.5, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "Responsvariabel",  main.title.size = 0.95, title = "andel", frame = FALSE, legend.outside = TRUE, 
            attr.outside = TRUE)

hist(data$veg_zones)

ggplot(data, aes(x = veg_zones, y = sqrt(future_hh_area), label = municipality)) +
	geom_point(pch = 21, fill = "steelblue", alpha = 1/2, size = 4) +
	geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +

  #geom_abline(linetype = 2, color = "red3")+
	geom_smooth(method=lm , color="red", se=TRUE)+
	stat_regline_equation(p.accuracy = 0.001,
   aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  label.x = 5
)+
	theme_classic()


# Spatial output

res <- data
# res <- data |> 
# 	dplyr::select(region, future_hh_areaerve_daa_sum)
spat_mod <- left_join(norske_kommuner, res, by = "region")

tm_shape(spat_mod, unit = "m") +
  tm_polygons(col = "future_hh_area", style = "jenks", palette = "Oranges", 
              border.col = "white", border.alpha = 0.5, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "future_hh_areaerve",  main.title.size = 0.95, title = "Daa", frame = FALSE, legend.outside = TRUE, 
            attr.outside = TRUE)

#Creating a df with the same number of observations as in the models 
# (without NA values for the variables at stake)
#data_num <- data |> select(region, future_hh_areaerve_daa_sum, holiday_homes) |> na.omit()

# best.model <- m7
# data$residuals_m1 <- residuals(best.model) # extract the residuals
# data$fitted_m1 <- fitted(best.model)# extract the predictions
#data$fitted_m1 <- data$fitted_m1^2

#spat_mod1 <- left_join(norske_kommuner, data, by = "region")

tm_shape(spat_mod, unit = "m") +
  tm_polygons(col = "predicted2", style = "jenks", palette = "Oranges", 
              border.col = "white", border.alpha = 0.5, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "future_hh_areaerve",  main.title.size = 0.95, title = "Daa", frame = FALSE, legend.outside = TRUE, 
            attr.outside = TRUE)

#current_style <- tmap_style("col_blind")

spat_mod$residuals_km2 <- spat_mod$future_hh_area - spat_mod$predicted2

tm1 <- tm_shape(spat_mod, unit = "m") + 
  tm_fill("future_hh_area", title = "future_hh_areaerve", style="jenks", palette = "Reds") +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Samlet future_hh_areaerve for fritidsboliger", main.title.size = 0.7 ,
            legend.position = c("right", "bottom"), legend.title.size = 0.8)

tm1

tm2 <- tm_shape(spat_mod, unit = "m") + 
  tm_fill("predicted2", title = "Prediksjon", style="jenks", palette = "Reds") +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "MLR: Holiday home areas", main.title.size = 0.7 ,
            legend.position = c("right", "bottom"), legend.title.size = 0.8)

tm2
#my_breaks <- c(-14,-3,-2,-1,1,2,3,14)

# tm3 <- tm_shape(spat_mod, unit = "m") + 
#   tm_fill("residuals_m1", title = "Residualer", style = "jenks", palette = "-RdBu", na.value="grey") +
#   tm_borders(alpha = 0.1) +
#   tm_layout(main.title = "Residuals", main.title.size = 0.7 ,
#             legend.position = c("right", "bottom"), legend.title.size = 0.8)
# 
# tm3

tm3 <- tm_shape(spat_mod, unit = "m") + 
  tm_fill("residuals_km2", title = "Residualer", style = "jenks", palette = "-RdBu", na.value="grey") +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Residuals", main.title.size = 0.7 ,
            legend.position = c("right", "bottom"), legend.title.size = 0.8)
 
tm3

tmap_arrange(tm1, tm2, tm3)

library(spdep)
spat_dat <- na.omit(spat_mod)
#?poly2nb
kommb <- poly2nb(spat_dat, queen=T)
poly2nb
# Spatial weights for neighbours lists
# Description
# The nb2listw function supplements a neighbours list with spatial weights for 
# the chosen coding scheme. 
spweights<-nb2listw(kommb, style="W", zero.policy = TRUE)

summary(spweights, zero.policy = TRUE)

moran.plot(spat_dat$residuals_km2, listw=spweights, xlab="Residuals", ylab="Neighbors response",
           label = TRUE, main=c("Scatterplot for model residuals") )

moran.test(spat_dat$residuals_km2, spweights, na.action = na.omit)
moran.mc(spat_dat$residuals_km2, spweights, nsim=999)

moran.test(spat_dat$residuals_km2, spweights_zero_policy, na.action = na.omit)
moran.mc(spat_dat$residuals_km2, spweights_zero_policy, nsim = 999)


moran.plot(spat_dat$future_hh_areaerve_daa_sum, spweights)

localI <- localmoran(spat_dat$residuals_km2, listw = spweights, zero.policy = TRUE)
head(localI)

m1 <- lm(log(holiday_homes) ~ ln_land_area + ln_holiday_homes_1970 + 
	ln_pop_4hr_drive + ln_ski_lifts + altitudinal_range, data = zs_data)

m1
glance(m1)
tidy(m1) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))+
		theme_bw()

data_num <- zs_data |> dplyr::select(-holiday_homes, -future_hh_area,
	-region, -municipality, -county) |> na.omit()

m2 <- lm(sqrt_future_hh_area ~ ln_land_area + ln_holiday_homes_1970 + 
	ln_pop_4hr_drive + ln_ski_lifts + altitudinal_range, data = zs_data)

m2

glance(m2)

tidy(m2) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))+
		theme_bw()



