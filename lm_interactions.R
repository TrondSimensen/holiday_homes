library(tidyverse)
library(corrplot)
library(tidymodels)

Sys.setlocale("LC_CTYPE", "norwegian")

## Load data for mountain municipalities
data <- read.csv("data/mountain_data.csv",  header = TRUE, sep = ",", encoding = "latin") %>%
  as.tibble()

data_z <- read.csv("data/z_mountain_data.csv",  header = TRUE, sep = ",", encoding = "latin") %>%
  as.tibble()

## Select relevant variables
data_z <- data_z %>%
  dplyr::select(region, municipality, county, # ID variables
                holiday_homes, future_hh_area, # response variables
                holiday_homes_1970, # category "holiday homes"
                dist_to_coast, veg_zones, # category "physical geography"
                infrastructure_index, # category "land use"
                population_change, # category "population"
                unrestricted_revenues, # category "economy"
                activity_data) %>% # category "recreation"
  dplyr::rename(pastHH = holiday_homes_1970,
                coast = dist_to_coast,
                veg = veg_zones,
                landuse = infrastructure_index,
                pop = population_change,
                econ = unrestricted_revenues,
                recr = activity_data) %>%
  dplyr::mutate(holiday_homes = log(holiday_homes),
                future_hh_area = sqrt(future_hh_area))

## Check correlations among explanatory variables
M <- cor(data_z[,6:12])
corrplot(M, order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', type = "lower" )

# --> Strongest correlation between land use (infrastructure) and vegetation zone (0.6)
# --> Past holiday homes correlated with recreation and land use (>0.5)
# --> Ecoonomy (revenues) somewhat correlated with everything but physical geography
# --> Land use (infrastrucuture) somewhat correlated with everything else


## Set up the "mother" models (containing all terms under consideration)
modAll_currentHH <- lm(holiday_homes ~
                         pastHH + coast + veg + landuse + pop + econ + recr + 
                         pastHH:coast + pastHH:veg + pastHH:landuse + pastHH:pop + pastHH:econ + pastHH:recr +
                         coast:veg + coast:landuse + coast:pop + coast:econ + coast:recr +
                         veg:landuse + veg:pop + veg:econ + veg:recr +
                         landuse:pop + landuse:econ + landuse:recr +
                         pop:econ + pop:recr +
                         econ:recr, 
                       data = data_z, na.action = "na.fail")

modAll_futureHH <- lm(future_hh_area ~
                        pastHH + coast + veg + landuse + pop + econ + recr + 
                        pastHH:coast + pastHH:veg + pastHH:landuse + pastHH:pop + pastHH:econ + pastHH:recr +
                        coast:veg + coast:landuse + coast:pop + coast:econ + coast:recr +
                        veg:landuse + veg:pop + veg:econ + veg:recr +
                        landuse:pop + landuse:econ + landuse:recr +
                        pop:econ + pop:recr +
                        econ:recr, 
                      data = data_z, na.action = "na.fail")

## Check mother model summaries
summary(modAll_currentHH)
# --> Evidence for all independent effects except landuse and the following interactions:
#     - coast:pop, coast:recr
#     - veg:econ
#     - landuse:econ
#     - pop:recr, econ:recr

summary(modAll_futureHH)
# --> Evidence for independent effects of pastHH, coast, and veg and the following interactions:
#     - pastHH:veg, pastHH:pop
#     - coast:veg, coast:landuse, coast:pop
#     - veg:recr
#     - pop:recr


## Find most parsimonious model by AIC (generally favours models with more parameters)
modRank_currentHH <- MuMIn::dredge(modAll_currentHH, rank = "AICc", extra = c("R^2", "adjR^2", "BIC")) 
write.csv(modRank_currentHH, file = "modRank_currentHH.csv", row.names = FALSE)

## Find most parsimonious model by BIC (generally favours models with less parameters)
modRank_futureHH <- MuMIn::dredge(modAll_futureHH, rank = "BIC", extra = c("R^2", "adjR^2", "BIC")) 
write.csv(modRank_futureHH, file = "modRank_futureHH.csv", row.names = FALSE)


## Make reduced mother models based on strength of evidence in full models
#  (include interactions only if p in full model < 0.2)
modRed_currentHH <- lm(holiday_homes ~
                         pastHH + coast + veg + landuse + pop + econ + recr + 
                         coast:pop + coast:econ + coast:recr +
                         veg:econ +
                         landuse:pop + landuse:econ + landuse:recr +
                         pop:recr + econ:recr,
                         data = data_z, na.action = "na.fail")

modRed_futureHH <- lm(future_hh_area ~
                        pastHH + coast + veg + landuse + pop + econ + recr + 
                        pastHH:coast + pastHH:veg + pastHH:pop + pastHH:recr +
                        coast:veg + coast:landuse + coast:pop +
                        veg:landuse + veg:recr +
                        pop:econ + pop:recr, 
                      data = data_z, na.action = "na.fail")

summary(modRed_currentHH)
summary(modRed_futureHH)

## Find most parsimonious model by AIC (generally favours models with more parameters)
modRank_currentHH <- MuMIn::dredge(modRed_currentHH, rank = "AICc", extra = c("R^2", "adjR^2", "BIC")) 
write.csv(modRank_currentHH, file = "modRank_reduced_currentHH.csv", row.names = FALSE)

# Least parameters among most parsimonious in terms of AICc and BIC (delta < 2)
#  pastHH + coast + veg + landuse + pop + econ + recr + 
#  coast:pop + coast:econ + coast:recr +
#  landuse:pop + landuse:recr +
#  pop:recr + econ:recr

## Find most parsimonious model by BIC (generally favours models with less parameters)
modRank_futureHH <- MuMIn::dredge(modRed_futureHH, rank = "AICc", extra = c("R^2", "adjR^2", "BIC")) 
write.csv(modRank_futureHH, file = "modRank_reduced_futureHH.csv", row.names = FALSE)

# Least parameters among most parsimonious in terms of AICc
#  pastHH + coast + veg + landuse + pop + recr + 
#  pastHH:coast + pastHH:veg + pastHH:pop + pastHH:recr +
#  coast:veg + coast:landuse + coast:pop +
#  veg:landuse + veg:recr +
#  pop:recr

# Least parameters among most parsimonious in terms of BIC
#  pastHH + coast + veg + landuse +
#  coast:landuse +
#  veg:landuse


