library(tidyverse)
options(scipen = 999)

# Your data
time_data <- tribble(
    ~year, ~dwellings, ~holiday_homes,
    1970, 1296734, 188977,
		1977, NA, 239743,
    1980, 1523508, NA,
    2006, 2214770, 367438,
    2007, 2242651, 371340,
    2008, 2274362, 376817,
    2009, 2300739, 381799,
    2010, 2323925, 386472,
    2011, 2343010, 393221,
    2012, 2368762, 397541,
    2013, 2399274, 400494,
    2014, 2426767, 403698,
    2015, 2456304, 406464,
    2016, 2485353, 409867,
    2017, 2515589, 413662,
    2018, 2547732, 417672,
    2019, 2581155, 421392,
    2020, 2610040, 437631,
    2021, 2637838, 440241,
    2022, 2666507, 445513
)

# Reshape the data
long_data <- time_data %>%
    pivot_longer(cols = c(dwellings, holiday_homes), names_to = "type", values_to = "count")

hh_development <- time_data |> select(-dwellings) |> 
	filter(year != 1980)

holiday_homes_1970_2022 <- ggplot(hh_development) +
  aes(x = year, y = holiday_homes) +
  geom_line(colour = "#112446", lwd = 0.5) +
  	geom_point(shape = 21, col = "white", fill = "#112446", size = 1.5, stroke = 0.5)+
  labs(
    x = "Year",
    y = "Holiday homes",
    #title = "Number of holiday homes in Norway 1970–2022",
    #caption = "Data from SSB (2023) and Vonlanthen (1979)"
  ) +
  theme_test() +
  ylim(0, 500000)

holiday_homes_1970_2022

ggsave("plots/holiday_homes_1970_2022.png", 
	holiday_homes_1970_2022, scale = 2, bg = "white", width = 8, height = 5, units = "cm", dpi = 600)




# Slope graphs ------------------------------------------------------------

Sys.setlocale("LC_CTYPE", "norwegian")
options(scipen=999)

# Reading in master data set
data <- read.csv("data/holiday_home_data.csv", 
	header = TRUE, sep = ",", encoding = "latin") |> as_tibble()

# Recoding variables and variable types
data$region <- as.character(data$region)
data$region[data$region == "301"] <- "0301"
glimpse(data)

data$hh_category = as.factor(data$hh_category)
levels(data$hh_category) <- c("Coastal", "Forest", "Mountain")
data$hh_category

data <- data |> select(region, hh_category)
data
# reading in time series data

changes <- read.csv("data/time_series_holiday_homes_1970_2022.csv", 
	header = TRUE, sep = ",") |> as_tibble()

changes

changes$hh_1970 <- as.integer(changes$hh_1970)
changes$hh_1977 <- as.integer(changes$hh_1977)

changes

#changes <- changes |> rename(Region = X)
changes$region <- as.character(changes$region)
changes$region[changes$region == "301"] <- "0301"
changes

change <- left_join(data, changes, by = "region")
change

hh_dev <- change |> filter(hh_category == "Mountain") |> select(municipality, county, hh_1970, hh_2022) |> 
  filter(municipality != "Hole")

hh_dev <- hh_dev |> mutate(change = hh_2022 - hh_1970)

# Pivoting the changes
long_changes <- hh_dev %>%
    pivot_longer(
        cols = starts_with("hh_"), # selecting the columns to pivot
        names_to = "year", # column name for the 'key' column
        values_to = "hh_dev" # column name for the 'value' column
    ) %>%
    mutate(year = str_remove(year, "hh_")) # removing the 'hh_' prefix from the year column

# Display the result
long_changes

# install.packages("CGPfunctions")
library(CGPfunctions)

newggslopegraph(dataframe = long_changes,
                Times = year,
                Measurement = hh_dev,
                Grouping = municipality)


hhdev_2 <- hh_dev |> mutate(growth_rate = ((hh_2022 - hh_1970)/hh_1970)*100)

hhdev_2 <- hh_dev |> 
  mutate(growth_rate = round(((hh_2022 - hh_1970) / hh_1970) * 100, 1))

hhdev_2

holiday_home_growth_rate_mountains <- ggplot(hhdev_2) +
  aes(x = reorder(municipality, growth_rate), y = growth_rate) +
  geom_col(fill = "#112446") +
  coord_flip() +
  theme_minimal()+
  labs(
    x = "Municipality",
    y = "Growth rate in percentages, holiday homes sice 1970",
    #title = "Number of holiday homes in Norway 1970–2022",
    #caption = "Data from SSB (2023) and Vonlanthen (1979)"
  ) 

holiday_home_growth_rate_mountains

holiday_home_growth_rate_mountains <- ggplot(hhdev_2) +
  aes(x = reorder(municipality, growth_rate), y = growth_rate, fill = county) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "Municipality",
    y = "Growth rate in percentages, holiday homes since 1970"
    # title = "Number of holiday homes in Norway 1970–2022",
    # caption = "Data from SSB (2023) and Vonlanthen (1979)"
  )

# Display the plot
print(holiday_home_growth_rate_mountains)


ggsave("plots/holiday_home_growth_rate_mountains_since_1970.png", 
	holiday_home_growth_rate_mountains, scale = 2, bg = "white", width = 10, height = 15, units = "cm", dpi = 600)


holiday_homes_since_1970 <- ggplot(hhdev_2) +
  aes(x = reorder(municipality, change), y = change) +
  geom_col(fill = "#112446") +
  coord_flip() +
  theme_minimal()+
  labs(
    x = "Municipality",
    y = "New holiday homes sice 1970",
    #title = "Number of holiday homes in Norway 1970–2022",
    #caption = "Data from SSB (2023) and Vonlanthen (1979)"
  ) 

holiday_homes_since_1970

# pdf(file = "tables/correlations_all_variables_Norway.pdf", width = 16.5, height = 11.7)
# corrplot(correlations, method = 'number', order = "hclust", type = "lower", 
# title = "Kendall Correlation Matrix, all variables", 
# mar = c(0,0,1,0), number.cex = 0.5, number.digits = 2, tl.cex = 0.7)

# dev.off()

ggsave("plots/holiday_homes_since_1970.png", 
	holiday_homes_since_1970, scale = 2, bg = "white", width = 10, height = 15, units = "cm", dpi = 600)



