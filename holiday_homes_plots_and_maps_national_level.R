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
library(tmap)
library(factoextra)#derive results from prcomp, pca
#theme_set(theme_minimal())

options(scipen=999)

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

# Time series
# Time series
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
    y = "Holiday homes in Norway",
    #title = "Number of holiday homes in Norway 1970–2022",
    #caption = "Data from SSB (2023) and Vonlanthen (1979)"
  ) +
  theme_test() +
  ylim(0, 500000)

holiday_homes_1970_2022

ggsave("plots/holiday_homes_1970_2022.png", 
	holiday_homes_1970_2022, scale = 2, bg = "white", width = 8, height = 5, units = "cm", dpi = 600)


# Figure 2 a)

#data$future_hh_area <- data$future_hh_area*0.001

data_num <- na.omit(data)

max(data_num$future_hh_area)

ggplot(data, aes(y = future_hh_area+0.001, x = holiday_homes+0.001)) +
	scale_x_continuous(trans='log10', limits=c(100,10000), breaks = c(100, 1000, 10000))+
	scale_y_continuous(trans='log10', limits=c(0.001,100), breaks = c(0,0.1,1, 10, 100))+
	geom_point(pch = 21, fill = "steelblue", alpha = 0.7, size = 4)+
		geom_smooth(method=lm , color="black", se=TRUE)+
	stat_cor(method = "kendall", cor.coef.name = "tau", p.accuracy = 0.001,
   aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
	xlab("Number of holiday homes in the municipalities")+
	ylab(bquote('Designated areas for future holiday homes,'~ km^2))+
	theme_classic()

plot1 <- ggplot(data, aes(y = future_hh_area+0.001, x = holiday_homes+0.001)) +
	scale_x_continuous(trans='log10', limits=c(100,10000), breaks = c(100, 1000, 10000))+
	scale_y_continuous(trans='log10', limits=c(0.001,100), breaks = c(0,0.1,1, 10, 100))+
	geom_point(pch = 21, fill = "steelblue", alpha = 0.7, size = 2)+
	geom_smooth(method=lm , color="black", se=TRUE)+
	stat_cor(method = "kendall", cor.coef.name = "tau", p.accuracy = 0.001,
   aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
	xlab("Number of holiday homes")+
	ylab(bquote('Areas for future holiday homes,'~ km^2))+
	theme_classic()

plot1

cor(data$future_hh_area, data$holiday_homes, use = "pairwise.complete.obs", method = "kendall")
cor(log(data$future_hh_area+1), log(data$holiday_homes+1), use = "pairwise.complete.obs", method = "kendall")


# Figure 2
plot2a <- ggplot(data, aes(x = hh_category, y = holiday_homes)) + 
  scale_y_continuous(trans='log10', limits=c(1,10000), breaks = c(0.01,0.1,1, 10, 100, 1000, 10000)) +
  geom_boxplot(#outlier.colour = "darkgrey",
               outlier.shape = NA,
               # outlier.fill = "darkgrey",
               # outlier.size = 1,
               width = 0.5) +
  geom_jitter(width = 0.2, height = 0, pch = 21, fill = "steelblue", alpha = 0.25, size = 2) +  # Add jittered data points
  scale_x_discrete(labels = c("Coastal", "Forest", "Mountain")) +
  xlab("Type of municipality") +
  ylab('Holiday homes') +
  theme_classic()

plot2a


# Figure 2
plot2b <- ggplot(data, aes(x = hh_category, y = future_hh_area+0.001)) + 
  scale_y_continuous(trans='log10', limits=c(0.001,100), breaks = c(0.01,0.1,1, 10, 100)) +
  geom_boxplot(#outlier.colour = "darkgrey",
               outlier.shape = NA,
               # outlier.fill = "darkgrey",
               # outlier.size = 1,
               width = 0.5) +
  geom_jitter(width = 0.2, height = 0, pch = 21, fill = "steelblue", alpha = 0.25, size = 2) +  # Add jittered data points
  scale_x_discrete(labels = c("Coastal", "Forest", "Mountain")) +
  xlab("Type of municipality") +
  ylab(bquote('Areas for future holiday homes,'~ km^2)) +
  theme_classic()

plot2b


# Publication ready plots -------------------------------------------------

grid.arrange(plot1, NULL, plot2a, NULL, plot2b, nrow = 1, ncol=5)

# figure2 <- ggarrange(plot1, plot2, plot3, 
#           labels = c("a)", "b)", "c)"),
#           nrow = 1, ncol=5, widths = c(1, 0.05, 1, 0.05, 1))
# 
# ggarrange(
#   plot1, NULL, plot2, NULL, plot3,
# 	vjust = 2,
#   nrow = 1, widths = c(6, 1, 6, 1,6),
#           labels = c("a)", "","b)","", "c)"))

figure2 <- ggarrange(
  plot1, NULL, plot2a, NULL, plot2b,
	vjust = 2,
  nrow = 1, widths = c(12, 1, 12, 1, 12),
          labels = c("a)", "","b)","", "c)"))

figure2

	
ggsave("plots/figure_overview.png", 
	figure2, scale = 2, bg = "white", width = 20, height = 6, units = "cm", dpi = 600)


# Maps --------------------------------------------------------------------

# Spatial data ------------------------------------------------------------

# 
# # Nxkkel:
# k_nummer_k_navn <- read.csv("P:/15022000_egenutvikling_trond_simensen/Spatial_regression/kommunenummer_kommunenavn_01012020.csv", header = TRUE, sep = ";")
# names(k_nummer_k_navn) <- c("kommunenummer", "kommune")
# length(unique(k_nummer_k_navn$Kommunenavn))

# Reading in basic spatial data -------------------------------------------

fgdb <- "data/Basisdata_0000_Norge_25833_N5000Kartdata_FGDB.gdb"
st_layers(fgdb, options = character(0), do_count = FALSE)
norske_kommuner <- st_read(fgdb,layer = "N5000_AdministrativeOmråder_omrade") %>% 
   rename(region = kommunenummer) %>% 
   select(region) #%>% st_simplify(preserveTopology = TRUE, dTolerance = 1000)

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

data

komm <- left_join(norske_kommuner, data, by = "region")
plot(komm, max.plot = 6)


nk <- st_drop_geometry(komm)
nk



hh_type <- komm |> filter(hh_category == "Mountain")

map1 <- tm_shape(komm)+
  tm_fill (col = "holiday_homes", palette = "Reds",style = "quantile", 
    legend.show = F, legend.size.show = FALSE, legend.hist=T, title = " ")+
    tm_shape(komm)+
  tm_fill(col = "white", alpha = 1)+
  tm_shape(komm)+
  tm_fill(col = "grey", alpha = 0.3)+
      tm_shape(hh_type)+
  tm_fill(col = "grey", alpha = 0.5)+
  tm_shape(hav)+
  tm_fill(col = "lightblue", alpha = 0.7)+
tm_shape(komm)+
  tm_borders(col = "darkgrey", lwd = 0.5)+
  tm_bubbles(size = "holiday_homes", 
    col = "red", 
    style = "quantile",
    border.col = "black", border.alpha = .5,
    legend.hist=T,
    title.size = ("Holiday homes"), #title
    legend.size.show = T,  
    legend.col.show = T)+
    tm_layout(title = "a)",  main.title.size = 0.95, frame = FALSE, legend.outside = FALSE)

map1

map2 <- tm_shape(komm)+
  tm_fill (col = "future_hh_area", palette = "Reds",style = "quantile", 
    legend.show = F, legend.size.show = FALSE, legend.hist=T, title = " ")+
    tm_shape(komm)+
  tm_fill(col = "white", alpha = 1)+
  tm_shape(komm)+
  tm_fill(col = "grey", alpha = 0.3)+
    tm_shape(hh_type)+
  tm_fill(col = "grey", alpha = 0.5)+
  tm_shape(hav)+
  tm_fill(col = "lightblue", alpha = 0.7)+
tm_shape(komm)+
  tm_borders(col = "darkgrey", lwd = 0.5)+
  tm_bubbles(size = "future_hh_area", 
    col = "red", 
    style = "quantile",
    border.col = "black", border.alpha = .5,
    legend.hist=T,
    title.size = expression("Future holiday home area, km"^2*""), #title
    legend.size.show = T,  
    legend.col.show = T)+
    tm_layout(title = "b)",  main.title.size = 0.95, frame = FALSE, legend.outside = FALSE)

map2

map3 <- tmap_arrange(map1, map2, nrow=1)

map3

tmap_save(map3, filename = "plots/maps.png", height = 12, width = 20, units = "cm", dpi = 600)

tmap_save(map3, filename = "plots/maps.png", height = 18, width = 30, units = "cm", dpi = 600)

getwd()

# Correct for Kinn kommune, split in two geographic areas
# kinn <- natur %>% filter (Region == 4602)
# kinn$Region <- as.numeric(kinn$Region)
# kinn <- as.data.frame(colMeans(kinn)) 
# kinn <- t(kinn)
# rownames(kinn) <- 4602
# kinn <- as.data.frame(kinn)
# kinn$Region <- as.character(kinn$Region)




