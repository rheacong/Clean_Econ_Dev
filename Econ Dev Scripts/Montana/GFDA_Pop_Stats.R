### GF Population Analysis
# Author: Rhea Cong, 08/14/2024

# Packages
library(tidycensus)
library(tidyverse)
library(dpylr)
Sys.setenv(CENSUS_KEY='0b3d37ac56ab19c5a65cbc188f82d8ce5b36cfe6')

census_api_key("0b3d37ac56ab19c5a65cbc188f82d8ce5b36cfe6")

# Age demographics
variables <- rep("B01001_0", 49)
variables <- paste0(variables, c("01","02","03","04","05","06","07","08","09", 10:49), rep("E",49))
age_data <- getCensus(
  name = "acs/acs5",
  vintage = 2022,
  vars = c("NAME", variables),
  region = "county:*",
  regionin = "state:30"  # MT is 30
)

age_data_sum <- age_data %>%
  mutate(County_name = gsub("Montana", "MT", NAME)) %>%
  filter(County_name %in% region_id$County) %>%
  summarise(
    PopUnder_18_2022 = sum(c_across(c(6:9, 30:33)), na.rm = TRUE),
    Pop18_64_2022 = sum(c_across(c(10:22, 34:46)), na.rm = TRUE),
    Pop65_plus_2022 = sum(c_across(c(23:28, 47:52)), na.rm = TRUE),
    Total_Pop = sum(B01001_001E, na.rm = TRUE)
  )

# Population growth
# Years you want to analyze
years <- c(2010, 2015, 2020, 2022)

# Initialize an empty list to store data
population_data_list <- list()

# Loop through the years to retrieve data
for (year in years) {
  data <- getCensus(
    name = "acs/acs5",
    vintage = year,
    vars = c("NAME", "B01001_001E"),
    region = "county:*",
    regionin = "state:30"
  )
  data$year <- year
  population_data_list[[as.character(year)]] <- data
}

# Combine all years into a single data frame
population_data <- bind_rows(population_data_list)

# Mutate for GF
population_data$NAME<-gsub("Montana", "MT", population_data$NAME)

population_growth <- population_data %>%
  filter(NAME %in% region_id$County) %>%
  select(-state, -county) %>%
  group_by(year) %>%
  summarise(Total_Pop = sum(B01001_001E)) %>%
  pivot_wider(names_from = year, values_from = Total_Pop)

GF_pop <- cbind(age_data_sum, population_growth) %>% select(-Total_Pop)
GF_pop$Pop_Growth_2010_22 <- GF_pop$`2022`/GF_pop$`2010`

# Disadvantaged communities and other demographics
# Read in CEJST data from https://screeningtool.geoplatform.gov/en/downloads
cejstdata <- read.csv("./Data/1.0-communities.csv")
cejstdata$County.Name <- paste0(cejstdata$County.Name, ", MT")
cejst_GF <- cejstdata[cejstdata$County.Name %in% region_id$County,]
cejst_GF <- cejst_GF %>%
  filter(State.Territory == "Montana")

disadvcols <- c(20, 21)
demographicscols <- c(9, 12, 13, 14, 23, 26, 117, 120, 122)
energycols <- c(46, 47)

cejst_GF_small <- cejst_GF[,c(1,2, disadvcols, demographicscols, energycols)]

GF_pop <- cbind(GF_pop, cejst_GF_small %>%
  summarise(across(
    .cols = c(5,11:15),               # Columns to calculate weighted mean
    .fns = ~ weighted.mean(.x, Total.population, na.rm = T),     # Function to apply
    .names = "weighted_mean_{col}"           # Naming convention for output columns
  ))
)

GF_pop$Disadvantaged_Tracts <- sum(cejst_GF_small$Identified.as.disadvantaged=="True")/nrow(cejst_GF_small)

# Save csvs
write.csv(GF_pop, file="./GFDA/GF_population_stats.csv")
write.csv(cejst_GF_small, file="./GFDA/GF_county_cejst_data.csv")
