##### New Mexico Climate Impacts Code
# Author: Rhea Cong
# Last edited 06-06-2024
# This code investigates NM's climate vulnerability, energy burden and
# energy poverty

library(tidyr)
library(dplyr)
library(tidycensus)
library(tidyverse)
library(readxl)

# Read in CEJST data from https://screeningtool.geoplatform.gov/en/downloads
cejstdata <- read.csv("1.0-communities.csv")

# Reduce to columns of interest
idcols <- c(1, 2, 3)
disadvcols <- c(15, 20, 21, 22)
demographicscols <- c(23, 26, 117, 120, 122)
energycols <- c(45, 46, 47)

cejstdatasmall <- cejstdata[,c(idcols, disadvcols, demographicscols, energycols)]

# Comparing percent of tracts that are disadvantaged and population-weighted measures of climate vulnerability
cejstdatasmall$Identified.as.disadvantaged <- as.logical(cejstdatasmall$Identified.as.disadvantaged)
cejstdatasmall$Greater.than.or.equal.to.the.90th.percentile.for.energy.burden.and.is.low.income. <-
  as.logical(cejstdatasmall$Greater.than.or.equal.to.the.90th.percentile.for.energy.burden.and.is.low.income.)

statecomparison <- cejstdatasmall %>%
  group_by(State.Territory) %>%
  summarise(percent.tracts.disadvantaged = sum(Identified.as.disadvantaged)/n(),
            percent.tracts.90th.energyburden.lowincome = sum(Greater.than.or.equal.to.the.90th.percentile.for.energy.burden.and.is.low.income.)/n(),
            popweighted.unemployment.avg = weighted.mean(Unemployment..percent., Total.population, na.rm=T),
            popweighted.energyburden.percentile.avg = weighted.mean(Energy.burden..percentile., Total.population, na.rm=T),
            popweighted.energyburden.avg = weighted.mean(Energy.burden, Total.population, na.rm=T),
            popweighted.individuals.below.poverty = weighted.mean(Percent.of.individuals...100..Federal.Poverty.Line, Total.population, na.rm=T))

write.csv(statecomparison, file = "NMClimateVulnerability")

# Comparing to states in same division
census_divisions <- read.csv('https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv')
mountainstates <- census_divisions$State[census_divisions$Division == "Mountain"]
divisionstatecomparison <- statecomparison %>%
  filter(State.Territory %in% mountainstates)

write.csv(divisionstatecomparison, file = "MountainStatesEnergy")

# NCA5 Data for climate change projections
NCA3 <- read.csv("./NCA_Atlas_Global_Warming_Level_5_deg_F_7660615008925248363.csv")
mountainnca <- NCA3 %>%
  filter(State.name %in% mountainstates)

# Load and join population data for counties by FIPS code
countypop <- get_acs(geography = "county", 
                     variable = "B01001_001",
                     year = 2022,
                     survey = "acs5",
                     geometry = FALSE)

countypop$GEOID <- as.numeric(countypop$GEOID)

colnames(countypop)[1] <- "FIPS"

mountaincountyavgpop <- left_join(mountainnca, countypop, by = "FIPS")
countyavgpop <- countyavgpop[,-26]

# Calculate weighted.mean for change in annual average temperature
mountainncastats <- mountaincountyavgpop %>%
  group_by(State.abbreviation) %>%
  summarize(popweightedtemp = weighted.mean(Temperature.average, estimate, na.rm=T),
            popweightedsummertemp = weighted.mean(Temp.mean.summer, estimate, na.rm=T),
            popweighted100Fdays = weighted.mean(Temp.Days.100.F, estimate, na.rm=T),
            popweightedprecip = weighted.mean(Precip.Annual.GWL3, estimate, na.rm=T),
            popweightedprecip5yr = weighted.mean(Precip.5.year.max, estimate, na.rm=T),
            areaweightedprecip = weighted.mean(Precip.Annual.GWL3, Shape__Area, na.rm=T))

write.csv(mountainncastats, file = "MountainDivisionStatsNCA")

# Highlighting energy burden by county in NM
NMenergyburden <- cejstdatasmall %>%
  filter(State.Territory == "New Mexico") %>%
  group_by(County.Name) %>%
  summarise(popweightedenergyburden = weighted.mean(Energy.burden, Total.population, na.rm=T),
            popweightedenergyburdenpercentile = weighted.mean(Energy.burden..percentile., Total.population, na.rm=T))

# Join by FIPS for mapping
mountaindivfips <- mountaincountyavgpop %>%
  select(County.name, State.name, FIPS)

colnames(mountaindivfips)[1] <- "County.Name"

nmfips <- mountaindivfips %>%
  filter(State.name == "New Mexico")

NMenergyburden <- left_join(NMenergyburden, nmfips, by = "County.Name") 

write.csv(NMenergyburden, file = "NMEnergyBurdenCounties")

# Compare to national avg energy burden
nationalenergyburden <- cejstdatasmall %>%
  summarise(popweightedenergyburden = weighted.mean(Energy.burden, Total.population, na.rm=T))
# = 2.55%

# Read in climate and social vulnerability data from https://www.epa.gov/cira/technical-appendices-and-data
laborhourslost <- read_excel("sv-labor-2021-07-09.xlsx", sheet = "Labor Hours Lost - Impacts", col_names=F)
colnames(laborhourslost) <- laborhourslost[2,]
laborhourslost <- laborhourslost[-2:-1,1:6]
colnames(laborhourslost)[1] <- "GEOID"
threedeglaborlost <- laborhourslost[,c(1,4)]

laborhoursrisk <- read_excel("sv-labor-2021-07-09.xlsx", sheet = "Labor Hours Lost - Risk", col_names=F)
colnames(laborhoursrisk) <- laborhoursrisk[2,]
laborhourslost <- laborhourslost[-2:-1,1:6]
colnames(laborhourslost)[1] <- "GEOID"
threedeglaborlost <- laborhourslost[,c(1,4)]

tempmortality <- read_excel("sv-temperature-mortality-2021-07-09.xlsx", sheet = "Temperature Mortality - Impact", col_names=F)
colnames(tempmortality) <- tempmortality[2,]
tempmortality <- tempmortality[-2:-1,1:6]
colnames(tempmortality)[1] <- "GEOID"
threedegtempmortality <- tempmortality[,c(1,4)]

# Get census tract IDs
nmtractspop <- get_acs(geography = "tract", 
                     variable = "B01001_001",
                     year = 2022,
                     survey = "acs5",
                     state = "NM",
                     geometry = FALSE)

# Join datasets to census data
nmlaborhourslost <- left_join(nmtractspop, threedeglaborlost, by = "GEOID")
nmtempmortality <- left_join(nmtractspop, threedegtempmortality, by = "GEOID")

nmlaborhourslost$County <- substr(nmlaborhourslost$GEOID, start = 1, stop = 5)
colnames(nmlaborhourslost)[6] <- "hours"
nmlaborhourslost$hours <- as.numeric(nmlaborhourslost$hours)

nmtempmortality$County <- substr(nmtempmortality$GEOID, start = 1, stop = 5)
colnames(nmtempmortality)[6] <- "mortality"
nmtempmortality$mortality <- as.numeric(nmtempmortality$mortality)

# Take population weight and fold up by county
NMweightedlaborhourslost <- nmlaborhourslost %>%
  group_by(County) %>%
  summarise(popweightlaborhourslost = weighted.mean(hours, estimate, na.rm=T))

NMweightedtempmortality <- nmtempmortality %>%
  group_by(County) %>%
  summarise(popweightmortality = weighted.mean(mortality, estimate, na.rm=T)) # mostly NA data

# Write csv
write.csv(NMweightedlaborhourslost, file = "NMCountyLaborCost")
