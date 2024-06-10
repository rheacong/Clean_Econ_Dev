#Generic Setup for CRED analysis

#Set the Working Directory to your Username
setwd("C:/Users/LCarey.RMI/")



# Libraries
library(bea.R)
library(downloader)
library(shiny)
library(shinycssloaders)
library(tidyverse)
library(ggplot2)
library(zoo)
library(usmapdata)
library(lubridate)
library(readxl)
library(httr)
library(censusapi)
library(httr)
library(data.table)
library(magrittr)
library(stringr)
library(ggmap)
library(data.table)
library(ggthemes)
library(wesanderson)
library(RColorBrewer)
library(chattr)
library(usmap)
library(maps)
library(sf)
library(grDevices)
library(officer)
library(rvg)
library(jsonlite)
library(blsAPI)
library(ggrepel)
library(fuzzyjoin)
library(tigris)

rmi_palette<-c("#0BD0D9",
               "#0989B1",
               "#003A61",
               "#FFCA08",
               "#F8931D",
               "#548538",
               "#7F7F7F")

# Adding distinct colors manually
additional_colors <- c("#E63946", "#F4A261", "#2A9D8F", "#264653", "#E76F51", "#8D99AE", "#9C6644","#6A4C93",  # Purple
                       "#F77F00",  # Vivid orange
                       "#80B918",  # Lime green
                       "#005F73",  # Dark cyan
                       "#9D4EDD",  # Lavender
                       "#EF233C")   # Crimson

# Combine existing and additional colors
expanded_palette <- c(rmi_palette, additional_colors)


#Geographies
census_divisions<- read.csv('https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv')
us_counties <-us_map("counties")
counties <- counties(class = "sf")
county_gdp<- read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/county_gdp_2022.csv',skip=3)
state_gdp<- read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/state_gdp_22.csv',skip=3)
msa_gdp<- read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/msa_gdp_2022.csv',skip=3)
states_simple <- read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/US Maps etc/Regions/rmi_regions.csv')
county_cbsa<-read.csv("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/US Maps etc/Regions/csa_cbsa_county.csv",skip=2)
EAs<-read_excel("RMI/US Program - Regional Investment Strategies/Great Lakes Investment Strategy/Reference Data/BEA Economic Areas and Counties.xls",2)
EAs<-EAs %>%
  mutate(fips=as.numeric(FIPS))
EA_gdp <- EAs %>%
  inner_join(county_gdp,by=c("fips"="fips")) %>%
  mutate(gdp_22=as.numeric(X2022)) %>%
  group_by(`EA Name`) %>%
  summarize_at(vars(gdp_22),sum,na.rm=T) 


#NAICS Codes
file_url <- 'https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx'
temp_file <- tempfile(fileext = ".xlsx")
GET(url = file_url, write_disk(temp_file, overwrite = TRUE))
naics2017 <- read_excel(temp_file, sheet = 1)  # 'sheet = 1' to read the first sheet

naics2022<-read_excel('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/2022_to_2017_NAICS.xlsx',1,skip=2)


