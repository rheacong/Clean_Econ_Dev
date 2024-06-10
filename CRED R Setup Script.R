#Generic Setup for CRED analysis

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

states_simple <- read.csv('C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/US Maps etc/Regions/rmi_regions.csv')
states_simple <- states_simple %>%
  mutate(region=ifelse(full=="Hawaii","west",region))

census_divisions<- read.csv('https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv')
us_counties <-us_map("counties")
counties <- counties(class = "sf")
county_gdp<- read.csv('C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/county_gdp_2022.csv',skip=3)
state_gdp<- read.csv('C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/state_gdp_22.csv',skip=3)
msa_gdp<- read.csv('C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/msa_gdp_2022.csv',skip=3)
states_simple <- read.csv('C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/US Maps etc/Regions/rmi_regions.csv')
county_cbsa<-read.csv("C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/US Maps etc/Regions/csa_cbsa_county.csv",skip=2)
EAs<-read_excel("C:/Users/LCarey.RMI/RMI/US Program - Regional Investment Strategies/Great Lakes Investment Strategy/Reference Data/BEA Economic Areas and Counties.xls",2)
EAs<-EAs %>%
  mutate(fips=as.numeric(FIPS))
EA_gdp <- EAs %>%
  inner_join(county_gdp,by=c("fips"="fips")) %>%
  mutate(gdp_22=as.numeric(X2022)) %>%
  group_by(`EA Name`) %>%
  summarize_at(vars(gdp_22),sum,na.rm=T) 

#Downscaling
great_falls<-EAs %>%
  filter(`EA Name`=="Great Falls, MT"|
           County %in% c("Judith Basin County, MT",
                         "Fergus County, MT",
                         "Lewis and Clark County, MT"))

region_id <- great_falls

EAs<-EAs%>%
  mutate(`EA Name`=ifelse(FIPS %in% great_falls$FIPS,"Great Falls, MT",`EA Name`)) %>%
  left_join(counties %>% 
              select(STATEFP,COUNTYFP) %>%
              mutate(FIPS=paste0(STATEFP,COUNTYFP)),by=c("FIPS"="FIPS")) %>%
  mutate(statefp=as.numeric(STATEFP)) %>%
  left_join(states_simple,by=c("statefp"="fips")) %>%
  select(-geometry) 


region_counties<-us_counties %>%
  filter(fips %in% region_id$FIPS) %>%
  left_join(states_simple,by=c("abbr"="abbr","full"="full"))

cbp_2021 <- getCensus(
  name = "cbp",
  vars=c("STATE",
         "COUNTY",
         "NAICS2017",
         "SECTOR",
         "SUBSECTOR",
         "INDLEVEL",
         "ESTAB",
         "EMP",
         "PAYANN"),
  region = "county:*",
  vintage = 2021)
cbp_21<-cbp_2021
cbp_21$state<-as.numeric(cbp_21$state)
cbp_21<-left_join(states_simple,cbp_21,by=c("fips"="state"))

cbp_21$NAICS2017<-as.numeric(cbp_21$NAICS2017)
cbp_21 <-left_join(cbp_21,eti,by=c("NAICS2017"="6-Digit Code"))

cbp_21 <- cbp_21 %>%
  filter(INDLEVEL=="6")

cbp21_2d <- cbp_2021 %>%
  mutate(state=as.numeric(state)) %>%
  filter(INDLEVEL=="2")  %>%
  mutate(FIPS=paste0(STATE, COUNTY)) %>%
  left_join(EAs,by=c("FIPS"="FIPS")) %>%
  left_join(naics2017 %>% select(`2017 NAICS US   Code`,`2017 NAICS US Title`),by=c("NAICS2017"="2017 NAICS US   Code")) %>%
  rename(naics_desc=`2017 NAICS US Title`)


region_cbp_2d <- cbp21_2d %>%
  filter(abbr==state_abbreviation) %>%
  mutate(region_id=ifelse(fips %in% region_id$fips,1,0)) %>%
  mutate(code=ifelse(NAICS2017 %in% c("00","11","21","22","23","31-33","42","48-49","54"),NAICS2017,"Other")) %>%
  group_by(region_id,code) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  group_by(code) %>%
  mutate(share=EMP/sum(EMP,na.rm=T)) 

region_totalemp<-region_cbp_2d %>%
  filter(code=="00",
         region_id==1) 

#NREL SLOPE Data - Energy COnsumption by County and Region
county_elec_cons <- read.csv("C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/energy_consumption_expenditure_business_as_usual_county.csv")
county_eleccons <- county_elec_cons %>%
  filter(Year=="2022" & Source=="elec") %>%
  select(County.Name,State.Name,Geography.ID,Sector,Consumption.MMBtu) %>%
  mutate(STATE=as.numeric(substr(Geography.ID,2,3)),
         COUNTY=as.numeric(substr(Geography.ID,4,7))) %>%
  group_by(STATE,COUNTY) %>%
  summarize_at(vars(Consumption.MMBtu),sum,na.rm=T) %>%
  left_join(county_pop %>% 
              select(STATE,COUNTY) %>% 
              filter(COUNTY!="0"),
            by=c("STATE"="STATE",
                 "COUNTY"="COUNTY")) %>%
  mutate(FIPS=paste0(sprintf("%02d", STATE), sprintf("%03d", COUNTY))) %>%
  left_join(EAs,by=c("FIPS"="FIPS")) %>%
  left_join(county_cbsa,by=c("fips"="fips")) %>%
  select(region,abbr,full,County,`EA Name`,CBSA.Title,FIPS,fips,Consumption.MMBtu)

msa_eleccons <- county_eleccons %>% 
  group_by(CBSA.Title) %>%
  summarize_at(vars(Consumption.MMBtu),sum,na.rm=T)
ea_eleccons <- county_eleccons %>% 
  group_by(`EA Name`) %>%
  summarize_at(vars(Consumption.MMBtu),sum,na.rm=T)

region_eleccons<-county_eleccons %>%
  filter(abbr== state_abbreviation) %>%
  mutate(region_id=ifelse(FIPS %in% region_id$FIPS,1,0)) %>%
  group_by(`EA Name`,region_id) %>%
  summarize_at(vars(Consumption.MMBtu),sum,na.rm=T) %>%
  ungroup() %>%
  mutate(share=Consumption.MMBtu/sum(Consumption.MMBtu,na.rm=T)) %>%
  filter(region_id==1)


#EIA Data - Electricity Capacity by County and Region
url <- 'https://www.eia.gov/electricity/data/eia860m/xls/march_generator2024.xlsx'
destination_folder<-"C:/Users/LCarey.RMI/Downloads/"
file_path <- paste0(destination_folder, "eia_op_gen.xlsx")
downloaded_content <- GET(url, write_disk(file_path, overwrite = TRUE))
op_gen <- read_excel(file_path, sheet = 1,skip=2)
counties <- counties(class = "sf")
op_gen_clean <- op_gen %>% 
  filter(!is.na(Latitude) & !is.na(Longitude))
op_gen_sf <- st_as_sf(op_gen_clean, coords = c("Longitude", "Latitude"), crs = 4326)
op_gen_sf <- st_transform(op_gen_sf, crs = st_crs(counties))

op_gen_with_county <- st_join(op_gen_sf, counties)

EA_gen <- op_gen_with_county %>%
  mutate(fips=as.numeric(GEOID)) %>%
  inner_join(EAs,by=c("fips"="fips")) %>%
  filter(Status=="(OP) Operating") %>%
  group_by(region,full,`EA Name`,`Operating Year`,Technology) %>%
  summarize_at(vars(`Nameplate Capacity (MW)`),sum,na.rm=T) 

region_rengen <- EA_gen %>%
  as.data.frame(.) %>%
  filter(region %in% region_counties$region) %>%
  mutate(tech = case_when(
    Technology=="Natural Gas Steam Turbine" ~ "Natural Gas",
    Technology=="Natural Gas Fired Combined Cycle" ~ "Natural Gas",
    Technology=="Natural Gas Internal Combustion Engine" ~ "Natural Gas",
    Technology=="Natural Gas Fired Combustion Turbine" ~ "Natural Gas",
    Technology=="Conventional Steam Coal" ~ "Coal",
    Technology=="Conventional Hydroelectric" ~ "Hydro",
    Technology=="Onshore Wind Turbine" ~ "Wind",
    Technology=="Batteries" ~ "Storage",
    Technology=="Solar Photovoltaic" ~ "Solar",
    Technology=="Solar Thermal with Energy Storage" ~ "Solar",
    Technology=="Hydroelectric Pumped Storage" ~ "Hydro",
    Technology=="Geothermal" ~ "Geothermal",
    Technology=="Wood/Wood Waste Biomass"~"Biomass"
  )) %>%
  group_by(full,`EA Name`, tech) %>%
  summarize(`Nameplate Capacity (MW)` = sum(`Nameplate Capacity (MW)`, na.rm = TRUE)) 
#complete(`Operating Year` = 2013:2024, fill = list(`Nameplate Capacity (MW)` = 0)) %>%
#mutate(Year = make_date(`Operating Year`)) %>%

rengen_share <- region_rengen %>%
  mutate(region_of_interest=ifelse(`EA Name`==region_name,1,0)) %>%
  filter(full==state_name) %>%
  group_by(tech) %>%
  mutate(share=round(`Nameplate Capacity (MW)`/sum(`Nameplate Capacity (MW)`),1)) 



#Net Zero Scenario - Net Zero America
file_url <- 'https://netzeroamerica.princeton.edu/data/nzap-data.csv'
temp_file <- tempfile(fileext = ".csv")
GET(url = file_url, write_disk(temp_file, overwrite = TRUE))
nza <- read.csv(temp_file)  # 'sheet = 1' to read the first sheet

nza<-nza %>%
  mutate(geo=str_to_title(geo)) 
nzap<-left_join(nza,states_simple,by=c("geo"="full"))

#States
nza_states<-nzap %>%
  filter(scenario %in% c("REF","E+","E+RE+")) %>%
  drop_na(value) %>%
  filter(geo != "national") %>%
  group_by(year,geo,abbr,scenario,filter_level_1,filter_level_2,filter_level_3,variable_name,unit) %>%
  summarize_at(vars(value),sum) %>%
  spread(year,value) %>%
  filter(!is.na(abbr)) %>%
  mutate(scenario = factor(scenario, levels = c("REF", "E+", "E+RE+")))

#Jobs by Economic Sector
nza_jobs_econ <- nza_states %>%
  ungroup()%>%
  filter(abbr==state_abbreviation) %>%
  filter(filter_level_2=="Jobs",
         filter_level_3=="By economic sector") %>%
  mutate(variable_name=gsub("By economic sector - ","",variable_name)) %>%
  select(abbr,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
  pivot_longer(cols=c(`2025`,`2030`,`2035`,`2040`,`2045`,`2050`),names_to="year",values_to="Value") %>%
  mutate(code=case_when(
    variable_name=="Agriculture"~"11",
    variable_name=="Mining"~"21",
    variable_name=="Utilities"~"22",
    variable_name=="Construction"~"23",
    variable_name=="Manufacturing"~"31-33",
    variable_name=="Trade"~"42",
    variable_name=="Pipeline"~"48-49",
    variable_name=="Professional"~"54",
    variable_name=="Other"~"Other"
  ))

nza_jobs_region <- nza_jobs_econ %>%
  left_join(region_cbp_2d %>% filter(region_id==1) %>% select(code,share),by=c("code"="code")) %>%
  mutate(Value=Value*share) 


plot_nza_jobs_econ<-ggplot(data=nza_jobs_region, aes(x=year,y=Value,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Job Creation in", region_name,",","in a Net Zero Scenario"), 
       x="Year", y="Jobs",
       fill="Economic Sector",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="bottom")

#Jobs by Resource Sector
nza_jobs_resource <- nza_states %>%
  ungroup()%>%
  filter(abbr==state_abbreviation) %>%
  filter(filter_level_2=="Jobs",
         filter_level_3=="By resource sector") %>%
  mutate(variable_name=gsub("By resource sector - ","",variable_name)) %>%
  select(abbr,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
  pivot_longer(cols=c(`2025`,`2030`,`2035`,`2040`,`2045`,`2050`),names_to="year",values_to="Value")

nza_jobs_resource_region <- nza_jobs_resource %>%
  mutate(Value=Value*region_totalemp$share) 


plot_nza_jobs_resource<-ggplot(data=nza_jobs_resource_region, aes(x=year,y=Value,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Job Creation in", region_name,",","in a Net Zero Scenario"), 
       x="Year", y="Jobs",
       fill="Resource Sector",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="bottom")


#Capital Invested in Electricity Generation Chart
nza_cap_inv <- nza_states %>%
  filter(abbr==state_abbreviation) %>%
  filter(filter_level_3=="Capital invested" ) %>%
  filter(!grepl("Constrained",variable_name)) %>%
  mutate(variable_name=gsub("Capital invested - ","",variable_name),
         variable_name=gsub(" - Base","",variable_name)) %>%
  ungroup() %>%
  select(abbr,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
  pivot_longer(cols=c(`2025`,`2030`,`2035`,`2040`,`2045`,`2050`),names_to="year",values_to="Value") %>%
  mutate(variable=ifelse(grepl("Biomass",variable_name),"Biomass",variable_name)) %>%
  left_join(rengen_share  %>%
              filter(region_of_interest==1) %>%
              select(tech,share),by=c("variable"="tech")) %>%
  mutate(Value_region=Value*share) 

plot_nza_ren_capinv<-ggplot(data=nza_cap_inv,aes(x=year,y=Value_region,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Capital invested in ", region_name, "in electricity generation in a Net Zero Scenario"),
       subtitle = "Downscaled investment figures based on existing share of statewide electricity capacity additions.",
       x="Year", y="Billion $ 2018",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()


#Generating Capacity Chart
nza_capacity<- nza_states %>%
  filter(abbr==state_abbreviation) %>%
  filter(filter_level_2=="Generating capacity",
         filter_level_3 != "Capital invested") %>%
  filter(!grepl("Constrained",variable_name)) %>%
  mutate(variable_name=gsub("Installed renewables - ","",variable_name),
         variable_name=gsub(" - Base land use assumptions","",variable_name),
         variable_name=gsub("Installed thermal - ","",variable_name)) %>%
  ungroup() %>%
  select(abbr,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
  pivot_longer(cols=c(`2025`,`2030`,`2035`,`2040`,`2045`,`2050`),names_to="year",values_to="Value") %>%
  mutate(variable=ifelse(grepl("Biomass",variable_name),"Biomass",variable_name)) %>%
  left_join(rengen_share  %>%
              filter(region_of_interest==1) %>%
              select(tech,share),by=c("variable"="tech")) %>%
  mutate(share=ifelse(is.na(share),region_eleccons$share,share)) %>%
  mutate(Value_region=Value*share) 

plot_nza_ren_cap<-ggplot(data=nza_capacity,aes(x=year,y=Value_region,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("New electricity generation capacity in", region_name, "in a Net Zero Scenario"),
       subtitle = "Downscaled investment figures based on existing share of statewide electricity capacity.",
       x="Year", y="MW",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()



#Final Energy USe Chart
nza_final_energyuse <- nza_states %>%
  filter(abbr==state_abbreviation) %>%
  filter(filter_level_2=="Overview",
         filter_level_3 == "Final energy use") %>%
  filter(!grepl("Constrained",variable_name)) %>%
  mutate(variable_name=gsub("Final energy use - ","",variable_name)) %>%
  ungroup() %>%
  select(abbr,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
  pivot_longer(cols=c(`2025`,`2030`,`2035`,`2040`,`2045`,`2050`),names_to="year",values_to="Value") %>%
  mutate(share=region_eleccons$share) %>%
  mutate(Value_region=Value*share) 

plot_nza_finalenergyuse<-ggplot(data=nza_final_energyuse,aes(x=year,y=Value_region,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Final Energy Use in", region_name, "in Different Scenarios"),
       subtitle = "Downscaled investment figures based on existing share of statewide electricity consumption",
       x="Year", y="PJ",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="bottom")
%>%
  ungroup() %>%
  arrange(desc(gdp_22))

county_cbsa<-county_cbsa %>%
  mutate(state.fips=str_pad(FIPS.State.Code,2,pad="0"),
         county.fips=str_pad(FIPS.County.Code,3,pad="0")) %>%
  mutate(fips=as.numeric(paste0(state.fips,county.fips)))

county_gdp<-county_gdp %>%
  mutate(fips=as.numeric(GeoFips))


#NAICS Codes
file_url <- 'https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx'
temp_file <- tempfile(fileext = ".xlsx")
GET(url = file_url, write_disk(temp_file, overwrite = TRUE))
naics2017 <- read_excel(temp_file, sheet = 1)  # 'sheet = 1' to read the first sheet

naics2022<-read_excel('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/2022_to_2017_NAICS.xlsx',1,skip=2)


