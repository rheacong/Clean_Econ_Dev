#EVs

# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "MN"  # Replace with any US state abbreviation
state_name <- "Minnesota"  # Replace with the full name of any US state
region_id <- us_counties %>%
  filter(abbr == state_abbreviation) 

#EV Registrations by State
#Check for latest data here: https://afdc.energy.gov/data/categories/maps-data-categories?sort=most+recent
url <- 'https://afdc.energy.gov/files/u/data/data_source/10962/10962-ev-registration-counts-by-state_6-11-24.xlsx?d9ade1eda8'
dest_file <- tempfile(fileext = ".xlsx")
download.file(url, destfile = dest_file, mode = "wb")
data <- read_excel(dest_file,skip=2)
evs_state <- data %>%
  rename(ev_reg = "Registration Count") %>%
  select(State,ev_reg)


#EV Registrations per Capita
evs_state <- evs_state %>%
  left_join(socioecon %>% filter(quarter=="2023-Q4") %>% select(StateName,population), by=c("State"="StateName")) %>%
  mutate(ev_cap = ev_reg/population) %>%
  arrange(desc(ev_cap)) 


#County-Level EV Registration in select states
#Check for latest data here: https://www.atlasevhub.com/materials/state-ev-registration-data/#data
#States include CA,CO,CT,ME,MN,MT,NJ,NM,NY,NC,OR,TN,TX,VA,VT,WA
evs_county <- read.csv(paste0("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/",state_abbreviation,"_EV_Registrations.csv"))

#If the EV registration is by zip code
zip_county_ev <- zip_code_db %>% 
  filter(state == state_abbreviation) %>%
  select(zipcode, county) %>%
  left_join(evs_county, by=c("zipcode"="ZIP.Code")) %>%
  left_join(region_id %>% select(fips, county), by="county")
  

state_evs_county<-zip_county_ev %>% #or just evs_county if originally by county
  group_by(State,County) %>%
  summarize(total=sum(Vehicle.Count,na.rm=T)) %>%
  left_join(county_pop %>% filter(STNAME==state_name) %>% select(CTYNAME,POPESTIMATE2022),by=c("County"="CTYNAME")) %>%
  mutate(ev_cap=total/POPESTIMATE2022) 


#EV Charging Stations, by State
url <-'https://afdc.energy.gov/files/docs/historical-station-counts.xlsx?year=2023'
dest_file <- tempfile(fileext = ".xlsx")
download.file(url, destfile = dest_file, mode = "wb")
data <- read_excel(dest_file,skip=2, sheet=2)
ev_stations_state <- data %>%
  rename("State"="...1",
         "total_chargers"="...10") %>%
  select(State,total_chargers) %>%
  filter(!is.na(State)) %>%
  left_join(socioecon %>% filter(quarter=="2023-Q4") %>% select(StateName,population), by=c("State"="StateName")) %>%
  mutate(ev_stations_cap = total_chargers/population)

#EV Charging Stations, by individual location
#Get latest data here: https://afdc.energy.gov/stations#/analyze?fuel=ELEC&ev_levels=all&access=public&access=private&status=E&status=P
ev_locs <- read.csv("OneDrive - RMI/Documents/Data/Raw Data/alt_fuel_stations.csv")

url<- 'https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt'
# Reading the text file as a CSV
data <- read_delim(url, delim = ",", col_names = TRUE)


evlocs <- ev_locs %>%
  filter(Fuel.Type.Code=="ELEC") %>%
  select(State,ZIP,Groups.With.Access.Code,Facility.Type) %>%
  left_join(data %>% select(ZCTA5,GEOID), by=c("ZIP"="ZCTA5")) %>%
  left_join(EAs,by=c("GEOID"="FIPS")) 

ea_evlocs <- evlocs %>%
  group_by(`EA Name`) %>%
  summarize(n=n()) %>%
  left_join(ea_pop,by="EA Name") %>%
  mutate(evlocs_per_cap= n/POPESTIMATE2022) %>%
  arrange(desc(evlocs_per_cap))

write.csv(ea_evlocs,paste0(output_folder,"/ea_evlocs.csv"))

region_evlocs<-evlocs %>%
  filter(`EA Name` %in% multi_region_id$`EA Name`) %>%
  group_by(Groups.With.Access.Code,Facility.Type) %>%
  summarize(n=n())

#Joining with County-Level EV Registrations
evlocs_county <- ev_locs %>%
  filter(Fuel.Type.Code=="ELEC") %>%
  select(State,ZIP,Groups.With.Access.Code,Facility.Type) %>%
  left_join(data %>% select(ZCTA5,GEOID), by=c("ZIP"="ZCTA5")) %>%
  group_by(State,GEOID) %>%
  summarize(total_charg=n()) 

state_evlocs_county<-evlocs_county %>%
  filter(State==state_abbreviation) %>%
  left_join(county_labels,by=c("State"="abbr","GEOID"="fips")) %>%
  left_join(state_evs_county,by=c("State"="State","county"="County")) %>%
  

