### CIM Facilities Coords Code
# Author: Rhea Cong, 08-14-2024

# This code prompts user for locations to missing CIM facilities data if the investment is over 90M and post-IRA
# Join with EA name and CGT technology/feasibility

# Load packages
library(dplyr)
library(ggmap)

#Google API
register_google(key ="AIzaSyBQFZhv1jZWHejy4BCdI5kb3JN8zfO62Wc")

# Load data
facilities_update <- read.csv("manufacturing_energy_and_industry_facility_metadata.csv",skip=4)

#Tech Mapping between RMI, NAICS, and CMI Terminology
tech_mapping <- data.frame(
  Segment = c("Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Energy and Industry", "Energy and Industry", "Energy and Industry", "Energy and Industry", "Energy and Industry", "Energy and Industry"),
  Technology = c("Batteries", "Solar", "Critical Minerals", "Fueling Equipment", "Zero Emission Vehicles", "Electrolyzers", "Storage", "Wind", "Hydrogen", "SAF", "Storage", "Nuclear", "Solar", "Wind"),
  tech = c("Batteries & Components", "Solar Energy Components", "Low-Carbon Minerals", "Low-Carbon Industrial Equipment", "Electric Vehicles", "Low-Carbon Industrial Equipment", "Batteries & Components", "Wind Energy Components", "Green Hydrogen", "Biofuels", "Energy Utility Systems", "Nuclear Electric Power", "Solar Electric Power", "Wind Electric Power")
)

tech_mapping <- left_join(tech_mapping,eti_long %>% select(Sector,Subsector,Technology,`6-Digit Code`,`6-Digit Description`),by=c("tech"="Technology"))

tech_mapping<-left_join(tech_mapping,naics2022 %>% select("2022 NAICS Code",
                                                          "2022 NAICS Title",
                                                          "2017 NAICS Code"),
                        by=c("6-Digit Code"="2017 NAICS Code"))

#Load Latest Clean Growth Tool Data
cgt<-readRDS('./Data/acre_tool_final_data_042624')

msa_data<-cgt$msa_data
naics_msa_data<-cgt$naics_msa_data
naics_data<-cgt$naics_data
states_msa<-cgt$states_msa
naics6d_data<-cgt$naics6d_data
transition<-cgt$transition_sector_data

#Combine and Clean Data
feasibility<-naics_data %>%
  select(transition_sector_category_id,naics,naics_desc) %>%
  right_join(transition,by=c("transition_sector_category_id"="transition_sector_category_id")) %>%
  select(transition_sector_category,naics,naics_desc) %>%
  inner_join(naics_msa_data,by=c("naics"="naics")) %>%
  filter(aggregation_level=="2") %>%
  left_join(states_msa,by=c("msa"="cbsa")) %>%
  left_join(msa_data %>%
              select(region,msa,msa_name),by=c("msa"="msa")) %>%
  select(statefp,state_name,state_avb,region,msa,msa_name,transition_sector_category, naics,naics_desc,density,pci,rca,jobs,jobs_l5,share_good_jobs,percent_change_jobs_l5) %>%
  group_by(naics,naics_desc) %>%
  mutate(feas_industry_percentile=percent_rank(density)) %>%
  distinct()

# Create function to prompt user to fill in missing addresses, under specified conditions
prompt_fill <- function(data) {
  for (i in 1:nrow(data)) {
      if (data[i,32] == FALSE) {
        cat("Missing address for", data[i,1], ", which is a facility owned by", data[i,3],"in", data[i,10], "\n")
        address <- readline(prompt = "Please enter an address for facility: ")
        
        # Replace NA with the new value
        data[i, 10] <- address
        
        # Add geocoded longitude and latitude
        data[i, 30] <- geocode(data[i,10])[2]
        data[i, 31] <- geocode(data[i,10])[1]
    }
  }
  return(data)
}

# Apply prompt for addresses
facilities_update$Announcement_Date <- as.Date(facilities_update$Announcement_Date)
facilities_update$LatLon_Valid <- as.logical(facilities_update$LatLon_Valid)

facilities_update_man <- facilities_update %>% filter(Segment=="Manufacturing")

rows <- which(is.na(facilities_update_man[, 11]) & (facilities_update_man[,34]>90) & (facilities_update_man[,33]>"2022-08-16"))

facilities_update_man[rows,] <- prompt_fill(facilities_update_man[rows,])


#Take Latitude & Longitude of Projects and match to Counties and EA if manually filled in
facilities_sf <- st_as_sf(facilities_update_man[rows,], coords = c("Longitude", "Latitude"), crs = 4326)
options(tigris_class = "sf") # Ensure tigris returns simple features (sf) objects
us_counties <- counties()
us_counties <- st_transform(us_counties, st_crs(facilities_sf))
facilities_with_counties <- st_join(facilities_sf, us_counties)
facilities_update_man$county_2020_geoid[rows] <-facilities_with_counties$GEOID
facilities_update_man$county_2020_geoid<-as.numeric(facilities_update_man$county_2020_geoid)

facilities_ea<-left_join(facilities_update_man,EAs %>% select(fips, County, `EA Name`),by=c("county_2020_geoid"="fips"))

# Tech crosswalk
# Edits for inverters, electrolyzers, fueling equipment, ev chargers -> energy transmission equipment
facilities_ea <- left_join(facilities_ea, tech_mapping %>% filter(Segment =="Manufacturing") %>% select(Technology, tech) %>% distinct(), by="Technology")
facilities_ea$tech[which(grepl("nverter",facilities_ea$Subcategory))] <- "Energy Transmission Equipment"
facilities_ea$tech[which(grepl("Electrolyzers",facilities_ea$Technology))] <- "Energy Transmission Equipment"
facilities_ea$tech[which(grepl("Fueling",facilities_ea$Technology))] <- "Energy Transmission Equipment"

# Join with feasibility
facilities_ea_feas <- left_join(facilities_ea %>% mutate(EA_name=paste0(`EA Name`, " (EA)")), feasibility %>% select(msa_name, naics_desc, density, pci, rca, jobs, share_good_jobs, feas_industry_percentile), 
          by=c("EA_name"="msa_name", "tech"="naics_desc")) %>%
  distinct()

# Unique scenario for critical minerals
crit_mins <- facilities_ea %>% filter(Technology=="Critical Minerals")
crit_mins$tech <- "Energy Transition Metals"

critmins_feas1 <- left_join(crit_mins %>% mutate(EA_name=paste0(`EA Name`, " (EA)")), feasibility %>% select(msa_name, naics_desc, density, pci, rca, jobs, share_good_jobs, feas_industry_percentile), 
                                by=c("EA_name"="msa_name", "tech"="naics_desc")) %>%
  distinct()

crit_mins$tech <- "Low-Carbon Chemicals"

critmins_feas2 <- left_join(crit_mins %>% mutate(EA_name=paste0(`EA Name`, " (EA)")), feasibility %>% select(msa_name, naics_desc, density, pci, rca, jobs, share_good_jobs, feas_industry_percentile), 
                            by=c("EA_name"="msa_name", "tech"="naics_desc")) %>%
  distinct()

# Append dataset with alternate rows for each critical mineral technology feasibiliy
facilities_ea_feas <- rbind(facilities_ea_feas,critmins_feas1, critmins_feas2)

# Write csv
write.csv(facilities_ea_feas, file="./facilities_ea_feas.csv")

# Investigating missing rows
na_facilities <- facilities_ea_feas[which(facilities_ea_feas$EA_name=="NA (EA)"),]

