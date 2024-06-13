#BUILDINGS

# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "MT"  # Replace with any US state abbreviation
state_name <- "Montana"  # Replace with the full name of any US state
region_name <- "Great Falls, MT"

#If necessary, Create Relevant Downscaling file by county and/or Economic Area
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

#GGRF Needs - Climate Policy Initiative Report & Data
url<-'https://www.climatepolicyinitiative.org/wp-content/uploads/2023/08/GGRF_Needs_by_county_Aug2023.xlsx'
temp_file <- tempfile(fileext = ".xlsx")
GET(url = file_url, write_disk(temp_file, overwrite = TRUE))
ggrf <- read_excel(temp_file, sheet = 4)

#Clean file
ggrf_county <- ggrf %>%
  left_join(county_pop %>% select(STNAME,CTYNAME,STATE,COUNTY,POPESTIMATE2022),by=c("State"="STNAME","County"="CTYNAME")) %>%
  mutate(FIPS=paste0(sprintf("%02d", STATE), sprintf("%03d", COUNTY))) %>%
  left_join(EAs,by=c("FIPS"="FIPS")) %>%
  left_join(county_cbsa,by=c("fips"="fips")) %>%
  filter(`Rooftop PV measure`=="Both",
         !is.na(`EA Name`))

#County Totals
ggrf_county_total <- ggrf_county %>%
  group_by(FIPS) %>%
  summarize_at(vars(`Investment ($'000)`),
               sum,
               na.rm=T) 
write.csv(ggrf_county_total,'C:/Users/LCarey.RMI/Downloads/ggrf_county.csv')

#EA Totals
ggrf_EAs <- ggrf_county %>%
  group_by(`EA Name`,`GGRF category`) %>%
  summarize_at(vars(`Investment ($'000)`),
               sum,
               na.rm=T) %>%
  arrange(desc(`Investment ($'000)`)) %>%
  pivot_wider(names_from=`GGRF category`,values_from=`Investment ($'000)`) %>%
  rename()


#Buildings Technologies in EAs
ggrf_build_EAs <- ggrf_EAs %>%
  filter(`GGRF category`=="Buildings") %>%
  write.csv("C:/Users/LCarey.RMI/Downloads/ggrf_ea_buildings.csv")

ggrf_tech <- ggrf %>%
  mutate(Technology = recode(Technology,
                             "Commercial Sales of cooking units - Electric Resistance" = "Commercial Cooking",
                             "Commercial Sales of space heating units - Electric Heat Pump" = "Commercial Heating/Cooling",
                             "Commercial Sales of space heating units - Electric Resistance" = "Commercial Heating/Cooling",
                             "Commercial Sales of water heating units - Electric Heat Pump" = "Commercial Heating/Cooling",
                             "Commercial Sales of water heating units - Electric Resistance" = "Commercial Heating/Cooling",
                             "Residential Sales of cooking - Electric Resistance" = "Residential Cooking",
                             "Residential Sales of space heating - Electric Heat Pump" = "Residential Heating/Cooling",
                             "Residential Sales of space heating - Electric Resistance" = "Residential Heating/Cooling",
                             "Residential Sales of water heating - Electric Heat Pump" = "Residential Heating/Cooling",
                             "Residential Sales of water heating - Electric Resistance" = "Residential Heating/Cooling",
                             "Installed renewables - Rooftop PV"="Rooftop PV",
                             "Installed renewables - Rooftop PV (alt.)" = "Rooftop PV",
                             "Public EV charging plugs capital - DC Fast" = "EV Charging",
                             "Public EV charging plugs capital - L2" = "EV Charging",
                             "Vehicle sales - Heavy-duty - EV" = "EV Sales",
                             "Vehicle sales - Light-duty - EV" = "EV Sales",
                             "Vehicle sales - Medium-duty - EV" = "EV Sales",
                             "Vehicle sales - Heavy-duty - hydrogen FC" = "H2 FC Vehicle Sales",
                             "Vehicle sales - Medium-duty - hydrogen FC" = "H2 FC Vehicle Sales",
                             "Vehicle sales - Light-duty - hydrogen FC" = "H2 FC Vehicle Sales")) %>%
  filter(Technology %in% c("Commercial Cooking",
                           "Commercial Heating/Cooling",
                           "Residential Cooking",
                           "Residential Heating/Cooling",
                           "Rooftop PV",
                           "EV Charging",
                           "EV Sales")) %>%
  rename(investment = "Investment ($'000)")


ggrf_tech_total <- ggrf_tech %>%
  group_by(Technology, LIDC) %>%
  summarize_at(vars(investment),
               sum,na.rm=T) %>%
  pivot_wider(names_from="LIDC",values_from="investment")
write.csv(ggrf_tech_total,'C:/Users/LCarey.RMI/Downloads/ggrf_tech.csv')

ggrf_tech_period <- ggrf_tech %>%
  group_by(Technology,LIDC,Period) %>%
  summarize_at(vars(investment),sum,na.rm=T)

rooftop_ggrf <- ggrf_tech_period %>%
  filter(Technology=="Rooftop PV") %>%
  pivot_wider(names_from="LIDC",values_from="investment")
write.csv(rooftop_ggrf,'C:/Users/LCarey.RMI/Downloads/rooftop_ggrf.csv')

heatpump_ggrf <- ggrf_tech_period %>%
  filter(Technology=="Residential Heating/Cooling") %>%
  pivot_wider(names_from="LIDC",values_from="investment")
write.csv(heatpump_ggrf,'C:/Users/LCarey.RMI/Downloads/heatpump_ggrf.csv')




#Rooftop Solar
rooftop_county <- read.csv("C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/techpot_baseline_county.csv")
colnames(rooftop_county)[6]<-"potential_mwh"
rooftop_county<-rooftop_county %>%
  mutate(statefp=substr(Geography.ID,2,3),
         countyfp=substr(Geography.ID,5,7)) %>%
  mutate(STATE=as.numeric(statefp),
         COUNTY=as.numeric(countyfp)) %>%
  mutate(fips=paste0(statefp,countyfp)) %>%
  filter(Technology %in% c("residential_pv",
                           "commercial_pv")) %>%
  group_by(fips,State.Name,County.Name,STATE,COUNTY) %>%
  summarize_at(vars(potential_mwh),sum,na.rm=T) %>%
  left_join(county_pop %>% select(STATE,COUNTY,POPESTIMATE2023),by=c("STATE"="STATE","COUNTY"="COUNTY")) %>%
  mutate(potential_cap=potential_mwh/POPESTIMATE2023)%>%
  left_join(acs_5yr_21,by=c("fips"="geoid")) %>%
  left_join(states_simple,by=c("State.Name"="full")) %>%
  filter(med_house_inc>0)

write.csv(rooftop_county,'C:/Users/LCarey.RMI/Downloads/rooftop_county.csv')

rooftop_EA <- rooftop_county %>%
  left_join(EAs,by=c("fips"="FIPS")) %>%
  group_by(`EA Name`) %>%
  summarize_at(vars(potential_mwh,POPESTIMATE2023),sum,na.rm=T) %>%
  mutate(potential_cap=potential_mwh/POPESTIMATE2023) %>%
  arrange(desc(potential_cap))

rooftopEA_sub5<-rooftop_EA %>%
  filter(POPESTIMATE2023<5000000)

rooftop_region <- rooftop_county %>%
  group_by(region) %>%
  summarize_at(vars(potential_mwh,POPESTIMATE2023),sum,na.rm=T) %>%
  mutate(potential_cap=potential_mwh/POPESTIMATE2023) %>%
  arrange(desc(potential_cap))


#Charts
bad_rows <- is.na(rooftop_county$med_house_inc) | 
  is.na(rooftop_county$potential_cap) |
  is.infinite(rooftop_county$med_house_inc) | 
  is.infinite(rooftop_county$potential_cap)


# Remove those rows
rooftop_county_clean <- rooftop_county[!bad_rows, ]


ggplot(data=rooftop_county_clean)+
  geom_point(aes(y=potential_cap,x=med_house_inc,size=potential_mwh))


ggplot(data=rooftop_county_clean, aes(x=med_house_inc, y=potential_mwh, fill=region)) +
  stat_summary_bin(fun="sum", geom="bar", bins=5) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, face="italic"))

us_med_house_inc<- getCensus(
  name = "acs/acs5",
  vars = c("B19013_001E"),
  region = "us:*",
  vintage = 2021)

rooftop_lidc <- rooftop_county %>%
  mutate(lidc=ifelse(med_house_inc<0.85*us_med_house_inc$B19013_001E,"LIDC","Not LIDC")) %>%
  group_by(lidc,region) %>%
  summarize_at(vars(potential_mwh),sum,na.rm=T) %>%
  pivot_wider(names_from="region",values_from="potential_mwh")

write.csv(rooftop_lidc,'C:/Users/LCarey.RMI/Downloads/rooftop_lidc.csv')


#EIA Installations
url <- 'https://www.eia.gov/electricity/monthly/xls/table_6_01_b.xlsx'
dest_file <- tempfile(fileext = ".xlsx")
download.file(url, destfile = dest_file, mode = "wb")

data <- read_excel(dest_file)

rooftop_state<-read_excel("C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/small_scale_solar_2024.xlsx",sheet=1,skip=2)
rooftop_state <- rooftop_state %>%
  rename_with(~c("res_cap",
                 "com_cap",
                 "ind_cap",
                 "total_cap",
                 "res_gen", 
                 "com_gen",
                 "ind_gen",
                 "total_gen"), .cols = 5:12) %>%
  mutate(across(c(res_cap:total_gen),as.numeric))

epa_state <- read_excel("C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/eGrid2022_data.xlsx",sheet=5, skip=1)
epa_state <- epa_state %>%
  select(PSTATABB,FIPSST,STNAMEPCAP,STNGENAN,STGENATH)

rooftop_state <- rooftop_state %>%
  select(-c("Year","Month","Data Status")) %>%
  left_join(epa_state,by=c("State"="PSTATABB")) %>%
  left_join(xchange_pol_index,by=c("State"="abbr")) %>%
  mutate(resrooftop_capshare=res_cap/STNAMEPCAP*100,
         resrooftop_genshare=res_gen/STNGENAN*100,
         rooftop_capshare=total_cap/STNAMEPCAP*100)


rooftop_state_pot<-rooftop_county %>%
  group_by(State.Name,STATE) %>%
  summarize_at(vars(potential_mwh),sum,na.rm=T)

rooftop_state<-rooftop_state %>%
  left_join(rooftop_state_pot,by=c("State.y"="State.Name"))%>%
  mutate(potential_share=potential_mwh/STNGENAN*100) %>%
  left_join(pres_2020_state,by=c("State"="state_po"))%>%
  mutate(gen_pote_ratio = total_gen/potential_mwh) %>%
  arrange(desc(gen_pote_ratio))

ggplot(data=rooftop_state) +
  geom_point(aes(y=rooftop_capshare,x=gen_pote_ratio,size=res_cap,color=net_metering))+
  ylim(0,10)


model <- lm(rooftop_capshare ~ potential_share+
              net_metering +
              #clean_electricity_policy_index+
              demshare.y,data=rooftop_state)
summary(model)
