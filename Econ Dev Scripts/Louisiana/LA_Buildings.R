# BUILDINGS for LA
# Last edited by Rhea Cong, 12/02/24

### INPUTS
# Climate Policy Initiative projections for GGRF needs by county, Aug 2023
# NREL baseline rooftop potential data at county level
# EIA rooftop solar installation data
# EPA eGRID 2022 data

### OUTPUTS
# GGRF investments projection graph by technology in region of interest
# Data for residential and commercial solar capacity by county in state of interest

# Note: solar data not fully cleaned, could produce more interesting graphs


# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "LA"  # Replace with any US state abbreviation
state_name <- "Louisiana"  # Replace with the full name of any US state
region_name <- "New Orleans-Metairie, LA"
output_folder <- "./LA"

#Make a region_id for your state/region of interest
region_id <- us_counties %>%
  mutate(fips=as.numeric(fips)) %>%
  left_join(county_cbsa, by="fips") %>%
  filter(CBSA.Title == region_name) 

# Region id within state of interest
region_id_state <- region_id %>% filter(full==state_name)


#GGRF Needs - Climate Policy Initiative Report & Data
# url<-'https://www.climatepolicyinitiative.org/wp-content/uploads/2023/08/GGRF_Needs_by_county_Aug2023.xlsx'
# temp_file <- tempfile(fileext = ".xlsx")
# GET(url = file_url, write_disk(temp_file, overwrite = TRUE))
# ggrf <- read_excel(temp_file, sheet = 4)

file = "./Data/GGRF_Needs_by_county_Aug2023.xlsx"
ggrf <- read_excel(file, sheet="Data")


#Clean file
ggrf_county <- ggrf %>%
  left_join(county_pop %>% select(STNAME,CTYNAME,STATE,COUNTY,POPESTIMATE2022),by=c("State"="STNAME","County"="CTYNAME")) %>%
  mutate(FIPS=paste0(sprintf("%02d", STATE), sprintf("%03d", COUNTY))) %>%
  left_join(EAs,by=c("FIPS"="FIPS")) %>%
  left_join(county_cbsa,by=c("fips"="fips")) %>%
  filter(`Rooftop PV measure`=="Both",
         !is.na(`EA Name`))

#MSA of interest
ggrf_MSA <- ggrf_county %>%
  filter(CBSA.Title == region_name, State==state_name) %>%
  group_by(`GGRF category`, Technology, LIDC, Period) %>%
  summarize_at(vars(`Investment ($'000)`),
               sum,
               na.rm=T) %>%
  arrange(desc(`Investment ($'000)`))


#County Totals
ggrf_county_total <- ggrf_county %>%
  group_by(FIPS) %>%
  summarize_at(vars(`Investment ($'000)`),
               sum,
               na.rm=T) 

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
ggrf_build_EAs <- ggrf_county %>%
  filter(`GGRF category`=="Buildings") %>%
  group_by(`EA Name`,`GGRF category`) %>%
  summarize_at(vars(`Investment ($'000)`),
               sum,
               na.rm=T) %>%
  arrange(desc(`Investment ($'000)`)) %>%
  pivot_wider(names_from=`GGRF category`,values_from=`Investment ($'000)`) %>%
  rename()%>%
  write.csv("./MN/ggrf_ea_buildings.csv")

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

ggrf_tech_state <- ggrf_tech %>%
  filter(State==state_name)

ggrf_tech_region <- ggrf_tech_state %>%
  filter(County %in% region_id_state$county) %>%
  group_by(Technology, LIDC, Period) %>%
  summarise(total_inv = sum(investment))

ggrf_tech_region_dw <- ggrf_tech_region %>%
  ungroup() %>%
  filter(Period == "2026-30") %>%
  pivot_wider(names_from = LIDC, values_from = total_inv)

write.csv(ggrf_tech_region_dw, file = paste0("./DataWrapper/",state_abbreviation, "_ggrf_dw"))

# Graph of GGRF Investment Needs by technology in LIDC and Non-LIDC communities in the region of interest
tech_region_inv_plot <- ggplot(ggrf_tech_region, aes(x = factor(Period), y = total_inv, fill = Technology)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ LIDC) +
  labs(title = paste0("GGRF Investments Projection in ", region_name),
       x = "Period",
       y = "Investment, $'000",
       fill = "Technology",
       caption="Source: Climate Policy Initiative") +
  scale_fill_manual(values = expanded_palette)+
  theme_classic()

ggsave(paste0(output_folder,"/",state_abbreviation,"_ggrf_region_tech_invest.png"),plot=tech_region_inv_plot,width=8,height=6,units="in",dpi=300)

ggrf_tech_period <- ggrf_tech %>%
  group_by(Technology,LIDC,Period) %>%
  summarize_at(vars(investment),sum,na.rm=T)

rooftop_ggrf <- ggrf_tech_period %>%
  filter(Technology=="Rooftop PV") %>%
  pivot_wider(names_from="LIDC",values_from="investment")


heatpump_ggrf <- ggrf_tech_period %>%
  filter(Technology=="Residential Heating/Cooling") %>%
  pivot_wider(names_from="LIDC",values_from="investment")


#Rooftop Solar
acs_5yr_21<- getCensus(
  name = "acs/acs5",
  vars = c("B19013_001E"),
  region = "county:*",
  vintage = 2021)

acs_5yr_21$fips <- paste0(acs_5yr_21$state,acs_5yr_21$county)
colnames(acs_5yr_21)[3] <- "med_house_inc"

# NREL baseline rooftop potential data at county level (residnetial + commercial)
rooftop_county <- read.csv("./Data/techpot_baseline_county.csv")
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
  left_join(acs_5yr_21,by="fips") %>%
  left_join(states_simple %>% select(abbr,full, region),by=c("State.Name"="full")) %>%
  filter(med_house_inc>0)

region_rooftop <- rooftop_county %>%
  filter(fips %in% region_id_state$fips)

# Data for residential and commercial solar capacity by county in state of interest
write.csv(region_rooftop,paste0(output_folder, '/region_rooftop_county.csv'))

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

# Graphs comparing median house income and potential rooftop solar capacity -- incomplete
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

write.csv(rooftop_lidc,'./DataWrapper/rooftop_lidc.csv')

# Wind and solar
rooftop_county <- read.csv("./Data/techpot_baseline_county.csv")
colnames(rooftop_county)[6]<-"potential_mwh"
wind_solar_county<-rooftop_county %>%
  mutate(statefp=substr(Geography.ID,2,3),
         countyfp=substr(Geography.ID,5,7)) %>%
  mutate(STATE=as.numeric(statefp),
         COUNTY=as.numeric(countyfp)) %>%
  mutate(fips=paste0(statefp,countyfp)) %>%
  group_by(fips,State.Name,County.Name,STATE,COUNTY) %>%
  summarize_at(vars(potential_mwh),sum,na.rm=T) %>%
  left_join(county_pop %>% select(STATE,COUNTY,POPESTIMATE2023),by=c("STATE"="STATE","COUNTY"="COUNTY")) %>%
  mutate(potential_cap=potential_mwh/POPESTIMATE2023)%>%
  left_join(acs_5yr_21,by="fips") %>%
  left_join(states_simple %>% select(abbr,full, region),by=c("State.Name"="full")) %>%
  filter(State.Name == state_name)
write.csv(wind_solar_county, file="./DataWrapper/LA_wind_solar_cap")

wind_county<-rooftop_county %>%
  mutate(statefp=substr(Geography.ID,2,3),
         countyfp=substr(Geography.ID,5,7)) %>%
  mutate(STATE=as.numeric(statefp),
         COUNTY=as.numeric(countyfp)) %>%
  mutate(fips=paste0(statefp,countyfp)) %>%
  group_by(fips,State.Name,County.Name,STATE,COUNTY) %>%
  filter(Technology %in% c("land_based_wind", "distributed_wind")) %>%
  summarize_at(vars(potential_mwh),sum,na.rm=T) %>%
  left_join(county_pop %>% select(STATE,COUNTY,POPESTIMATE2023),by=c("STATE"="STATE","COUNTY"="COUNTY")) %>%
  mutate(potential_cap=potential_mwh/POPESTIMATE2023)%>%
  left_join(acs_5yr_21,by="fips") %>%
  left_join(states_simple %>% select(abbr,full, region),by=c("State.Name"="full")) %>%
  filter(State.Name == state_name)

write.csv(wind_county, file="./DataWrapper/LA_wind_cap")

solar_county<-rooftop_county %>%
  mutate(statefp=substr(Geography.ID,2,3),
         countyfp=substr(Geography.ID,5,7)) %>%
  mutate(STATE=as.numeric(statefp),
         COUNTY=as.numeric(countyfp)) %>%
  mutate(fips=paste0(statefp,countyfp)) %>%
  group_by(fips,State.Name,County.Name,STATE,COUNTY) %>%
  filter(Technology %in% c("utility_pv", "csp","residential_pv","commercial_pv")) %>%
  summarize_at(vars(potential_mwh),sum,na.rm=T) %>%
  left_join(county_pop %>% select(STATE,COUNTY,POPESTIMATE2023),by=c("STATE"="STATE","COUNTY"="COUNTY")) %>%
  mutate(potential_cap=potential_mwh/POPESTIMATE2023)%>%
  left_join(acs_5yr_21,by="fips") %>%
  left_join(states_simple %>% select(abbr,full, region),by=c("State.Name"="full")) %>%
  filter(State.Name == state_name)

write.csv(solar_county, file=paste0("./DataWrapper/",state_abbreviation,"_solar_cap"))


#EIA Installations
url <- 'https://www.eia.gov/electricity/monthly/xls/table_6_01_b.xlsx'
dest_file <- tempfile(fileext = ".xlsx")
download.file(url, destfile = dest_file, mode = "wb")

data <- read_excel(dest_file)

rooftop_state<-read_excel("./Data/small_scale_solar_2024.xlsx",sheet=1,skip=2)
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

epa_state <- read_excel("./Data/eGrid2022_data.xlsx",sheet=5, skip=1)
epa_state <- epa_state %>%
  select(PSTATABB,FIPSST,STNAMEPCAP,STNGENAN,STGENATH)

rooftop_state <- rooftop_state %>%
  select(-c("Year","Month","Data Status")) %>%
  left_join(epa_state,by=c("State"="PSTATABB")) %>%
  left_join(states_simple %>% select(abbr,full),by=c("State"="abbr")) %>%
  #left_join(xchange_pol_index,by=c("State"="abbr")) %>% ###missing xchange_pol_index
  mutate(resrooftop_capshare=res_cap/STNAMEPCAP*100,
         resrooftop_genshare=res_gen/STNGENAN*100,
         rooftop_capshare=total_cap/STNAMEPCAP*100)


rooftop_state_pot<-rooftop_county %>%
  group_by(State.Name,STATE) %>%
  summarize_at(vars(potential_mwh),sum,na.rm=T)

rooftop_state<-rooftop_state %>%
  left_join(rooftop_state_pot,by=c("full"="State.Name"))%>%
  mutate(potential_share=potential_mwh/STNGENAN*100) %>%
  #  left_join(pres_2020_state,by=c("State"="state_po"))%>% ### missing also
  mutate(gen_pote_ratio = total_gen/potential_mwh) %>%
  arrange(desc(gen_pote_ratio))

### missing net_metering variable
# ggplot(data=rooftop_state) +
#   geom_point(aes(y=rooftop_capshare,x=gen_pote_ratio,size=res_cap,color=net_metering))+
#   ylim(0,10)
# 
# 
# model <- lm(rooftop_capshare ~ potential_share+
#               net_metering +
#               #clean_electricity_policy_index+
#               demshare.y,data=rooftop_state)
# summary(model)

# DW all states investment needs
ggrf_total_national <- ggrf_tech %>%
  group_by(State) %>%
  summarise(total_investment = sum(investment))

write.csv(ggrf_total_national, file="./DataWrapper/state_ggrf_needs")
