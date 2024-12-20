#Net Zero America Downscaling
# Last edited by Rhea Cong, 11/17/2024

### INPUTS
# Census County Business Patterns
# NREL electricity consumption and generation capacity data
# Princeton Net Zero America data

### OUTPUTS
# graphs for Net Zero Scenario projections: generation capacity, job creation, final energy use
# state level and down-scaled to region of interest (code for MSA and EA)

output_folder <- "./LA"

# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "LA"  # Replace with any US state abbreviation
state_name <- "Louisiana"  # Replace with the full name of any US state
region_name <- "New Orleans-Metairie, LA"

#Make a region_id for your state/region of interest
region_interest <- states_simple %>%
  filter(abbr %in% c("LA", "OK", "TX", "AR", "MS"))

#Make MSA file
MSAs <- county_cbsa %>%
  filter(Metropolitan.Micropolitan.Statistical.Area=="Metropolitan Statistical Area") %>%
  select(CBSA.Code, CBSA.Title, County.County.Equivalent, State.Name, FIPS.State.Code, fips)

#Set the Working Directory to your Username
#setwd("C:/Users/LCarey.RMI/")


#Use County Business Patterns Census Data for Employment figures at county level
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

#Join with State Abbreviations for state names etc
cbp_21$state<-as.numeric(cbp_21$state)
cbp_21<-left_join(states_simple,cbp_21,by=c("fips"="state"))

#join with NAICS Codes to get industry descriptions
# edited eti-> eti_long
cbp_21$NAICS2017<-as.numeric(cbp_21$NAICS2017)
cbp_21 <-left_join(cbp_21,eti_long,by=c("NAICS2017"="6-Digit Code"))

#Six Digit Level
cbp_21 <- cbp_21 %>%
  filter(INDLEVEL=="6")

#Two Digit Level
cbp21_2d <- cbp_2021 %>%
  mutate(state=as.numeric(state)) %>%
  filter(INDLEVEL=="2")  %>%
  mutate(FIPS=paste0(STATE, COUNTY)) %>%
  left_join(EAs,by=c("FIPS"="FIPS")) %>%
  left_join(naics2017 %>% select(`2017 NAICS US   Code`,`2017 NAICS US Title`),by=c("NAICS2017"="2017 NAICS US   Code")) %>%
  rename(naics_desc=`2017 NAICS US Title`)

#Filter just for region of interest
# region_cbp_2d <- cbp21_2d %>%
#   filter(STATE==states_simple$fips[states_simple$abbr==state_abbreviation]) %>%
#   mutate(region_id=ifelse(fips %in% region_id$fips,1,0)) %>%
#   mutate(code=ifelse(NAICS2017 %in% c("00","11","21","22","23","31-33","42","48-49","54"),NAICS2017,"Other")) %>%
#   group_by(region_id,code) %>%
#   summarize_at(vars(EMP),sum,na.rm=T) %>%
#   group_by(code) %>%
#   mutate(share=EMP/sum(EMP, na.rm=T))
## ERROR: the above code assigns the same share to each code

region_cbp_2d <- cbp21_2d %>%
  filter(STATE==states_simple$fips[states_simple$abbr==state_abbreviation]) %>%
  mutate(region_id=ifelse(fips %in% region_id$fips,1,0)) %>%
  mutate(code=ifelse(NAICS2017 %in% c("00","11","21","22","23","31-33","42","48-49","54"),NAICS2017,"Other")) %>%
  group_by(region_id,code) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  mutate(share=EMP/sum(EMP[code=="00"]))

MSA_cbp_2d <- cbp21_2d %>%
  filter(fips %in% region_id$fips) %>%
  # mutate(region_id=ifelse(fips %in% EA_id$fips,1,0)) %>%
  mutate(code=ifelse(NAICS2017 %in% c("00","11","21","22","23","31-33","42","48-49","54"),NAICS2017,"Other")) %>%
  group_by(code) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  mutate(share=EMP/EMP[code=="00"])

#Total Employment in Region/State
region_totalemp<-region_cbp_2d %>%
  ungroup() %>%
  filter(code=="00") %>%
  mutate(stateemp=sum(EMP),share=(EMP/stateemp))


#NREL SLOPE Data - Energy Consumption by County and Region

#Read in the NREL SLOPE Data
county_elec_cons <- read.csv("./Data/energy_consumption_expenditure_business_as_usual_county.csv")

county_eleccons <- county_elec_cons %>% #filter for 2022 and electricity consumption
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
  left_join(EAs,by=c("FIPS"="FIPS")) %>% #for Economic Areas
  left_join(county_cbsa,by=c("fips"="fips")) %>% #For Metropolitan Statistical Areas 
  left_join(states_simple, by=c("STATE"="fips")) %>%
  select(region,abbr,full,County,`EA Name`,CBSA.Title,FIPS,fips,Consumption.MMBtu)

msa_eleccons <- county_eleccons %>% #group by MSA
  group_by(CBSA.Title) %>%
  summarize_at(vars(Consumption.MMBtu),sum,na.rm=T)
ea_eleccons <- county_eleccons %>% #group by Economic Area
  group_by(`EA Name`) %>%
  summarize_at(vars(Consumption.MMBtu),sum,na.rm=T)


#County-level Electricity Consumption for Region of Interest
region_eleccons<-county_eleccons %>%
  filter(abbr== state_abbreviation) %>%
  mutate(region_id=ifelse(FIPS %in% region_id$fips,1,0)) %>%
  group_by(CBSA.Title,region_id) %>%
  summarize_at(vars(Consumption.MMBtu),sum,na.rm=T) %>%
  ungroup() %>%
  mutate(share=Consumption.MMBtu/sum(Consumption.MMBtu,na.rm=T)) %>%
  filter(region_id==1)

state_county_eleccons <- county_eleccons %>%
  filter(abbr== state_abbreviation) %>%
  mutate(region_id=ifelse(FIPS %in% region_id$fips,1,0)) %>%
  select(County, CBSA.Title, FIPS, Consumption.MMBtu)

write.csv(state_county_eleccons, file="./DataWrapper/LA_eleccons")

#EIA Data - Electricity Capacity by County and Region
# url <- 'https://www.eia.gov/electricity/data/eia860m/xls/april_generator2024.xlsx' #Check for Updated Data
# destination_folder<-"Downloads/"
# file_path <- paste0(destination_folder, "eia_op_gen.xlsx")
# downloaded_content <- GET(url, write_disk(file_path, overwrite = TRUE))
op_gen <- read_excel("./Data/eia_op_gen.xlsx", sheet = 1,skip=2)

#Clean Data and add Location Information
op_gen_clean <- op_gen %>% 
  filter(!is.na(Latitude) & !is.na(Longitude))
op_gen_sf <- st_as_sf(op_gen_clean, coords = c("Longitude", "Latitude"), crs = 4326)
op_gen_sf <- st_transform(op_gen_sf, crs = st_crs(counties))

op_gen_with_county <- st_join(op_gen_sf, counties)

#Generating Capacity by Economic Areas
EA_gen <- op_gen_with_county %>%
  mutate(fips=as.numeric(GEOID)) %>%
  inner_join(EAs,by=c("fips"="fips")) %>%
  filter(Status=="(OP) Operating") %>% #Only Operating Plants
  left_join(states_simple, by=c("Plant State"="abbr")) %>%
  group_by(region,full,`EA Name`,`Operating Year`,Technology) %>%
  summarize_at(vars(`Nameplate Capacity (MW)`),sum,na.rm=T) 

#Generating Capacity by MSA
MSA_gen <- op_gen_with_county %>%
  mutate(fips=as.numeric(GEOID)) %>%
  inner_join(county_cbsa,by=c("fips"="fips")) %>%
  filter(Status=="(OP) Operating") %>% #Only Operating Plants
  left_join(states_simple, by=c("Plant State"="abbr")) %>%
  group_by(region,full,CBSA.Title,`Operating Year`,Technology) %>%
  summarize_at(vars(`Nameplate Capacity (MW)`),sum,na.rm=T) 

#Generating Capacity within Region of Interest
region_rengen <- op_gen_with_county %>%
  mutate(fips=as.numeric(GEOID)) %>%
  filter(Status=="(OP) Operating", STATEFP==states_simple$fips[states_simple$full==state_name]) %>%
  mutate(region_id=ifelse(fips %in% region_id$fips,1,0)) %>%
  as.data.frame(.) %>%
  mutate(tech = case_when( #Group similar technologies
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
    Technology=="Wood/Wood Waste Biomass"~"Biomass",
    Technology=="Nuclear"~"Nuclear"
  )) %>%
  group_by(region_id, tech) %>%
  summarize(`Nameplate Capacity (MW)` = sum(`Nameplate Capacity (MW)`, na.rm = TRUE)) 
#complete(`Operating Year` = 2013:2024, fill = list(`Nameplate Capacity (MW)` = 0)) %>%
#mutate(Year = make_date(`Operating Year`)) %>%

#Share of Generating Capacity in Region of Interest
rengen_share <- region_rengen %>%
  group_by(tech) %>%
  mutate(share=round(`Nameplate Capacity (MW)`/sum(`Nameplate Capacity (MW)`),3))

write.csv(rengen_share %>% filter(region_id=="1"), file=paste0("./DataWrapper/",state_abbreviation,"_rengen_share"))

# Total generation capacity share
gen_cap_share <- rengen_share %>%
  group_by(region_id) %>%
  summarise(gen_capacity = sum(`Nameplate Capacity (MW)`))

gen_cap_share$Name <- c("State", "New Orleans-Metairie, LA")

#Net Zero Scenario - Net Zero America
file_url <- 'https://netzeroamerica.princeton.edu/data/nzap-data.csv'
temp_file <- tempfile(fileext = ".csv")
GET(url = file_url, write_disk(temp_file, overwrite = TRUE))
nza <- read.csv(temp_file)  # 'sheet = 1' to read the first sheet

nza<-nza %>%
  mutate(geo=str_to_title(geo)) 
nzap<-left_join(nza,census_divisions,by=c("geo"="State"))

#States
nza_states<-nzap %>%
  filter(scenario %in% c("REF","E+","E+RE+")) %>%
  drop_na(value) %>%
  filter(geo != "national") %>%
  group_by(year,geo,State.Code,scenario,filter_level_1,filter_level_2,filter_level_3,variable_name,unit) %>%
  summarize_at(vars(value),sum) %>%
  spread(year,value) %>%
  filter(!is.na(State.Code)) %>%
  mutate(scenario = factor(scenario, levels = c("REF", "E+", "E+RE+")))

#Jobs by Economic Sector
nza_jobs_econ <- nza_states %>%
  ungroup()%>%
  filter(State.Code==state_abbreviation) %>%
  filter(filter_level_2=="Jobs",
         filter_level_3=="By economic sector") %>%
  mutate(variable_name=gsub("By economic sector - ","",variable_name)) %>%
  select(State.Code,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
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
  mutate(Value_downscale=Value*share*region_totalemp$share[region_totalemp$region_id=="1"]) 

# Plot job creation in region
plot_nza_jobs_econ<-ggplot(data=nza_jobs_region, aes(x=year,y=Value_downscale,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Job Creation in", region_name,"in a Net Zero Scenario"), 
       x="Year", y="Jobs",
       fill="Economic Sector",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="bottom")

ggsave(paste0(output_folder,"/",state_abbreviation,"_nza_jobs_econ.png"),plot=plot_nza_jobs_econ,width=8,height=6,units="in",dpi=300)


# Plot job creation at state level
plot_nza_jobs_econ_state<-ggplot(data=nza_jobs_region, aes(x=year,y=Value,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Job Creation in", state_name,"in a Net Zero Scenario"), 
       x="Year", y="Jobs",
       fill="Economic Sector",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="bottom")

ggsave(paste0(output_folder,"/",state_abbreviation,"_nza_jobs_econ_state.png"),plot=plot_nza_jobs_econ_state,width=8,height=6,units="in",dpi=300)


#Jobs by Resource Sector
nza_jobs_resource <- nza_states %>%
  ungroup()%>%
  filter(State.Code==state_abbreviation) %>%
  filter(filter_level_2=="Jobs",
         filter_level_3=="By resource sector") %>%
  mutate(variable_name=gsub("By resource sector - ","",variable_name)) %>%
  select(State.Code,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
  pivot_longer(cols=c(`2025`,`2030`,`2035`,`2040`,`2045`,`2050`),names_to="year",values_to="Value")

nza_jobs_resource_region <- nza_jobs_resource %>%
  mutate(Value_downscale=Value*region_totalemp$share[region_totalemp$region_id=="1"]) 

# Plot job creation in region, which is same proportions as state
plot_nza_jobs_resource<-ggplot(data=nza_jobs_resource_region, aes(x=year,y=Value_downscale,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Job Creation in", region_name,"in a Net Zero Scenario"), 
       x="Year", y="Jobs",
       fill="Resource Sector",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="bottom")

ggsave(paste0(output_folder,"/",state_abbreviation,"_nza_jobs_resource.png"),plot=plot_nza_jobs_resource,width=8,height=6,units="in",dpi=300)

# Plot job creation at state level
plot_nza_jobs_resource_state<-ggplot(data=nza_jobs_resource, aes(x=year,y=Value,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Job Creation in", state_name,"in a Net Zero Scenario"), 
       x="Year", y="Jobs",
       fill="Resource Sector",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="bottom")

# Without coal, CO2, nuclear
nza_jobs_resource_filter <- nza_jobs_resource %>%
  filter(!variable_name %in% c("Coal", "CO2", "Nuclear"))

plot_nza_jobs_resource_state_filter<-ggplot(data=nza_jobs_resource_filter, aes(x=year,y=Value,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Projected", state_name, "Energy Generation Job Creation in Different Energy Scenarios"), 
       x="Year", y="Jobs",
       fill="Resource Sector",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="bottom")

#Capital Invested in Electricity Generation Chart
nza_cap_inv <- nza_states %>%
  filter(State.Code==state_abbreviation) %>%
  filter(filter_level_3=="Capital invested" ) %>%
  filter(!grepl("Constrained",variable_name)) %>%
  mutate(variable_name=gsub("Capital invested - ","",variable_name),
         variable_name=gsub(" - Base","",variable_name)) %>%
  ungroup() %>%
  select(State.Code,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
  pivot_longer(cols=c(`2025`,`2030`,`2035`,`2040`,`2045`,`2050`),names_to="year",values_to="Value") %>%
  mutate(variable=ifelse(grepl("Biomass",variable_name),"Biomass",variable_name)) %>%
  mutate(variable1=ifelse(grepl("Solar",variable),"Solar",variable))%>%
  left_join(rengen_share  %>%
              filter(region_id==1) %>%
              select(tech,share),by=c("variable1"="tech")) %>%
  mutate(Value_region=Value*share) 

# Plot regional investment
plot_nza_ren_capinv<-ggplot(data=nza_cap_inv,aes(x=year,y=Value_region,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Capital invested in ", region_name, "in electricity generation in a Net Zero Scenario"),
       subtitle = "Downscaled investment figures based on existing share of statewide electricity capacity additions.",
       x="Year", y="Billion $ 2018",
       fill="Investment Industry",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()

ggsave(paste0(output_folder,"/",state_abbreviation,"_nza_ren_capinv.png"),plot=plot_nza_ren_capinv,width=10,height=6,units="in",dpi=300)

# Plot state investment
plot_nza_ren_capinv_state<-ggplot(data=nza_cap_inv,aes(x=year,y=Value,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Capital invested in ", state_name, "in electricity generation in a Net Zero Scenario"),
       subtitle = "Downscaled investment figures based on existing share of statewide electricity capacity additions.",
       x="Year", y="Billion $ 2018",
       fill="Investment Industry",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()

ggsave(paste0(output_folder,"/",state_abbreviation,"_nza_ren_capinv_state.png"),plot=plot_nza_ren_capinv_state,width=10,height=6,units="in",dpi=300)


#Generating Capacity Chart
### NB that downscaling to MSA might not be very representative of opportunities in the state
nza_capacity<- nza_states %>%
  filter(State.Code==state_abbreviation) %>%
  filter(filter_level_2=="Generating capacity",
         filter_level_3 != "Capital invested") %>%
  filter(!grepl("Constrained",variable_name)) %>%
  mutate(variable_name=gsub("Installed renewables - ","",variable_name),
         variable_name=gsub(" - Base land use assumptions","",variable_name),
         variable_name=gsub("Installed thermal - ","",variable_name)) %>%
  ungroup() %>%
  select(State.Code,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
  pivot_longer(cols=c(`2025`,`2030`,`2035`,`2040`,`2045`,`2050`),names_to="year",values_to="Value") %>%
  mutate(variable=ifelse(grepl("Biomass",variable_name),"Biomass",variable_name))

nza_capacity$variable[nza_capacity$variable=="Rooftop PV"] <- "Solar"
nza_capacity$variable[nza_capacity$variable=="Natural gas"] <- "Natural Gas"


nza_capacity <- nza_capacity %>%
  left_join(rengen_share  %>%
              filter(region_id==1) %>%
              select(tech,share),by=c("variable"="tech")) %>%
  mutate(share=ifelse(is.na(share),region_eleccons$share,share)) %>%
  mutate(Value_region=Value*share) 

#Plot regional generation capacity
plot_nza_ren_cap<-ggplot(data=nza_capacity,aes(x=year,y=Value_region,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("New electricity generation capacity in", region_name, "in a Net Zero Scenario"),
       subtitle = "Downscaled investment figures based on existing share of statewide electricity capacity.",
       x="Year", y="MW",
       fill="Electricity Source",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()

ggsave(paste0(output_folder,"/",state_abbreviation,"_nza_ren_cap.png"),plot=plot_nza_ren_cap,width=10,height=6,units="in",dpi=300)

# Plot state generation capacity
plot_nza_ren_cap_state<-ggplot(data=nza_capacity,aes(x=year,y=Value,fill=variable_name)) +
  geom_col(position='stack') +
  facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("New electricity generation capacity in", state_name, "in a Net Zero Scenario"),
       subtitle = "Downscaled investment figures based on existing share of statewide electricity capacity.",
       x="Year", y="MW",
       fill="Electricity Source",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()

ggsave(paste0(output_folder,"/",state_abbreviation,"_nza_ren_cap_state.png"),plot=plot_nza_ren_cap_state,width=10,height=6,units="in",dpi=300)



#Final Energy Use Chart
nza_final_energyuse <- nza_states %>%
  filter(State.Code==state_abbreviation) %>%
  filter(filter_level_2=="Overview",
         filter_level_3 == "Final energy use") %>%
  filter(!grepl("Constrained",variable_name)) %>%
  mutate(variable_name=gsub("Final energy use - ","",variable_name)) %>%
  ungroup() %>%
  select(State.Code,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
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
       fill="Energy Use",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="bottom")

ggsave(paste0(output_folder,"/",state_abbreviation,"_nza_finalenergyuse.png"),plot=plot_nza_finalenergyuse,width=10,height=6,units="in",dpi=300)


#Energy Production
nza_energy_production<- nzap %>%
  #filter(scenario %in% c("REF","E+","E+RE+")) %>%
  drop_na(value) %>%
  filter(geo != "National") %>%
  filter(filter_level_3 %in% c("Natural gas production" ,
                               "Oil production")) 

nza_oil_prod_10 <- nza_energy_production %>%
  filter(year=="2025",
         filter_level_3=="Oil production") %>%
  slice_max(order_by=value,n=10) 
nza_gas_prod_10 <- nza_energy_production %>%
  filter(year=="2025",
         filter_level_3=="Natural gas production") %>%
  slice_max(order_by=value,n=10) 

nza_energy_prod<- nza_energy_production %>%
  filter(geo %in% nza_oil_prod_10$geo|
           geo %in% nza_gas_prod_10$geo) %>%
  group_by(geo,filter_level_3) %>%
  mutate(value_norm=value/value[year=="2025"]*100)

plot_nza_energyprod<-ggplot(data=nza_energy_prod %>%
                              filter(geo==state_name),aes(x=year,y=value,group=filter_level_3,fill=filter_level_3)) +
  geom_col() +
  facet_wrap(~ filter_level_3, scales = "free_y") +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette, name="Fossil Fuel")+
  labs(title=paste("Fossil Fuel Production in", state_name, "in a Net Zero Scenario"),
       subtitle = " ",
       x="Year", y="Natural Gas (tcf) & Oil (mbbl) Production",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="bottom")


ggsave(file.path(output_folder, paste0(state_abbreviation,"_plot_nza_energyprod", ".png")), 
       plot = plot_nza_energyprod,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)
