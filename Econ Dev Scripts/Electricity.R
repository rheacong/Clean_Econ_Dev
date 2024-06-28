#Electricity Sector
#This script is used to generate the charts and maps for the electricity sector in the US

#included in this script are:
 # Operating Generation Capacity by Technology, by State, Economic Area, and County (EIA 860m data)
 # Planned Generation Capacity by Technology, by State, Economic Area, and County (EIA 860m data)
 # Renewable Electricity Generation Share by State, Economic Area, and County (EPA eGrid data)
 # Emissions Rate of Electricity Generation by State, Economic Area, and County (EPA eGrid data)
 # Industrial Electricity Prices by State (SEDS data)
 # Industrial Electricity Expenditure by State, Economic Area, and County (NREL SLOPE data)
 # Electricity Imports and Exports by State  (SEDS Data)

#Set the Working Directory to your Username and update output folder for saved charts etc
setwd("C:/Users/LCarey.RMI/")
output_folder <- paste0("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Slide Decks/States/",state_abbreviation)

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

#Filter for specific counties from US Map package
region_counties<-us_counties %>%
  filter(fips %in% region_id$FIPS) %>%
  left_join(census_divisions,by=c("abbr"="State.Code","full"="State"))



#State Operating Generation Capacity
#EIA Generation Capacity Data - Check it's the latest month available
url <- 'https://www.eia.gov/electricity/data/eia860m/xls/april_generator2024.xlsx'
destination_folder<-'OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/States Data/'
file_path <- paste0(destination_folder, "eia_op_gen.xlsx")
downloaded_content <- GET(url, write_disk(file_path, overwrite = TRUE))

#Operating Generation
op_gen <- read_excel(file_path, sheet = 1,skip=2)

#State-Level
abbr_opgen_12_23 <- op_gen %>%
  filter(`Plant State`== state_abbreviation,
         `Operating Year` > 2011) %>%
  mutate(Technology = ifelse(grepl("Natural Gas", Technology), "Natural Gas", Technology)) %>%
  filter(Technology != "Flywheels" & Technology != "All Other" & Technology != "Other Gases" & Technology != "Wood/Wood Waste Biomass" & Technology != "Pumped Storage" & Technology != "Wood and Wood Derived Fuels") 

#Region-Level
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





#State Chart: Operating Generation Capacity by Technology, annual additions 
plot_elec_ann_1224<-ggplot(data=abbr_opgen_12_23,aes(x=`Operating Year`,y=`Nameplate Capacity (MW)`,fill=Technology)) +
  geom_col(position='stack') +
  labs(title=paste("Operating Generation Capacity Additions by Technology in",state_name), 
       x="Year", y="Nameplate Capacity (MW)") +
  theme_classic()+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual(values = expanded_palette)

ggsave(file.path(output_folder,"/",state_abbreviation, paste0("plot_elec_ann_1224", ".png")), 
       plot = plot_elec_ann_1224,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)


#Region Chart: Operating Generation Capacity by Technology, annual additions
region_rencap<-op_gen_with_county %>%
  as.data.frame(.) %>%
  filter(GEOID %in% region_counties$fips,
         `Operating Year` > 2011) %>%
  mutate(roi=region_name) %>%
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
  group_by(roi, tech,`Operating Year`) %>%
  summarize(`Nameplate Capacity (MW)` = sum(`Nameplate Capacity (MW)`, na.rm = TRUE)) 


#Region Chart: Operating Generation Capacity by Technology, annual additions 
plot_region_elec_ann_1224<-ggplot(data=region_rencap,aes(x=`Operating Year`,y=`Nameplate Capacity (MW)`,fill=tech)) +
  geom_col(position='stack') +
  labs(title=paste("Operating Generation Capacity Additions by Technology in",region_name), 
       x="Year", y="Nameplate Capacity (MW)") +
  theme_classic()+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual(values = expanded_palette)

ggsave(file.path(output_folder,"/",state_abbreviation, paste0("plot_elec_ann_1224", ".png")), 
       plot = plot_elec_ann_1224,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)

#Generation Map

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
state_map_data<-usa %>%
  filter(ID==str_to_lower(state_name))
state_sf<-st_as_sf(state_map_data,coords=c("x","y"),crs=4326)
bbox<-st_bbox(state_sf)
bbox_named <- c(left = bbox["xmin"],
                bottom = bbox["ymin"],
                right = bbox["xmax"],
                top = bbox["ymax"])
names(bbox_named) <- c("left", "bottom", "right", "top")

base_map <- get_map(location = bbox_named, maptype = "terrain", source = "google")

# Plot the bubble map
map_elec_cap<-ggmap(base_map, darken = c(0.5, "white")) +
  # State boundaries
  #geom_sf(data = state_map_data, inherit.aes = FALSE, color = "black", fill = NA) +  # Explicitly turn off inheritance
  # Location bubbles
  geom_point(data = abbr_opgen_12_23, aes(x=Longitude,y=Latitude,size = `Nameplate Capacity (MW)`, color = Technology), 
             alpha = 0.75, inherit.aes = T) +
  # Customize scales
  scale_size_continuous(range = c(1, 10), guide = "none", name = "Variable Name") +
  scale_color_manual(values = expanded_palette, name="Industry",guide = guide_legend(override.aes = list(size = 5))) +
  # Additional settings
  labs(title = paste0("New electricity generation capacity announced in ",state_name, " since 2011"), 
       subtitle = "Bubble size is scaled by nameplate capacity (MW)",
       caption="Source: EIA 860m") +
  coord_sf(xlim = c(bbox_named["left"], bbox_named["right"]),
           ylim = c(bbox_named["bottom"], bbox_named["top"]),
           crs=3857) +
  geom_sf(data = state_map_data, inherit.aes = FALSE, color = "darkgray", fill = NA) + 
  theme_void()+
  theme(legend.position="bottom")

ggsave(file.path(output_folder, paste0("map_elec_cap", ".png")), 
       plot = map_elec_cap,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)

#generate_ppt_with_chart(paste0("New electricity generation capacity announced in ",state_name, " since 2011"), map_elec_cap, 8)

abbr_12_23_elec_tech <- abbr_opgen_12_23 %>%
  group_by(Technology) %>%
  summarize_at(vars(`Nameplate Capacity (MW)`),sum,na.rm=T) %>%
  ungroup() %>%
  mutate(share=`Nameplate Capacity (MW)`/sum(`Nameplate Capacity (MW)`)) %>%
  arrange(desc(share))


#Chart: Generation Capacity Additions since 2012
plot_elec_cap_1224<-ggplot(data=abbr_12_23_elec_tech,aes(y=reorder(Technology,`Nameplate Capacity (MW)`),
                                                         x=`Nameplate Capacity (MW)`,fill=Technology)) +
  geom_col() +
  labs(title=paste0("Total Generation Capacity Additions in ",state_name," since 2012"), 
       y="", x="Nameplate Capacity (MW)",
       caption="Source: EIA 860m") +
  theme_classic()+
  theme(legend.position = "none")+
  scale_fill_manual(values = expanded_palette)

ggsave(file.path(output_folder, paste0("plot_elec_cap_1224", ".png")), 
       plot = plot_elec_cap_1224,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)  

#All States Generation
states_gen <- op_gen %>%
  group_by(`Plant State`,`Operating Year`,Technology) %>%
  summarize_at(vars(`Nameplate Capacity (MW)`),sum,na.rm=T) %>%
  left_join(states_simple,by=c("Plant State"="abbr"))

states_rengen <- states_gen %>%
  filter(`Operating Year` > 2012 & Technology %in% c("Conventional Hydroelectric",
                                                     "Onshore Wind Turbine",
                                                     "Batteries",
                                                     "Solar Photovoltaic",
                                                     "Solar Thermal with Energy Storage",
                                                     "Hydroelectric Pumped Storage",
                                                     "Geothermal",
                                                     "Solar Thermal without Energy Storage",
                                                     "Offshore Wind Turbine")) %>%
  
  group_by(region,full, `Operating Year`) %>%
  summarize(`Nameplate Capacity (MW)` = sum(`Nameplate Capacity (MW)`, na.rm = TRUE)) %>%
  complete(`Operating Year` = 2013:2023, fill = list(`Nameplate Capacity (MW)` = 0)) %>%
  mutate(Year = make_date(`Operating Year`)) %>%
  mutate(cum_cap = cumsum(`Nameplate Capacity (MW)`)) %>%
  group_by(region,full) %>%
  mutate(cap_index_18 = 100*cum_cap/cum_cap[Year=="2022-01-01"]) %>%
  mutate(rengrowth_18_23 = round(cap_index_18-100,1))


plot_elec_2020index<-ggplot(data=states_rengen %>%
                              filter(region == region_abbrv$region),
                            aes(x=Year,
                                y=cap_index_18,
                                group=full,
                                color=full)) +
  geom_line(data = subset(states_rengen%>%filter(region == region_abbrv$region), full != state_name), size = 1) +  # Plot other lines
  geom_line(data = subset(states_rengen%>%filter(region == region_abbrv$region,), full == state_name), size = 2) +
  scale_size_identity() +
  labs(title="Renewable Electricity Growth since 2022",
       subtitle="Cumulative Renewable Capacity Additions since 2012, indexed to Jan 2022",
       x="", y="Index (100=08-2020)",
       color="State")+
  theme_classic()+
  scale_color_manual(values = rmi_palette)

region_abbrv<-states_simple %>%
  filter(abbr == state_abbreviation) 

#Chart: Renewable Electricity Growth since the IRA
plot_elec_2020index<-ggplot(data=states_rengen %>%
                              filter(region == region_abbrv$region),
                            aes(x=Year,
                                y=cap_index_18,
                                group=full,
                                color=full)) +
  geom_line(data = subset(states_rengen%>%filter(region == region_abbrv$region), full != state_name), size = 1) +  # Plot other lines
  geom_line(data = subset(states_rengen%>%filter(region == region_abbrv$region,), full == state_name), size = 2) +
  scale_size_identity() +
  labs(title="Renewable Electricity Growth since 2022",
       subtitle="Cumulative Renewable Capacity Additions since 2012, indexed to Jan 2022",
       x="", y="Index (100=08-2020)",
       color="State")+
  theme_classic()+
  scale_color_manual(values = expanded_palette)

ggsave(file.path(output_folder, paste0("plot_elec_2020index", ".png")), 
       plot = plot_elec_2020index,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)  

#Planned Generation
plan_gen <- read_excel(file_path, sheet = 2,skip=2)

abbr_plangen <- plan_gen %>%
  filter(`Plant State`== state_abbreviation) %>%
  mutate(Technology = ifelse(grepl("Natural Gas", Technology), "Natural Gas", Technology))

planned_gen_plot<-ggplot(data=abbr_plangen,aes(x=reorder(Technology,-`Nameplate Capacity (MW)`),
                                               y=`Nameplate Capacity (MW)`,
                                               fill=Status)) +
  geom_col(position='stack') +
  labs(title=paste0("Planned Generation Capacity by Technology in ",state_name), 
       x="", y="Nameplate Capacity (MW)") +
  scale_y_discrete(expand = c(0,0))+
  theme_classic()+
  theme(legend.position=c(0.8,0.9))+
  scale_fill_manual(values = rmi_palette)


ggsave(file.path(output_folder, paste0("planned_gen_plot", ".png")), 
       plot = planned_gen_plot,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)  


#State Share of National Total Capacity by Technology
abbr_tech_share <- op_gen %>%
  filter(Status=="(OP) Operating" ) %>%
  group_by(`Plant State`,Technology) %>%
  summarize_at(vars(`Nameplate Capacity (MW)`),sum,na.rm=T) %>%
  group_by(Technology) %>%
  mutate(share=round(`Nameplate Capacity (MW)`/sum(`Nameplate Capacity (MW)`)*100,1)) %>%
  filter(`Plant State`==state_abbreviation) %>%
  arrange(desc(share))



#County Plant-Level Generation


#NREL SLOPE Data - Energy Consumption by County and Region

#Read in the NREL SLOPE Data
county_elec_cons <- read.csv("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/energy_consumption_expenditure_business_as_usual_county.csv")

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
  right_join(census_divisions,by=c("full"="State")) %>% #For Census Divisions
  select(Region,Division,abbr,full,County,`EA Name`,CBSA.Title,FIPS,fips,Consumption.MMBtu)

county_elecgdp <- county_eleccons %>%
  left_join(county_gdp,by=c("fips"="fips")) %>%
  left_join(county_pop %>%
              select(CTYNAME,POPESTIMATE2022),by=c("County"="CTYNAME")) %>%
  mutate(across(c(X2022,POPESTIMATE2022),as.numeric,na.rm=T)) %>%
  mutate(gdp_per_cap=X2022/POPESTIMATE2022,
         elec_cap=Consumption.MMBtu/POPESTIMATE2022) 


msa_eleccons <- county_eleccons %>% #group by MSA
  group_by(CBSA.Title) %>%
  summarize_at(vars(Consumption.MMBtu),sum,na.rm=T)
ea_eleccons <- county_eleccons %>% #group by Economic Area
  group_by(`EA Name`) %>%
  summarize_at(vars(Consumption.MMBtu),sum,na.rm=T)


#Retail Service Territories from EIA Energy Atlas
  #https://atlas.eia.gov/datasets/f4cd55044b924fed9bc8b64022966097
shapefile <- st_read("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/Electric_Retail_Service_Territories.shp")

sbrgn_sf_transformed <- st_transform(shapefile, crs = st_crs(counties))
sbrgn_sf_transformed <- st_make_valid(sbrgn_sf_transformed)
sf::sf_use_s2(FALSE)

# Perform the spatial join with countiesto get retail territories by county
matched <- st_join(sbrgn_sf_transformed, counties, join = st_intersects)
sf::sf_use_s2(TRUE)
matched<-as.data.frame(matched)
matched <- matched %>%
  select(ID,NAME.x,STATE,CNTRL_AREA,HOLDING_CO,NET_GEN,CUSTOMERS,STATEFP,COUNTYFP,GEOID) %>%
  mutate(id=as.numeric(ID))

#Join with MSA & EA Areas
utilities <- matched %>%
  mutate(fips=as.numeric(GEOID)) %>%
  left_join(county_cbsa %>% select(CBSA.Title,fips),by=c("fips"="fips")) %>%
  left_join(EAs %>% select(`EA Name`,fips),by=c("fips"="fips"))

#filter for Utilities in region of interest
region_utility <- utilities %>%
  filter(GEOID %in% region_id$FIPS) 


#EPA eGRID Data for plant-level generation
file_url <- 'https://www.epa.gov/system/files/documents/2024-01/egrid2022_data.xlsx'
temp_file <- tempfile(fileext = ".xlsx")
GET(url = file_url, write_disk(temp_file, overwrite = TRUE))
epa_plnt <- read_excel(temp_file, sheet = 4,skip=1)

#Join with retail service areas to get county-level estimates
epa_retail_area <- epa_plnt %>%
  select(PNAME,OPRNAME,OPRCODE,UTLSRVNM,SRNAME,PLGENATN,	PLGENATR,	PLGENAWI,PLGENASO,PLGENATH,	PLGENACY,	PLGENACN,PLCO2AN, PLCO2RTA, FIPSST,FIPSCNTY) %>%
  inner_join(utilities,by=c("OPRCODE"="id",
                            "FIPSST"="STATEFP",
                            "FIPSCNTY"="COUNTYFP"))

region_retail_area <- epa_retail_area %>%
  filter(GEOID %in% region_id$FIPS) %>%
  distinct(STATE,`EA Name`,CBSA.Title,GEOID,OPRNAME,UTLSRVNM,SRNAME)

epa_gen_opr <- epa_retail_area %>%
  group_by(OPRNAME,OPRCODE) %>%
  summarize(across(c(PLGENATN,  #Plant annual total nonrenewables net generation (MWh)
                     PLGENATR,	#Plant annual total renewables net generation (MWh)
                     PLGENAWI, #Plant annual wind net generation (MWh)
                     PLGENASO,  #Plant annual solar net generation (MWh)
                     PLGENATH,	#Plant annual total nonhydro renewables net generation (MWh)
                     PLGENACY,	 #Plant annual total combustion net generation (MWh)
                     PLGENACN, #Plant annual total noncombustion net generation (MWh)
                     PLCO2AN, #Plant annual CO2 emissions (tons)
                     PLCO2RTA), #Plant annual CO2 total output emission rate (lb/MWh)
                   sum,
                   na.rm=T)) %>%
  mutate(total_gen=PLGENATN	+ PLGENATR,
         ren_mix=PLGENATH/total_gen*100,
         noncomb_mix=PLGENACN/total_gen*100,
         em_rate = PLCO2AN/total_gen) %>%
  arrange(desc(em_rate))

epa_gen_srname<-epa_retail_area %>%
  group_by(SRNAME) %>%
  summarize(across(c(PLGENATN,  #Plant annual total nonrenewables net generation (MWh)
                     PLGENATR,	#Plant annual total renewables net generation (MWh)
                     PLGENAWI, #Plant annual wind net generation (MWh)
                     PLGENASO,  #Plant annual solar net generation (MWh)
                     PLGENATH,	#Plant annual total nonhydro renewables net generation (MWh)
                     PLGENACY,	 #Plant annual total combustion net generation (MWh)
                     PLGENACN, #Plant annual total noncombustion net generation (MWh)
                     PLCO2AN, #Plant annual CO2 emissions (tons)
                     PLCO2RTA), #Plant annual CO2 total output emission rate (lb/MWh)
                   sum,
                   na.rm=T)) %>%
  mutate(total_gen=PLGENATN	+ PLGENATR,
         ren_mix=PLGENATH/total_gen*100,
         noncomb_mix=PLGENACN/total_gen*100,
         em_rate = PLCO2AN/total_gen) %>%
  arrange(desc(em_rate))

weighted_sum_func <- function(x, w) {
  sum(x * w, na.rm = TRUE) 
}


#SUmmarize at Economic Area Level
opr_eas <- matched %>%
  mutate(fips=as.numeric(GEOID)) %>%
  left_join(EAs,by=c("fips"="fips")) %>%
  filter(!is.na(`EA Name`)) %>%
  distinct(`EA Name`,id,NAME.x,NET_GEN,CUSTOMERS) %>%
  left_join(epa_gen_opr,by=c("id"="OPRCODE")) %>%
  filter(!is.na(total_gen)) %>%
  left_join(ea_eleccons,by=c("EA Name"="EA Name")) %>%
  group_by(id) %>%
  mutate(owner_share=Consumption.MMBtu/sum(Consumption.MMBtu),
         msa_demand=owner_share*total_gen) 


#Share of renewable generation within EAs
EA_renshare <- opr_eas %>%
  group_by(`EA Name`) %>%
  summarize(across(c(PLGENATN, PLGENATR, PLGENATH, PLGENACY, PLGENACN, PLCO2AN),
                   ~ weighted_sum_func(.x, owner_share))) %>%
  mutate(total_gen=PLGENATN	+ PLGENATR,
         ren_mix=PLGENATH/total_gen*100,
         noncomb_mix=PLGENACN/total_gen*100,
         em_rate=PLCO2AN/total_gen) %>%
  left_join(ea_eleccons,by=c("EA Name"="EA Name")) %>%
  left_join(EAs,by=c("EA Name"="EA Name")) %>%
  distinct(region,`EA Name`,total_gen,ren_mix,noncomb_mix,em_rate) %>%
  mutate(em_rate_bin = ntile(em_rate, 5)) %>%
  arrange(desc(em_rate))

write.csv(EA_renshare,"Downloads/EA_renshare.csv")
multi_region_id <- EAs %>% 
  filter(`EA Name`==region_name)

#Regional EA Emissions 
plot_data<- EA_renshare %>%
  filter(region %in% multi_region_id$region) %>%
  mutate(region_id=as.factor(ifelse(`EA Name` %in% multi_region_id$`EA Name`,1,0))) %>%
  slice_min(em_rate,n=20)

#Column Chart for Emissions Rate of Utilities within EA
plot_EA_renshare<-ggplot(data=,plot_data, aes(x=reorder(`EA Name`,-em_rate),y=em_rate,fill=region_id)) +
  geom_col(position='stack') +
  coord_flip()+
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Emissions rate of local utility generation across the region"),
       subtitle = "Average CO2 emissions rate of electricity generation in lbs/MWh",
       x="", y="lbs/Mwh",
       caption="Source: EPA, eGrid 2022") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="none")

ggsave(file.path(output_folder, paste0("plot_EA_renshare", ".png")), 
       plot = plot_EA_renshare,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)

#Region's Utilities Emissions Rates
plot_utilities <- ggplot(data=opr_eas %>%
  filter(`EA Name` %in% multi_region_id$`EA Name`),aes(x=reorder(`OPRNAME`,-ren_mix),y=ren_mix,fill=NAME.x)) +
    geom_col(position='stack') +
    coord_flip()+
    scale_fill_manual(values = expanded_palette)+
    labs(title=paste("Non-hydro renewables generation share of utilities across the region"),
         subtitle = "Non-hydro renewables share",
         x="", y="%",
         caption="Source: EPA, eGrid 2022") +
    scale_y_continuous(expand = c(0,0))+
    theme_classic()+
    theme(legend.position="right")
  
  ggsave(file.path(output_folder, paste0("plot_utilities_renshare", ".png")), 
         plot = plot_utilities,
         width = 8,   # Width of the plot in inches
         height = 8,   # Height of the plot in inches
         dpi = 300)
  
  
#Regional Emissions Generation Map
county_map_data <- us_counties %>%
  left_join(EAs %>% select(`EA Name`,FIPS,region),by=c("fips"="FIPS"))%>%
  left_join(EA_renshare,by=c("region"="region","EA Name"="EA Name")) %>%
  filter(region %in% multi_region_id$region) %>%
  mutate(region_id=ifelse(fips %in% region_counties$fips,1,0.7)) %>%
  filter(full!="Alaska")


county_labels<-centroid_labels(regions = c("states"))
county_labels <- county_labels %>%
  filter(full %in% county_map_data$full)

ren_gen_map<-ggplot() +
  geom_polygon(data = county_map_data, aes(x = x, y = y, group = group, fill = em_rate, alpha = region_id), color = 'grey',size=0.001) +
  scale_fill_gradient2(low="#2A9D8F",mid="white",high="#E63946", midpoint=mean(EA_renshare$em_rate),na.value = "grey90", name = "Emissions Rate (lbs/Mwh)") +
  scale_alpha_identity() +
  geom_polygon(data=us_states %>% filter(full %in% county_map_data$full),aes(x=x,y=y,group=group),color="black",fill=NA,alpha=0)+
  geom_text(data = county_labels, aes(x = x, y = y, label = full), size = 2, color = "black", fontface = "bold") +
  labs(title = paste("Electricity Generation Emissions in ", str_to_sentence(multi_region_id$region)," States"), 
       subtitle = "",
       fill = "Location Quotient",
       caption = "Source: EPA, eGrid2022") +
  theme_void() 
#theme(legend.position = c(0.9, 0.1),
#     plot.background = element_rect(fill = "white", color = "white"),
#    panel.background = element_rect(fill = "white", color = "white"))


#Industrial Electricity Prices

#Industrial Electricity Expenditure & Consumption out to 2050 from NREL Estimates
region_industrial <- county_elec_cons %>%
  mutate(FIPS=paste0(substr(Geography.ID,2,3),substr(Geography.ID,5,7))) %>%
  filter(Sector=="industrial",
         FIPS %in% region_counties$fips) %>%
  group_by(Year,Sector,Source) %>%
  summarize_at(vars(Consumption.MMBtu,Expenditure.US.Dollars),sum,na.rm=T) %>%
  mutate(exp_cons=Expenditure.US.Dollars/Consumption.MMBtu) 

industrial_exp_plot<-ggplot(data=region_industrial,aes(x=Year,y=exp_cons,group=Source,color=Source)) +
  geom_line() +
  labs(title=paste0("Industrial Energy Expenditure in ",region_name, "out to 2050"), 
       subtitle="Modelled based on 2016 data",
       y="$/MMBtu",
       x="Year",
       caption="Source:SLOPE") +
  theme_classic()+
  scale_color_manual(values = rmi_palette)

ggplot(data=region_industrial,aes(x=Year,y=Expenditure.US.Dollars,fill=Source)) +
  geom_col(position='stack') +
  labs(title=paste0("Industrial Energy Expenditure in ",region_name, "out to 2050"), 
       subtitle="Modelled based on 2016 data",
       y="$/MMBtu",
       x="Year",
       caption="Source:SLOPE") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  scale_fill_manual(values = rmi_palette)


#Electricity Price in Industrial Sector - SEDS Data
seds_all <- read.csv('https://www.eia.gov/state/seds/sep_update/Complete_SEDS_update.csv') #NB: BIG file

msn_descriptions <- data.frame(
  MSN=c("ESICD", #Electricity price in the industrial sector
        "ESICP", #Electricity consumed by (i.e., sold to) the industrial sector
        "ESICV", #Electricity expenditures in the industrial sector
        "ESISB"), #Electricity sales to the industrial sector excluding refinery use
  Description=c("Electricity price in the industrial sector",
                "Electricity consumed by (i.e., sold to) the industrial sector",
                "Electricity expenditures in the industrial sector",
                "Electricity sales to the industrial sector excluding refinery use")
)

region_division <- census_divisions %>%
  filter(State.Code ==state_abbreviation)

seds_elec_pric_ind <- seds_all %>%
  filter(MSN %in% c("ESICD", #Electricity price in the industrial sector
                    "ESICP", #Electricity consumed by (i.e., sold to) the industrial sector
                    "ESICV", #Electricity expenditures in the industrial sector
                    "ESISB", #Electricity sales to the industrial sector excluding refinery use
                    "GDPRV"), #Real GDP
         Year %in% 2002:2022) %>%
  left_join(msn_descriptions,by=c("MSN"="MSN")) %>%
  left_join(census_divisions, by=c("StateCode"="State.Code")) %>%
  left_join(seds_all %>% filter(MSN=="GDPRV"),by=c("Year"="Year","StateCode"="StateCode"), suffix = c("", "_gdp")) %>%
  mutate(data_gdp = round(Data/Data_gdp*100,2)) %>%
  select(Region,Division,State,StateCode,Year,MSN, Description,Data,data_gdp)


industrial_prices_plot<-ggplot() +
  geom_line(data=seds_elec_pric_ind %>%
              filter(Division == region_division$Division,
                     StateCode != state_abbreviation,
                     MSN=="ESICD"),aes(x=Year,y=Data,group=StateCode,color=StateCode), size = 1) +  # Plot other lines
  geom_line(data=seds_elec_pric_ind %>%
              filter(Division == region_division$Division,
                     StateCode == state_abbreviation,
                     MSN=="ESICD"),aes(x=Year,y=Data,group=StateCode,color=StateCode), size = 2) +
  scale_size_identity()+
  labs(title=paste0("Industrial Energy Prices in the ",region_division$Division, " Division since 2002"), 
       subtitle="",
       y="$",
       x="Year",
       caption="Source: EIA") +
  theme_classic()+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual(values = expanded_palette)

industrial_exp_gdp_plot<-ggplot() +
  geom_line(data=seds_elec_pric_ind %>%
              filter(Division == region_division$Division,
                     StateCode != state_abbreviation,
                     MSN=="ESICV"),aes(x=Year,y=data_gdp,group=StateCode,color=StateCode), size = 1) +  # Plot other lines
  geom_line(data=seds_elec_pric_ind %>%
              filter(Division == region_division$Division,
                     StateCode == state_abbreviation,
                     MSN=="ESICV"),aes(x=Year,y=data_gdp,group=StateCode,color=StateCode), size = 2) +
  scale_size_identity()+
  labs(title=paste0("Industrial Electricity Expenditure/GDP in the ",region_division$Division, " Division since 2002"), 
       subtitle="",
       y="Expenditure/GDP",
       x="Year",
       caption="Source: EIA") +
  theme_classic()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  scale_color_manual(values = expanded_palette)

#Electricity Imports & Exports
seds_elec_impexp <- seds_all %>%
  filter(MSN %in% c("ELIMV", #Electricity import expenditure
                    "ELEXV"), #Electricity export expenditure
         Year %in% 2002:2022) %>%
  left_join(census_divisions, by=c("StateCode"="State.Code")) %>%
  left_join(seds_all %>% filter(MSN=="GDPRV"),by=c("Year"="Year","StateCode"="StateCode"), suffix = c("", "_gdp")) %>%
  pivot_wider(names_from=MSN,values_from=Data) %>%
  mutate(net_exp = ELEXV-ELIMV,
         net_exp_gdp = round(net_exp/Data_gdp*100,2)) %>%
  select(Region,Division,State,StateCode,Year,ELEXV,ELIMV,net_exp,net_exp_gdp)


industrial_exp_gdp_plot<-ggplot() +
  geom_line(data=seds_elec_impexp %>%
              filter(Division == region_division$Division,
                     StateCode != state_abbreviation),aes(x=Year,y=net_exp,group=StateCode,color=StateCode), size = 1) +  # Plot other lines
  geom_line(data=seds_elec_impexp %>%
              filter(Division == region_division$Division,
                     StateCode == state_abbreviation),aes(x=Year,y=net_exp,group=StateCode,color=StateCode), size = 2) +
  scale_size_identity()+
  labs(title=paste0("Electricity Net Export Expenditure in the ",region_division$Division, " Division since 2002"), 
       subtitle="",
       y="$ Millions",
       x="Year",
       caption="Source: EIA") +
  theme_classic()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  scale_color_manual(values = expanded_palette)


#Renewable Production by State
msn_descriptions <- data.frame(
  MSN=c("REPRB", #Renewable energy production
        "REACB", #Renewable energy sources consumed by the transportation sector
        "RECCB", #Renewable energy sources consumed by the commercial sector
        "REEIB", #Renewable energy sources consumed by the electric power sector
        "REGBP", #Renewable energy total generating units net summer capacity in all sectors
        "REICB", #Renewable energy sources consumed by the industrial sector
        "RERCB"), #Renewable energy sources consumed by the residential sector
  Description=c("Renewable energy production",
                "Renewable energy sources consumed by the transportation sector",
                "Renewable energy sources consumed by the commercial sector",
                "Renewable energy sources consumed by the electric power sector",
                "Renewable energy total generating units net summer capacity in all sectors",
                "Renewable energy sources consumed by the industrial sector",
                "Renewable energy sources consumed by the residential sector")
)

seds_ren_prod <- seds_all %>%
  filter(MSN %in% msn_descriptions$MSN, #Electricity export expenditure
         Year %in% 2002:2022) %>%
  left_join(census_divisions, by=c("StateCode"="State.Code")) %>%
  left_join(msn_descriptions,by=c("MSN"="MSN")) %>%
  select(Region,Division,State,StateCode,Year,MSN, Description, Data)

seds_ren_prod_plot<-ggplot(data=seds_ren_prod %>%
                             filter(), aes(x=Year,y=Data,fill=variable_name)) +
  geom_col(position='stack') +
  scale_fill_manual(values = expanded_palette)+
  labs(title=paste("Job Creation in", region_name,",",state_abbreviation, "in a Net Zero Scenario"), 
       x="Year", y="Jobs",
       caption="Source: Net Zero America (2021), Princeton University") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()