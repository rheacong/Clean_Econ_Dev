#Clean Investment Monitor

#This script contains:
#1. National Trends
#2. State Clean Energy Investment
#3. Clean Energy Facility announcement before/after IRA within Region
#4. Announced Investments by Segment, Technology within States, Economic Areas and MSAs

# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "NM"  # Replace with any US state abbreviation
state_name <- "New Mexico"  # Replace with the full name of any US state

#Set the Working Directory to your Username
setwd("C:/Users/LCarey.RMI/")

#Create output folder and update
output_folder <- paste0("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Slide Decks/States/",state_abbreviation)


# Clean investment Monitor Data - Check it's the latest quarter available
investment_data_path <- 'OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/clean_investment_monitor_q1_24/quarterly_actual_investment.csv'
facilities_data_path <- 'OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/clean_investment_monitor_q1_24/manufacturing_energy_and_industry_facility_metadata.csv'
socioeconomics_data_path <- 'OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/clean_investment_monitor_q1_24/socioeconomics.csv'

# Read Data
investment <- read.csv(investment_data_path, skip=5)
facilities <- read.csv(facilities_data_path, skip=5)
socioecon <- read.csv(socioeconomics_data_path, skip=5)


#Tech Mapping between RMI, NAICS, and CMI Terminology

tech_mapping <- data.frame(
  Segment = c("Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Energy and Industry", "Energy and Industry", "Energy and Industry", "Energy and Industry", "Energy and Industry", "Energy and Industry"),
  Technology = c("Batteries", "Solar", "Critical Minerals", "Fueling Equipment", "Zero Emission Vehicles", "Electrolyzers", "Storage", "Wind", "Hydrogen", "SAF", "Storage", "Nuclear", "Solar", "Wind"),
  tech = c("Batteries & Components", "Solar Energy Components", "Low-Carbon Minerals", "Low-Carbon Industrial Equipment", "Electric Vehicles", "Low-Carbon Industrial Equipment", "Batteries & Components", "Wind Energy Components", "Green Hydrogen", "Biofuels", "Energy Utility Systems", "Nuclear Electric Power", "Solar Electric Power", "Wind Electric Power")
)
tech_mapping_left_join(tech_mapping,eti_long %>% select(Sector,Subsector,Technology,`6-Digit Code`,`6-Digit Description`),by=c("tech"="Technology"))
tech_mapping<-left_join(tech_mapping,naics2022 %>% select("2022 NAICS Code",
                                                          "2022 NAICS Title",
                                                          "2017 NAICS Code"),
                        by=c("6-Digit Code"="2017 NAICS Code"))



#National Trends

#Total Investment by Year
investment_year<-investment %>% 
  mutate(year=as.numeric(substr(quarter,1, 4))) %>%
  group_by(year) %>%
  summarize_at(vars(Value),sum,na.rm=T)

#Total Investment by Segment,Year
investment_year_segment<-investment %>%
  mutate(year=as.numeric(substr(quarter,1,4))) %>%
  group_by(year,Segment) %>%
  summarize_at(vars(Estimated_Actual_Quarterly_Expenditure),sum,na.rm=T) %>%
  mutate(Segment = recode(Segment, "energyandindustry" = "Energy & Industry",
                          "manufacturing" = "Manufacturing",
                          "retail" = "Retail")) %>%
  pivot_wider(names_from=year,values_from=Estimated_Actual_Quarterly_Expenditure) #wide format for Datwrapper

#Investment by Segment
investment_segment<-investment %>%
  group_by(Segment) %>%
  summarize_at(vars(Estimated_Actual_Quarterly_Expenditure ),sum,na.rm=T) %>%
  mutate(Segment = recode(Segment, "energyandindustry" = "Energy & Industry",
                          "manufacturing" = "Manufacturing",
                          "retail" = "Retail")) %>%
  mutate(share=Estimated_Actual_Quarterly_Expenditure /sum(Estimated_Actual_Quarterly_Expenditure ))


#Cumulative Growth

#Top 10 Technologies in latest quarter
investment_10 <- investment %>%
  filter(!Subcategory %in% c("Power - Natural Gas", "Power - Coal","Natural gas processing")) %>%
  mutate(Segment = recode(Segment, "energyandindustry" = "Energy & Industry",
                          "manufacturing" = "Manufacturing",
                          "retail" = "Retail")) %>%
  #rename(Value=Estimated_Actual_Quarterly_Expenditure) %>%
  filter(quarter=="2024-Q1") %>%
  group_by(Segment,Technology) %>%
  summarize(across(Value,sum,na.rm=T)) %>%
  ungroup() %>%
  mutate(Sector=paste0(Segment,"-",Technology)) %>%
  slice_max(order_by=Value,n=10)

investment_growth <-investment %>%
  filter(!Subcategory %in% c("Power - Natural Gas", "Power - Coal","Natural gas processing")) %>% #filter out Carbon Management categories we don't like
  #rename(Value=Estimated_Actual_Quarterly_Expenditure) %>%
  mutate(Segment = recode(Segment, "energyandindustry" = "Energy & Industry",
                          "manufacturing" = "Manufacturing",
                          "retail" = "Retail")) %>%
  mutate(year_quarter = yq(quarter)) %>%
  group_by(Segment,Technology,quarter,year_quarter) %>%
  summarize_at(vars(Value),sum,na.rm=T) %>%
  group_by(Segment,Technology) %>%
  mutate(Value = replace_na(Value,0)) %>%
  mutate(yoy_growth = (Value / lag(Value, n = 4) - 1) * 100) %>%
  mutate(yoy_growth_moving_avg = rollmean(yoy_growth, 4, align = 'right', na.pad = TRUE)) %>% #4 quarter moving average
  arrange(year_quarter) %>%
  mutate(cum_inv = cumsum(Value)) %>% #cumulative investment
  mutate(inv_index_ira = 100*cum_inv/cum_inv[quarter=="2022-Q2"],na.rm=T) %>% #Index to IRA
  ungroup() %>%
  mutate(Sector=paste0(Segment,"-",Technology)) %>%
  filter(Sector %in% investment_10$Sector) 

investment_growth_wide<-investment_growth %>% #wide format for Datawrapper
  select(Sector,inv_index_ira,quarter) %>%
  pivot_wider(names_from=Sector,values_from=inv_index_ira) 



#State Clean Energy Investment  - Clean Investment Monitor

# Determine the top 5 technologies by value for each state and segment and create "Other" Category
top_technologies <-investment %>%
  rename(Value=Estimated_Actual_Quarterly_Expenditure ) %>%
  mutate(industry=ifelse(Segment=="Manufacturing",paste0(Technology," Manufacturing"),Technology)) %>%
  group_by(State,industry) %>%
  summarize_at(vars(Value),sum,na.rm=T) %>%
  arrange(desc(Value)) %>%
  slice_max(order_by=Value,n=5)%>%
  ungroup() %>%
  select(State, industry) %>%
  distinct() %>%
  mutate(top=1) # Get unique top technologies by state and segment

#Quarterly Investment by State and Industry
states_investment_quarterly <- investment %>%
  filter(!Subcategory %in% c("Power - Natural Gas", "Power - Coal","Natural gas processing")) %>% #filter out Carbon Management categories we don't like
  mutate(industry=ifelse(Segment=="Manufacturing",paste0(Technology," Manufacturing"),Technology)) %>%
  left_join(socioecon,by=c("State"="State","quarter"="quarter")) %>%
  left_join(top_technologies, by = c("State", "industry")) %>%
  rename(Value=Estimated_Actual_Quarterly_Expenditure) %>%
  mutate(industry = ifelse(top=="1",industry, "Other"),
         industry=ifelse(is.na(industry),"Other",Technology)) %>%
  group_by(State,industry,quarter) %>%
  summarize_at(vars(Value,real_gdp),sum,na.rm=T) %>%
  mutate(inv_gdp = Value/real_gdp) %>%
  mutate(year_quarter = yq(quarter))

#Quarterly Clean Energy Investment Chart for the State
plot_quarterly_inv<-ggplot(data=states_investment_quarterly %>%
                             filter(State==state_abbreviation) %>%
                             select(State,year_quarter,Value,industry),aes(x=year_quarter,y=Value,fill=industry)) +
  geom_col(position='stack') +
  labs(title=paste("Clean Energy Investment in", state_name, "since 2018"), 
       x="Quarter", y="Investment Value") +
  theme_classic()+
  scale_fill_manual(values = rmi_palette)+
  scale_y_continuous(expand=c(0,0))

ggsave(paste0(output_folder,"/",state_abbreviation,"_quarterly_inv.png"),plot=plot_quarterly_inv,width=8,height=6,units="in",dpi=300)

#pre and Post IRA
state_inv_ira<-states_investment_quarterly %>%
  mutate(post_IRA = ifelse(year_quarter>"2022-08-15",1,0)) %>%
  group_by(State,post_IRA,industry) %>%
  summarize_at(vars(Value,inv_gdp),sum,na.rm=T)


#Investment Location Quotient - i.e. specialization in investment by State
#National
investment_gdp_2123<- investment %>%
  filter(!Subcategory %in% c("Power - Natural Gas", "Power - Coal","Natural gas processing")) %>% #filter out Carbon Management categories we don't like
  rename(Value=Estimated_Actual_Quarterly_Expenditure) %>%
  mutate(industry=ifelse(Segment=="Manufacturing",paste0(Technology," Manufacturing"),Technology),
         year_quarter = yq(quarter)) %>%
  left_join(socioecon,by=c("State"="State","quarter"="quarter")) %>%
  filter(year_quarter>"2020-12-01") %>%
  group_by(industry) %>%
  summarize_at(vars(Value,real_gdp),sum,na.rm=T) %>%
  mutate(US_inv_gdp = Value/real_gdp,
         State = "US",
         US_inv_share=Value/sum(Value)) %>%
  select(industry,US_inv_gdp,US_inv_share) 

#State
states_investment_gdp_2123 <- investment %>%
  filter(!Subcategory %in% c("Power - Natural Gas", "Power - Coal","Natural gas processing")) %>% #filter out Carbon Management categories we don't like
  rename(Value=Estimated_Actual_Quarterly_Expenditure) %>%
  mutate(industry=ifelse(Segment=="Manufacturing",paste0(Technology," Manufacturing"),Technology),
         year_quarter = yq(quarter)) %>%
  left_join(socioecon,by=c("State"="State","quarter"="quarter")) %>%
  filter(year_quarter>"2020-12-01") %>%
  mutate(post_IRA = ifelse(year_quarter>"2022-08-15",1,0)) %>%
  filter(post_IRA=="1") %>%
  group_by(State,industry,Subcategory) %>%
  summarize_at(vars(Value,real_gdp),sum,na.rm=T) %>%
  mutate(inv_gdp = Value/real_gdp) %>%
  group_by(State) %>%
  mutate(inv_share=Value/sum(Value)) %>%
  left_join(investment_gdp_2123,by=c("industry")) %>%
  mutate(gdp_lq=inv_gdp/US_inv_gdp,
         lq=inv_share/US_inv_share) %>%
  filter(industry != "Other") %>%
  arrange(desc(lq))

#State Specialization Map - Find most specialized for each state
state_spec_inv_2123<-states_investment_gdp_2123 %>%
  group_by(State) %>%
  slice_max(order_by=lq,n=1)%>%
  rename(state=State) %>%
  mutate(lq_bin=cut(lq,breaks=c(0,1,5,10,25,Inf),labels=c("0-1x","1-5x","5-10x","10-25x","25x+")))

# Get US state map data
us_states <-us_map("states")
state_map_data <- left_join(us_states, state_spec_inv_2123, by = c("abbr" = "state"))

state_labels<-centroid_labels(regions = c("states"))
state_labels <- state_labels %>%
  left_join(state_spec_inv_2123,by=c("abbr"="state"))

# Plotting the US state map with investment data
inv_spec_map<-ggplot() +
  geom_polygon(data = state_map_data, aes(x=x,y=y,group=group,fill = lq_bin), color = "white") +
  geom_label_repel(data = state_labels, aes(x = x, y = y, label = Subcategory), size = 2, color = "black", fontface = "bold", label.padding = unit(0.5, "lines"), label.size = 0.25, fill = "white") +
  scale_fill_manual(values = rmi_palette, na.value = "grey90", name = "Investment Share Relative to National Average") +
  labs(title = "State Clean Energy Investment Specializations", 
       subtitle = "Share of clean energy investment relative to the national average for most specialized industry (labelled)",
       fill = "Investment Specialization",
       caption="Source: Clean Investment Monitor") +
  theme_void()+
  theme(legend.position="bottom")

ggsave(paste0(output_folder,"/state_specialization_map.png"), plot = inv_spec_map, width = 8, height = 6, dpi = 300)


#Select State Investment/GDP
plot_invgdp<- ggplot(data=states_investment_gdp_2123 %>%
                       filter(State==state_abbreviation) %>%
                       slice_max(order_by=gdp_lq,n=6),aes(y=reorder(Subcategory,gdp_lq),x=gdp_lq,fill=Subcategory)) +
  geom_bar(stat="identity")+
  labs(title=paste("Clean Energy Investment in", state_name, "since 2021 Relative to GDP"), 
       y="Industry", x="Investment/GDP Relative to National Average") +
  theme_classic()+
  scale_fill_manual(values = rmi_palette)+
  theme(legend.position="none")

ggsave(paste0(output_folder,"/state_investment_gdp.png"), plot = plot_invgdp, width = 8, height = 6, dpi = 300)

#Clean Energy Manufacturing since IRA within Region
state_man_total <- investment %>%
  mutate(post_IRA = ifelse(quarter %in% c("2022-Q3","2022-Q4","2023-Q1","2023-Q2","2023-Q3","2023-Q4","2024-Q1"),1,0)) %>%
  filter(Segment=="Manufacturing",
         post_IRA=="1") %>%
  group_by(State) %>%
  summarize_at(vars(Estimated_Actual_Quarterly_Expenditure),sum,na.rm=T) %>%
  mutate(share=Estimated_Actual_Quarterly_Expenditure/sum(Estimated_Actual_Quarterly_Expenditure)*100) %>%
  ungroup()

region_abbrv<-states_simple %>%
  filter(abbr == state_abbreviation) 

state_man <- facilities %>%
  left_join(states_simple,by=c("State"="abbr") ) %>%
  mutate(date=as.Date(Announcement_Date),
         year=substr(date,1,4),
         post_IRA = ifelse(date>"2022-08-15",1,0)) %>%
  filter(Segment=="Manufacturing",
         region.x %in% region_abbrv$region,
         post_IRA=="1") %>%
  group_by(State,Technology) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T) %>%
  ungroup() 
state_man<-as.data.frame(state_man)
state_man<-state_man[1:3]

#Wide format for stacked column chart in Datawrapper
state_man_wide <- state_man %>%
  select(State,Total_Facility_CAPEX_Estimated,Technology) %>%
  pivot_wider(names_from=Technology,values_from=Total_Facility_CAPEX_Estimated) 

#Stacked Column chart of clean energy manufacturing
plot_manufacturing<-ggplot(data=state_man,aes(x=reorder(State,-Total_Facility_CAPEX_Estimated),y=Total_Facility_CAPEX_Estimated,fill=Technology)) +
  geom_col(position='stack') +
  labs(title=paste("Clean Energy Manufacturing in the ", region_abbrv$region, "since passage of the IRA"), 
       x="State", y="Total Facility CAPEX Estimated ($m)",
       caption="Source: Clean Investment Monitor") +
  theme_classic()+
  scale_fill_manual(values = expanded_palette)+
  scale_y_continuous(expand=c(0,0))

ggsave(paste0(output_folder,"/",state_abbreviation,"_manufacturing.png"),plot=plot_manufacturing,width=8,height=6,units="in",dpi=300)
#ANNOUNCED INVESTMENT
#All Manufacturing Facilities
facilities_man<-facilities %>%
  filter(Segment=="Manufacturing") 

#Announced Manufacturing Investment by State
facilities_state <- facilities %>%
  filter(Segment=="Manufacturing") %>%
  group_by(State) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T) %>%
  mutate(share=Total_Facility_CAPEX_Estimated/sum(Total_Facility_CAPEX_Estimated)*100) %>%
  arrange(desc(share)) %>%
  slice_max(order_by=share,n=12) %>%
  write.csv('Downloads/facilities_man_state.csv')


#Pre and Post-IRA
facilities<-facilities %>%
  mutate(date=as.Date(Announcement_Date),
         post_IRA = ifelse(date>"2022-08-15",1,0))  %>%
  mutate(across(where(is.numeric), as.numeric))

facilities_ira_total<-facilities %>%
  filter(post_IRA=="1") %>%
  filter(Segment=="Manufacturing") %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T) 

facilities_status<-facilities %>%
  group_by(Investment_Status) %>%
  summarize_at(vars(Investment_Estimated),sum,na.rm=T) %>%
  mutate(Investment_Estimated=round(Investment_Estimated/1000,2)) %>%
  mutate(share=round(Investment_Estimated/sum(Investment_Estimated)*100,2)) 

facilities_status_tech<-facilities %>%
  mutate(year=as.numeric(substr(Announcement_Date,1,4))) %>%
  filter(Investment_Status != "C" & year>2021) %>%
  group_by(Investment_Status, Technology) %>%
  summarize_at(vars(Investment_Estimated),sum,na.rm=T) %>%
  mutate(Investment_Estimated=round(Investment_Estimated/1000,2)) %>%
  pivot_wider(names_from=Investment_Status,values_from=Investment_Estimated) %>%
  mutate(total = rowSums(select(., A, U, O), na.rm = TRUE)) %>%
  arrange(desc(total))


facilities_state_sector<-facilities %>%
  mutate(year=as.numeric(substr(Announcement_Date,1,4))) %>%
  filter(year>2020) %>%
  left_join(states_simple,by=c("State"="abbr")) %>%
  filter(Segment=="Manufacturing" & Investment_Status != "C") %>%
  group_by(region,State,full,Technology) %>%
  summarize_at(vars(Investment_Estimated),sum,na.rm=T)%>%
  group_by(region,State,full) %>%
  mutate(share=Investment_Estimated/sum(Investment_Estimated)) %>%
  arrange(desc(Investment_Estimated))

facilities_state_status<-facilities %>%
  left_join(states_simple,by=c("State"="abbr")) %>%
  group_by(region,State,full,Segment) %>%
  summarize_at(vars(Investment_Estimated),sum,na.rm=T) %>%
  pivot_wider(names_from=Investment_Status,values_from=Investment_Estimated) %>%
  mutate(total_inv_b = round((sum(A, U, O, na.rm = TRUE))/1000,4))%>%
  arrange(desc(total_inv_b))


#Breaking out Solar, Wind, Battery Supply chains
supply_chains <- facilities %>%
  filter(Segment=="Manufacturing") %>%
  mutate(date=as.Date(Announcement_Date),
         post_IRA = ifelse(date>"2022-08-15",1,0),
         Subcategory = ifelse(Technology=="Solar" & grepl("Cells",Subcategory),"Cells/Modules",Subcategory),
         Subcategory = ifelse(Technology=="Solar" & grepl("Modules",Subcategory),"Cells/Modules",Subcategory),
         Subcategory = ifelse(grepl("Lithium",Subcategory),"Lithium",Subcategory),
         Subcategory = ifelse(Technology=="Batteries" & grepl("Modules",Subcategory),"EAM/Cells/Modules",Subcategory),
         Subcategory = ifelse(Technology=="Batteries" & grepl("EAM",Subcategory),"EAM/Cells/Modules",Subcategory),
         Subcategory = ifelse(Technology=="Batteries" & grepl("Cells",Subcategory),"EAM/Cells/Modules",Subcategory),
         Subcategory = ifelse(Technology=="Critical Minerals" & grepl("Nickel",Subcategory),"Nickel/Cobalt",Subcategory),
         Subcategory = ifelse(Technology=="Critical Minerals" & grepl("Cobalt",Subcategory),"Nickel/Cobalt",Subcategory)) %>%
  group_by(post_IRA,Technology,Subcategory) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T) 

supply_chain_total <- supply_chains %>%
  filter(Technology %in% c("Batteries","Critical Minerals","Zero Emission Vehicles")) %>%
  group_by(post_IRA) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T) 

solar_sc <- supply_chains %>%
  filter(Technology=="Solar") %>%
  pivot_wider(names_from=post_IRA,values_from=Total_Facility_CAPEX_Estimated) %>%
  mutate(pre_ira=round(`0`/18,2),
         post_ira=round(`1`/6,2)) %>%
  select(-`1`,-`0`) %>%
  arrange(desc(`post_ira`)) %>%
  write.csv('Downloads/solar_sc.csv')


wind_sc <- supply_chains %>%
  filter(Technology=="Wind") %>%
  pivot_wider(names_from=post_IRA,values_from=Total_Facility_CAPEX_Estimated) %>%
  mutate(pre_ira=round(`0`/18,2),
         post_ira=round(`1`/6,2)) %>%
  select(-`1`,-`0`) %>%
  arrange(desc(`post_ira`)) %>% 
  write.csv('Downloads/wind_sc.csv')

batteries_sc <- supply_chains %>%
  filter(Technology %in% c("Batteries","Critical Minerals","Zero Emission Vehicles")) %>%
  pivot_wider(names_from=post_IRA,values_from=Total_Facility_CAPEX_Estimated) %>%
  mutate(pre_ira=round(`0`/18,2),
         post_ira=round(`1`/6,2)) %>%
  select(-`1`,-`0`) %>%
  arrange(desc(`post_ira`)) %>%
  write.csv('Downloads/batteries_sc.csv')


#County-Level Data

#Take Latitude & Longitude of Projects and match to Counties
facilities_sf <- st_as_sf(facilities, coords = c("Longitude", "Latitude"), crs = 4326)
options(tigris_class = "sf") # Ensure tigris returns simple features (sf) objects
us_counties <- counties()
us_counties <- st_transform(us_counties, st_crs(facilities_sf))
facilities_with_counties <- st_join(facilities_sf, us_counties)
facilities$County <- facilities_with_counties$NAME
facilities$statefp<-as.numeric(facilities_with_counties$STATEFP)
facilities$countyfp<-as.numeric(facilities_with_counties$COUNTYFP)
facilities$geoid<-paste0(facilities$statefp,facilities$countyfp)
county_stats$geoid<-paste0(county_stats$STATE,county_stats$county)
facilities<-left_join(facilities,county_cbsa,by=c("statefp"="FIPS.State.Code","countyfp"="FIPS.County.Code"))




#Technology Announcements by MSA
facilities_msa_tech <- facilities %>%
  mutate(announce_year=substr(Announcement_Date,1,4)) %>%
  filter(Current_Facility_Status != "C",
         #announce_year %in% c("2022","2023"),
         !is.na(CBSA.Title),
         nzchar(CBSA.Title)) %>%
  group_by(CBSA.Title,Segment,Technology) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T)%>%
  group_by(Segment) %>%
  mutate(Percentile = percent_rank(-Total_Facility_CAPEX_Estimated),
         Rank = rank(-Total_Facility_CAPEX_Estimated,)) %>%
  arrange(desc(Total_Facility_CAPEX_Estimated))%>%
  mutate(msa_name=paste0(CBSA.Title," (MSA)")) %>%
  mutate(industry=ifelse(Segment=="Manufacturing",paste0(Technology," Manufacturing"),Technology)) %>%
  select(-CBSA.Title)

#Technology Announcements by EA
facilities_EA_tech <- facilities %>%
  inner_join(EAs,by=c("fips"="fips")) %>%
  mutate(announce_year=substr(Announcement_Date,1,4)) %>%
  #filter(Current_Facility_Status != "C" & announce_year %in% c("2022","2023")) %>%
  group_by(`EA Name`,Segment,Technology) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T)%>%
  group_by(Segment) %>%
  mutate(Percentile = percent_rank(-Total_Facility_CAPEX_Estimated),
         Rank = rank(-Total_Facility_CAPEX_Estimated,)) %>%
  arrange(desc(Total_Facility_CAPEX_Estimated))%>%
  mutate(msa_name=paste0(`EA Name`," (EA)")) %>%
  mutate(industry=ifelse(Segment=="Manufacturing",paste0(Technology," Manufacturing"),Technology)) %>%
  select(-`EA Name`)

#Top 5 Technologies by EA
facilities_EA_tech5 <- facilities %>%
  inner_join(EAs,by=c("fips"="fips")) %>%
  mutate(announce_year=substr(Announcement_Date,1,4)) %>%
  filter(Current_Facility_Status != "C" & announce_year %in% c("2022","2023")) %>%
  group_by(State,`EA Name`,Segment,Technology,Subcategory) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T)%>%
  group_by(`EA Name`) %>%
  mutate(Rank = rank(-Total_Facility_CAPEX_Estimated)) %>%
  filter(Rank < 6) %>%
  mutate(top_inds = ifelse(Segment=="Manufacturing",paste0(Rank,". ",Subcategory," ",Segment," ($",round(Total_Facility_CAPEX_Estimated,1),"m), "),
                           paste0(Rank,". ",Subcategory," ($",round(Total_Facility_CAPEX_Estimated,1),"m), "))) %>%
  arrange(`EA Name`,Rank) %>%
  select(`EA Name`,Rank,top_inds) %>%
  pivot_wider(names_from=Rank,values_from=top_inds) %>%
  mutate(across(`1`:`5`, ~replace_na(., ""))) %>%
  # Combine the columns into inv_description
  mutate(inv_description = paste(`1`, `2`, `3`, `4`, `5`, sep = "")) %>%
  # Remove commas left at the end of the string
  mutate(inv_description = str_remove_all(inv_description, ", $")) %>%
  select(`EA Name`,inv_description)

#Top 5 Technologies by MSA
facilities_MSA_tech5 <- facilities %>%
  #inner_join(county_cbsa,by=c("county_2020_geoid"="fips")) %>%
  mutate(announce_year=substr(Announcement_Date,1,4)) %>%
  filter(Current_Facility_Status != "C",
         announce_year %in% c("2022","2023"),
         !is.na(CBSA.Title),
         nzchar(CBSA.Title)) %>%
  group_by(CBSA.Code,CBSA.Title,Segment,Technology,Subcategory) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T)%>%
  group_by(CBSA.Code,`CBSA.Title`) %>%
  mutate(Rank = rank(-Total_Facility_CAPEX_Estimated)) %>%
  filter(Rank < 6) %>%
  mutate(top_inds = ifelse(Segment=="Manufacturing",paste0(Rank,". ",Subcategory," ",Segment," ($",round(Total_Facility_CAPEX_Estimated,1),"m), "),
                           paste0(Rank,". ",Subcategory," ($",round(Total_Facility_CAPEX_Estimated,1),"m), "))) %>%
  arrange(CBSA.Title,Rank) %>%
  ungroup() %>%
  select(CBSA.Title,Rank,top_inds) %>%
  pivot_wider(names_from=Rank,values_from=top_inds) %>%
  select(CBSA.Title, `1`:`5`) %>%
  mutate(across(`1`:`5`, ~ifelse(is.na(.), "", as.character(.)))) %>%
  mutate(inv_description = paste(na_if(`1`, "NULL"), na_if(`2`, "NULL"), na_if(`3`, "NULL"), na_if(`4`, "NULL"), na_if(`5`, "NULL"), sep = "")) %>%
  # Remove commas left at the end of the string
  mutate(inv_description = str_remove_all(inv_description, ", NANANANA$")) %>%
  mutate(inv_description = str_remove_all(inv_description, ", NANANA")) %>%
  select(CBSA.Title,inv_description)


#Investment Announcements by Segment, MSA
facilities_msa_segment <- facilities %>%
  #inner_join(county_cbsa,by=c("county_2020_geoid"="fips")) %>%
  mutate(announce_year=substr(Announcement_Date,1,4)) %>%
  filter(Current_Facility_Status != "C",
         announce_year %in% c("2022","2023"),
         !is.na(CBSA.Title),
         nzchar(CBSA.Title)) %>%
  group_by(State,CBSA.Title,Segment) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T)%>%
  group_by(Segment) %>%
  mutate(Percentile = percent_rank(-Total_Facility_CAPEX_Estimated),
         Rank = rank(-Total_Facility_CAPEX_Estimated,)) %>%
  arrange(desc(Total_Facility_CAPEX_Estimated))

#Investment Announcements by Segment, EA
facilities_EA_segment <- facilities %>%
  inner_join(EAs,by=c("fips"="fips")) %>%
  mutate(announce_year=substr(Announcement_Date,1,4)) %>%
  filter(Current_Facility_Status != "C" & announce_year %in% c("2022","2023")) %>%
  group_by(State,`EA Name`,Segment) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T)%>%
  group_by(Segment) %>%
  mutate(Percentile = percent_rank(-Total_Facility_CAPEX_Estimated),
         Rank = rank(-Total_Facility_CAPEX_Estimated,)) %>%
  arrange(desc(Total_Facility_CAPEX_Estimated))


facilities_EA_segment <- facilities_EA_segment %>%
  inner_join(EA_gdp,by=c("EA Name"="EA Name")) %>%
  mutate(inv_gdp=Total_Facility_CAPEX_Estimated/gdp_22) %>%
  group_by(Segment) %>%
  mutate(Relative_Rank=rank(-inv_gdp)) %>%
  arrange(Relative_Rank)

#Total
facilities_msa_total <- facilities %>%
  mutate(announce_year=substr(Announcement_Date,1,4)) %>%
  filter(Current_Facility_Status != "C",
         announce_year %in% c("2022","2023")) %>%
  #right_join(county_cbsa,by=c("county_2020_geoid"="fips")) %>%
  filter(!is.na(CBSA.Title),
         nzchar(CBSA.Title)) %>%
  mutate(Total_Facility_CAPEX_Estimated = ifelse(is.na(Total_Facility_CAPEX_Estimated),0,Total_Facility_CAPEX_Estimated)) %>%
  group_by(CBSA.Code,CBSA.Title) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T)%>%
  ungroup() %>%
  mutate(Percentile = percent_rank(-Total_Facility_CAPEX_Estimated),
         Rank = rank(-Total_Facility_CAPEX_Estimated,)) %>%
  arrange(desc(Total_Facility_CAPEX_Estimated))

facilities_msa_topinvestment <- facilities %>%
  #right_join(county_cbsa,by=c("county_2020_geoid"="fips")) %>%
  filter(!is.na(CBSA.Title),
         nzchar(CBSA.Title),
         !is.na(Total_Facility_CAPEX_Estimated)) %>%
  mutate(announce_year=substr(Announcement_Date,1,4)) %>%
  group_by(CBSA.Title) %>%
  slice_max(order_by=Total_Facility_CAPEX_Estimated,n=1) %>%
  mutate(topinv_desc = paste0("In ", announce_year,", ", Company,ifelse(Current_Facility_Status=="Announced"," announced an investment of "," made an investment "),
                              round(Total_Facility_CAPEX_Estimated,1)," million dollars in a ",ifelse(Segment=="Manufacturing",paste0(Technology," Manufacturing"),Technology),
                              " project.")) %>%
  select(CBSA.Title,topinv_desc)

facilities_msa_total <- facilities_msa_total %>%
  inner_join(msa_gdp,by=c("CBSA.Code"="GeoFips")) %>%
  mutate(gdp_22=as.numeric(X2022)) %>%
  mutate(inv_gdp=Total_Facility_CAPEX_Estimated/gdp_22) %>%
  ungroup() %>%
  mutate(Relative_Rank=rank(-inv_gdp)) %>%
  arrange(Relative_Rank) %>%
  left_join(facilities_MSA_tech5,by=c("CBSA.Title"="CBSA.Title")) %>%
  left_join(facilities_msa_topinvestment,by=c("CBSA.Title"="CBSA.Title")) %>%
  mutate(top50=ifelse(Relative_Rank<51,CBSA.Title,"")) %>%
  mutate(msa_name=paste0(CBSA.Title," (MSA)")) %>%
  mutate(region="MSA")

facilities_EA_total <- facilities %>%
  mutate(announce_year=substr(Announcement_Date,1,4)) %>%
  filter(Current_Facility_Status != "C" & announce_year %in% c("2022","2023")) %>%
  right_join(EAs,by=c("fips"="fips")) %>%
  group_by(`EA Name`) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T)%>%
  ungroup() %>%
  mutate(Percentile = percent_rank(-Total_Facility_CAPEX_Estimated),
         Rank = rank(-Total_Facility_CAPEX_Estimated)) %>%
  arrange(desc(Total_Facility_CAPEX_Estimated)) 

facilities_ea_topinvestment <- facilities %>%
  right_join(EAs,by=c("fips"="fips")) %>%
  filter(!is.na(Total_Facility_CAPEX_Estimated)) %>%
  mutate(announce_year=substr(Announcement_Date,1,4)) %>%
  group_by(`EA Name`) %>%
  slice_max(order_by=Total_Facility_CAPEX_Estimated,n=1) %>%
  mutate(topinv_desc = paste0("In ", announce_year,", ", Company,ifelse(Current_Facility_Status=="Announced"," announced an investment of "," made an investment "),
                              round(Total_Facility_CAPEX_Estimated,1)," million dollars in a ",ifelse(Segment=="Manufacturing",paste0(Technology," Manufacturing"),Technology),
                              " project.")) %>%
  select(`EA Name`,topinv_desc)

facilities_EA_total <- facilities_EA_total %>%
  inner_join(EA_gdp,by=c("EA Name"="EA Name")) %>%
  mutate(inv_gdp=Total_Facility_CAPEX_Estimated/gdp_22) %>%
  ungroup() %>%
  mutate(Relative_Rank=rank(-inv_gdp)) %>%
  arrange(Relative_Rank) %>%
  left_join(facilities_EA_tech5,by=c("EA Name"="EA Name")) %>%
  left_join(facilities_ea_topinvestment,by=c("EA Name"="EA Name")) %>%
  mutate(top50=ifelse(Relative_Rank<51,`EA Name`,"")) %>%
  mutate(msa_name=paste0(`EA Name`," (EA)")) %>%
  mutate(region="EA") 


#Federal Tax Credit Incentives State-Level Estimates
tax_inv_cat<-read.csv('C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/clean_investment_monitor_q1_24/tax_investment_by_category.csv',skip=2)
tax_inv_state<-read.csv('C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/clean_investment_monitor_q1_24/tax_investment_by_state.csv',skip=2)

#45X
fac_45x<-facilities %>%
  filter(Segment=="Manufacturing",
         Technology %in% c("Solar",
                           "Wind",
                           "Critical Minerals",
                           "Batteries"),
         Current_Facility_Status=="O") %>%
  group_by(State,Segment) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T) %>%
  group_by(Segment) %>%
  mutate(cap_share=Total_Facility_CAPEX_Estimated/sum(Total_Facility_CAPEX_Estimated)) %>%
  left_join(tax_inv_cat %>% filter(Category=="Advanced Manufacturing Tax Credits"),by=c("Segment")) %>%
  left_join(tax_inv_state %>% select(State,Total.Federal.Investment..2022.Million.USD.),by=c("State")) %>%
  mutate(state_45x = Total.Federal.Investment.2022USBn*cap_share) 

#45V & 45Q
fac_45vq<-investment %>%
  filter(Segment=="Energy and Industry",
         Technology %in% c("Hydrogen")|
           Technology=="Carbon Management" & Subcategory %in% c("CCUS","Direct Air Capture")|
           Technology=="Sustainable Aviation Fuels") %>%
  group_by(State,Segment) %>%
  summarize_at(vars(Estimated_Actual_Quarterly_Expenditure),sum,na.rm=T) %>%
  group_by(Segment) %>%
  mutate(cap_share=Estimated_Actual_Quarterly_Expenditure/sum(Estimated_Actual_Quarterly_Expenditure)) %>%
  left_join(tax_inv_cat %>% filter(Category=="Emerging Climate Technology Tax Credits"),by=c("Segment")) %>%
  left_join(tax_inv_state %>% select(State,Total.Federal.Investment..2022.Million.USD.),by=c("State")) %>%
  mutate(state_45vq = Total.Federal.Investment.2022USBn*cap_share) 

#45
url <- 'https://www.eia.gov/electricity/data/eia860m/xls/april_generator2024.xlsx'
destination_folder<-'OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/States Data/'
file_path <- paste0(destination_folder, "eia_op_gen.xlsx")
downloaded_content <- GET(url, write_disk(file_path, overwrite = TRUE))

#Operating Generation
op_gen <- read_excel(file_path, sheet = 1,skip=2)

state_45 <- op_gen %>%
  filter(Status=="(OP) Operating",
         Technology %in% c("Onshore Wind Turbine",
                           "Solar Photovoltaic",
                           "Batteries",
                           "Solar Thermal with Energy Storage",
                           "Geothermal",
                           "Conventional Hydroelectric",
                           "Landfill Gas",
                           "Wood/Wood Waste Biomass")) %>%
  group_by(`Plant State`) %>%
  summarize_at(vars(`Nameplate Capacity (MW)`),sum,na.rm=T) %>%
  ungroup() %>%
  mutate(share_mw=`Nameplate Capacity (MW)`/sum(`Nameplate Capacity (MW)`)) %>%
  left_join(tax_inv_state %>% select(State,Total.Federal.Investment..2022.Million.USD.),by=c("Plant State"="State")) %>%
  cbind(tax_inv_cat %>% filter(Category=="Clean Electricity Tax Credits")) %>%
  mutate(state_45 = Total.Federal.Investment.2022USBn*share_mw)

#48
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

state_48 <- rooftop_state %>%
  filter(!State %in% c("US"),
         !is.na(State)) %>%
  mutate(com_gen = replace_na(com_gen, 0),
         res_gen = replace_na(res_gen, 0),
         ind_gen = replace_na(ind_gen, 0)) %>%
  select(State,ind_gen,com_gen,res_gen) %>%
  mutate(com_share=(ind_gen+com_gen)/sum((ind_gen+com_gen)),na.rm=T,
         res_share=res_gen/sum(res_gen),na.rm=T) %>%
  left_join(tax_inv_state %>% select(State,Total.Federal.Investment..2022.Million.USD.),by=c("State")) %>%
  cbind(tax_inv_cat %>% filter(Category=="Non-residential Distributed Energy Tax Credits")) %>%
  left_join(tax_inv_cat %>% filter(Category=="Residential Energy & Efficiency Tax Credits"),by=c("Segment")) %>%
  mutate(state_48_res = Total.Federal.Investment.2022USBn.y*(res_share)) %>%
  mutate(state_48_com=  Total.Federal.Investment.2022USBn.x*(com_share))

state_48 <- investment %>%
  filter(Segment=="Retail",Technology %in% c("Heat Pumps","Distributed Electricity and Storage"),
         quarter %in% c("2022-Q2",
                        "2022-Q3",
                        "2022-Q4",
                        "2023-Q1",
                        "2023-Q2",
                        "2023-Q3",
                        "2023-Q4",
                        "2024-Q1")) %>%
  group_by(State,Segment) %>%
  summarize_at(vars(Estimated_Actual_Quarterly_Expenditure),sum,na.rm=T) %>%
  group_by(Segment) %>%
  mutate(share=Estimated_Actual_Quarterly_Expenditure/sum(Estimated_Actual_Quarterly_Expenditure)) %>%
  left_join(tax_inv_cat %>% filter(Category=="Non-residential Distributed Energy Tax Credits"),by=c("Segment")) %>%
  left_join(tax_inv_cat %>% filter(Category=="Residential Energy & Efficiency Tax Credits"),by=c("Segment")) %>%
  mutate(state_48_res = Total.Federal.Investment.2022USBn.y*(share)) %>%
  mutate(state_48_com=  Total.Federal.Investment.2022USBn.x*(share))

#zev
zev<-investment %>%
  filter(Segment=="Retail",Technology=="Zero Emission Vehicles",
         quarter %in% c("2022-Q2",
                        "2022-Q3",
                        "2022-Q4",
                        "2023-Q1",
                        "2023-Q2",
                        "2023-Q3",
                        "2023-Q4",
                        "2024-Q1")) %>%
  group_by(State,Segment) %>%
  summarize_at(vars(Estimated_Actual_Quarterly_Expenditure),sum,na.rm=T) %>%
  group_by(Segment) %>%
  mutate(share_ev=Estimated_Actual_Quarterly_Expenditure/sum(Estimated_Actual_Quarterly_Expenditure)) %>%
  left_join(tax_inv_cat %>% filter(Category=="Zero Emission Vehicle Tax Credits"),by=c("Segment")) %>%
  mutate(state_zev = Total.Federal.Investment.2022USBn*share_ev)

#combine
state_estimates<-state_45 %>%
  rename(State=`Plant State`) %>%
  select(State,state_45) %>%
  left_join(fac_45x %>% select(State,state_45x),by=c("State")) %>%
  left_join(fac_45vq %>% select(State,state_45vq),by=c("State")) %>%
  left_join(state_48 %>% select(State,state_48_res,state_48_com),by=c("State")) %>%
  left_join(zev %>% select(State,state_zev),by=c("State")) %>%
  ungroup() %>%
  select(-geometry,-Segment.x,-Segment.y) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  mutate(total=(state_45+state_45x+state_45vq+state_48_res+state_48_com+state_zev)) %>%
  left_join(tax_inv_state %>% select(State,Total.Federal.Investment..2022.Million.USD.),by=c("State")) %>%
  mutate("Clean Electricity Tax Credits"=state_45/total*Total.Federal.Investment..2022.Million.USD.,
         "Advanced Manufacturing Tax Credits"=state_45x/total*Total.Federal.Investment..2022.Million.USD.,
         "Emerging Climate Technology Tax Credits"=state_45vq/total*Total.Federal.Investment..2022.Million.USD.,
         "Residential Energy & Efficiency Tax Credits"=state_48_res/total*Total.Federal.Investment..2022.Million.USD.,
         "Non-residential Distributed Energy Tax Credits"=state_48_com/total*Total.Federal.Investment..2022.Million.USD.,
         "Zero Emission Vehicle Tax Credits"=state_zev/total*Total.Federal.Investment..2022.Million.USD.)

cat_estimate<- state_estimates %>%
  mutate(across(where(is.numeric),~sum(.)))

state_estimates2<-state_estimates %>%
  select(State,`Clean Electricity Tax Credits`,
         `Advanced Manufacturing Tax Credits`,
         `Emerging Climate Technology Tax Credits`,
         `Residential Energy & Efficiency Tax Credits`,
         `Non-residential Distributed Energy Tax Credits`,
         `Zero Emission Vehicle Tax Credits`) %>%
  pivot_longer(cols=-State,names_to="Category",values_to="Federal Investment (millions 2022 USD)") %>%
  left_join(tax_inv_state %>% select(State,State.GDP..2022.Million.USD.),by=c("State")) %>%
  mutate("Federal Investment (millions 2022 USD)"=round(`Federal Investment (millions 2022 USD)`,2),
         "State GDP (millions 2022 USD)"=round(State.GDP..2022.Million.USD.,2),
         "Federal Investment (% of State GDP)"=round(`Federal Investment (millions 2022 USD)`/`State GDP (millions 2022 USD)`*100,2)) 

ggplot(data=state_estimates2) +
  geom_col(aes(x=reorder(State,-`Federal Investment (millions 2022 USD)`),y=`Federal Investment (millions 2022 USD)`,fill=Category),position="stack") +
  coord_flip() +
  scale_fill_manual(values=rmi_palette) +
  labs(title = "Federal IRA Investment by State, Cateogry", 
       subtitle = "",
       x="State",
       fill = "Tax Credit",
       caption="Source: Clean Investment Monitor")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(legend.position=c(0.8,0.8)) 

ggplot(data=state_estimates2) +
  geom_col(aes(x=reorder(State,-`Federal Investment (% of State GDP)`),y=`Federal Investment (% of State GDP)`,fill=Category),position="stack") +
  coord_flip() +
  scale_fill_manual(values=rmi_palette) +
  labs(title = "Federal IRA Investment by State, Cateogry", 
       subtitle = "Percentage of 2022 GDP",
       x="State",
       fill = "Tax Credit",
       caption="Source: Clean Investment Monitor")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(legend.position=c(0.8,0.8)) 


#State Map

# Get US state map data
us_states <-us_map("states")
state_map_data <- left_join(us_states, tax_inv_state, by = c("abbr" = "State"))
state_map_data <- state_map_data%>%
  mutate(invcap_bin=cut(Federal.Investment..2022.USD.per.capita.,breaks=c(0,50,100,250,500,1000),labels=c("0-$50","$50-100","$100-250","$250-500","$400-500","$500+")))


state_labels<-centroid_labels(regions = c("states"))
state_labels <- state_labels %>%
  left_join(tax_inv_state,by=c("abbr"="State"))

# Plotting the US state map with investment data
state_ira_map<-ggplot() +
  geom_polygon(data = state_map_data, aes(x=x,y=y,group=group,fill = invcap_bin), color = "white") +
  geom_text(data = state_labels, aes(x = x, y = y, label = abbr), size = 2, color = "black", fontface = "bold") +
  scale_fill_manual(values=rmi_palette)+
  #scale_fill_gradient2(low="#E63946",high="#2A9D8F", midpoint=median(tax_inv_state$Federal.Investment..2022.USD.per.capita.),na.value = "grey90", name = "Tax Credit $ per person") +
  labs(title = "Federal Tax Credits per Capita, 2022", 
       subtitle = "",
       fill = "2022 USD per capita",
       caption="Source: Clean Investment Monitor") +
  theme_void()+
  theme(legend.position=c(0.9,0.1),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"))

ggsave(file.path(output_folder, paste0(state_abbreviation,"state_ira_map", ".png")), 
       plot = state_ira_map,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)


#Regional State Comparisons
division_of_interest<-census_divisions %>%
  filter(State.Code==state_abbreviation)

state_ira <- state_estimates2 %>%
  left_join(census_divisions,by=c("State"="State.Code")) %>%
  filter(Division == division_of_interest$Division) 

state_ira_plot<-ggplot(data=state_ira) +
  geom_col(aes(x=reorder(State,-`Federal Investment (% of State GDP)`),y=`Federal Investment (% of State GDP)`,fill=Category),position="stack") +
  #coord_flip() +
  scale_fill_manual(values=rmi_palette) +
  labs(title = paste("Federal IRA Investment in the ",division_of_interest$Division," Division by Tax Credit"),
       subtitle = "Percentage of 2022 GDP",
       x="State",
       fill = "Tax Credit",
       caption="Source: Clean Investment Monitor")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(legend.position=c(0.8,0.8),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white")) 

  

ggsave(file.path(output_folder, paste0(state_abbreviation,"_state_ira_plot", ".png")), 
       plot = state_ira_plot,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)
  
  