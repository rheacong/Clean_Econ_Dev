#Employment Data

#This script contains:
#1. County Business Patterns Data download
#2. Fossil Fuel Employment Location Quotient Map
#3. Employment Diversity (Hachman Index) Map


# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "NM"  # Replace with any US state abbreviation
state_name <- "New Mexico"  # Replace with the full name of any US state
region_name <- "Great Falls, MT"

#Make a region_id for your state/region of interest
region_id <- us_counties %>%
  filter(abbr == state_abbreviation) 

#County Business Patterns
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
cbp_21$naics2017<-as.numeric(cbp_21$NAICS2017)
#cbp_21 <-left_join(cbp_21,eti_long,by=c("naics2017"="6-Digit Code"))

#Six Digit Level
cbp_21_6d <- cbp_21 %>%
  filter(INDLEVEL=="6")

#Two Digit Level
cbp21_2d <- cbp_21 %>%
  select(-fips) %>%
  mutate(state=as.numeric(STATE)) %>%
  filter(INDLEVEL=="2")  %>%
  mutate(FIPS=paste0(STATE, COUNTY)) %>%
  left_join(EAs,by=c("FIPS"="FIPS")) %>%
  left_join(naics2017 %>% select(`2017 NAICS US   Code`,`2017 NAICS US Title`),by=c("NAICS2017"="2017 NAICS US   Code")) %>%
  rename(naics_desc=`2017 NAICS US Title`)

#Filter just for region of interest
region_cbp_2d <- cbp21_2d %>%
  filter(abbr==state_abbreviation) %>%
  mutate(region_id=ifelse(fips %in% region_id$fips,1,0)) %>%
  mutate(code=ifelse(NAICS2017 %in% c("00","11","21","22","23","31-33","42","48-49","54"),NAICS2017,"Other")) %>%
  group_by(region_id,code) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  group_by(code) %>%
  mutate(share=EMP/sum(EMP,na.rm=T)) 

#Total Employment in Region
region_totalemp<-region_cbp_2d %>%
  filter(code=="00",
         region_id==1) 

#Calculate proportions
total_emp <- cbp21_2d %>%
  filter(NAICS2017=="00")
total_emp_nat<-cbp21_2d  %>%
  filter(NAICS2017=="00") %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  ungroup() 

#Fossil Fuel Employment
fossil_codes <- tibble(
  NAICS_code = c(211, 2121, 213111, 213112, 213113, 32411, 4861, 4862),
  Description = c("Oil and Gas Extraction",
                  "Coal Mining",
                  "Drilling Oil and Gas Wells",
                  "Support Activities for Oil and Gas Operations",
                  "Support Activities for Coal Mining",
                  "Petroleum Refineries",
                  "Pipeline Transportation of Crude Oil",
                  "Pipeline Transportation of Natural Gas"))

fossil_emp_national <- cbp_21 %>%
  mutate(fossil = ifelse(NAICS2017 %in% fossil_codes$NAICS_code,1,0)) %>%
  group_by(fossil) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  mutate(EMP_nat=total_emp_nat$EMP)%>%
  ungroup() %>%
  mutate(emp_share_national = EMP / EMP_nat)

fossil_emp_county <- cbp_21 %>%
  mutate(fossil = ifelse(NAICS2017 %in% fossil_codes$NAICS_code,1,0)) %>%
  filter(fossil==1) %>%
  group_by(abbr,full,STATE,COUNTY,fossil) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  left_join(total_emp %>% select(STATE,COUNTY,EMP), by = c("STATE"="STATE","COUNTY"="COUNTY"), suffix = c("", "_total")) %>%
  select(abbr,full,STATE,COUNTY,EMP,EMP_total,fossil) %>%
  ungroup() %>%
  mutate(emp_share = EMP / EMP_total) %>%
  left_join(fossil_emp_national, by = c("fossil" = "fossil")) %>%
  mutate(lq=emp_share/emp_share_national)

fossil_emp_map <- fossil_emp_county %>%
  filter(full==state_name) %>%
  mutate(FIPS=paste0(STATE,COUNTY))%>%
  select(full,FIPS,fossil,lq) %>%
  pivot_wider(names_from=fossil,values_from=lq) %>%
  mutate(region_id=ifelse(FIPS %in% region_counties$fips,1,1),
         lq = ifelse(is.na(`1`),0,`1`)) %>%
  select(full,FIPS,region_id,lq) 

#Fossil Fuel Employment-Map
us_counties<-us_map("counties")
fossil_map_data <- inner_join(us_counties, fossil_emp_map, by = c("fips" = "FIPS"))
fossil_map_data <- fossil_map_data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
  mutate(lq_bin=cut(lq, breaks = c(-1, 1, 5, 20, 100), labels = c("Below Average (0-1)","Above Average (1-5)", "High (5-20)", "Very High (20+)"))) 

county_labels<-centroid_labels(regions = c("counties"))
county_labels <- county_labels %>%
  inner_join(fossil_emp_map,by=c("fips"="FIPS")) 
#%>%  mutate(NAME=ifelse(region_id==1,NAME,""))

library(ggrepel)
fossil_lq_map<-ggplot() +
  geom_polygon(data = fossil_map_data, aes(x = x, y = y, group = group, fill = lq_bin, alpha = region_id), color = "darkgrey") +
  geom_text_repel(data = county_labels, aes(x = x, y = y, label = county), size = 2, color = "black", fontface = "bold") +
  scale_fill_manual(values=rmi_palette, name = "Specialization") +
  scale_alpha_identity() +
  labs(title = paste("Fossil Fuel Specialization in ", state_name), 
       subtitle = "Fossil employment share by county, relative to the national average.",
       fill = "Location Quotient",
       caption = "Source: Census Bureau, County Business Patterns") +
  theme_void() +
  theme(legend.position = "right",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"))

ggsave(file.path(output_folder, paste0(state_abbreviation,"_fossil_lq_map", ".png")), 
       plot = fossil_lq_map,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)

#Bar Charts
fossil_emp_state <- fossil_emp_county %>%
  filter(full==state_name) %>%
  mutate(FIPS=paste0(STATE,COUNTY)) %>%
  left_join(county_labels,by=c("FIPS"="fips")) 


#Diversity
emp_proportions <- cbp21_2d %>%
  left_join(total_emp %>% select(state,county,full,NAME,EMP), by = c("NAME"="NAME","full"="full","state"="state","county"="county","GEOID"="GEOID"), suffix = c("", "_total")) %>%
  select(GEOID,state,county,full,NAME,NAICS2017,naics_desc,EMP,EMP_total) %>%
  group_by(GEOID,state,county,full,NAME,NAICS2017,naics_desc) %>%
  mutate(emp_share=EMP/EMP_total) %>%
  filter(!is.na(full)) %>%
  arrange(desc(emp_share))

# National-level proportions
national_proportions <- emp_proportions %>%
  ungroup() %>%
  select(NAICS2017, naics_desc, EMP) %>%
  group_by(NAICS2017, naics_desc) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  ungroup()%>%
  mutate(emp_share_national = EMP / sum(EMP, na.rm = TRUE))

#Location Quotients
location_quotients_county <- emp_proportions %>%
  left_join(national_proportions, by = c("NAICS2017"="NAICS2017","naics_desc"="naics_desc")) %>%
  mutate(LQ = emp_share / emp_share_national,
         weighted_LQ = LQ * emp_share)  # Weighting by regional share

# Compute the Hachman index
hachman_indices_county <- location_quotients_county %>%
  filter(full==state_name) %>%
  mutate(region_id=ifelse(GEOID %in% region_counties$fips,1,0)) %>% # Identify the region of interest
  group_by(NAME,GEOID,region_id) %>%
  summarize(HI = 100 / sum(weighted_LQ, na.rm = TRUE), .groups = 'drop') %>% # Reciprocal of the sum of weighted LQs scaled by 100
  mutate(HI_bin = cut(HI, breaks = c(0, 50,60,70,80,90, 100), labels = c("Very Low (0-50)","Low (50-60)", "Low-Medium (60-70)","Medium-High (70-80)", "High (80-90)", "Very High (90-100)")))


# Plotting the state map with Hachman Index

# Get US county map data

county_map_data <- inner_join(us_counties, hachman_indices_county, by = c("fips" = "GEOID"))
county_map_data <- county_map_data %>%
  mutate(alpha = ifelse(region_id == 1, 1, 0.5))

county_labels<-centroid_labels(regions = c("counties"))
county_labels <- county_labels %>%
  inner_join(hachman_indices_county,by=c("fips"="GEOID"))

hachman_map_county_d<-ggplot() +
  geom_polygon(data = county_map_data, aes(x = x, y = y, group = group, fill = HI_bin, alpha = alpha), color = "grey") +
  geom_text(data = county_labels, aes(x = x, y = y, label = NAME), size = 2, color = "white", fontface = "bold") +
  scale_fill_manual(values = rmi_palette, na.value = "grey90", name = "Hachman Index") +
  scale_alpha_identity() +
  labs(title = paste("Economic Diversity within ", state_name), 
       subtitle = "A Hachman Index score ranges from 0 to 100. A higher score indicates that the subject area's industrial distribution more closely resembles that of the US as a whole, and is therefore diverse.",
       fill = "Hachman Index",
       caption = "Source: Census Bureau, County Business Patterns") +
  theme_void() +
  theme(legend.position = c(0.9, 0.1),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"))


#Quarterly Census of Employment and Wages

#2018:2023 data for industries in Clean Investment MOnitory
#Fastest:
cim_qcew <- read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/QCEW.csv')

#Latest (but very slow)
cim_qcew <- data.frame()

# Loop for years before 2022 - note use of NAICS2017 codes
for (year in c('2018', '2019', '2020', '2021')) {
  
  # Loop over each quarter
  for (quarter in c('1', '2', '3', '4')) {
    
    # Loop over each industry
    for (industry_code in tech_mapping$`6-Digit Code`) {
      
      # Pull data for the current combination of year, quarter, and industry
      current_data <- blsQCEW(method = 'Industry',
                              year = year,
                              quarter = quarter,
                              industry = industry_code)
      
      # Append the current data to the overall data frame
      cim_qcew <- rbind(cim_qcew, current_data)
      
    }
  }
}

# Loop for years 2022 and first three quarters of 2023 - note use of NAICS2022 codes
for (year in c('2022', '2023')) {
  
  # Determine quarters to loop over based on the year
  quarters <- if (year == '2023') c('1', '2', '3') else c('1', '2', '3', '4')
  
  # Loop over each quarter
  for (quarter in quarters) {
    
    # Loop over each industry for years 2022 and first three quarters of 2023
    for (industry_code in tech_mapping$`2022 NAICS Code`) {
      
      # Pull data for the current combination of year, quarter, and industry
      current_data <- blsQCEW(method = 'Industry',
                              year = year,
                              quarter = quarter,
                              industry = industry_code)
      
      # Append the current data to the overall data frame
      cim_qcew <- rbind(cim_qcew, current_data)
      
    }
  }
}



# US Total Employment
cim_qcew_us <- cim_qcew %>%
  filter(area_fips == "US000") %>%
  select(3,6,7,10:12) %>%
  pivot_longer(cols = c(month1_emplvl, month2_emplvl, month3_emplvl), 
               names_to = "month",
               values_to = "emplvl") %>%
  mutate(date = as.Date(paste0(year, "-", sprintf("%02d", qtr *3-(3-as.integer(gsub("\\D", "", month)))), "-01"))) %>%
  left_join(tech_mapping,by=c("industry_code"="6-Digit Code")) %>%
  left_join(tech_mapping,by=c("industry_code"="2022 NAICS Code")) %>%
  mutate(Segment=ifelse(is.na(Segment.x),Segment.y,Segment.x),
         Technology=ifelse(is.na(Technology.x),Technology.y,Technology.x)) %>%
  mutate(technology=ifelse(Segment=="Manufacturing",paste0(Technology," ",Segment),Technology)) %>%
  distinct(date,technology,industry_code,emplvl)

cim_qcew_us_tech <- cim_qcew_us %>%
  group_by(date,technology) %>%
  summarize_at(vars(emplvl),sum,na.rm=T) %>%
  group_by(technology) 
#mutate(ira_index = 100*emplvl/emplvl[date=="2022-08-01"]) 

#2021-2023 Growth
qcew_us_2123 <- cim_qcew_us_tech %>%
  #select(-`EA Name`,-ira_index) %>%
  filter(date =="2021-01-01"|date =="2023-09-01") %>%
  pivot_wider(names_from=date,values_from=emplvl) %>%
  mutate(emp_2123=`2023-09-01`-`2021-01-01`,
         emp_2123_perc=emp_2123/`2021-01-01`*100) %>%
  arrange(desc(emp_2123))

ira_emp_wide <- cim_qcew_us_tech%>%
  select(date,technology,ira_index) %>%
  pivot_wider(names_from=technology,values_from=ira_index)

write.csv(ira_emp_wide,"Downloads/ira_emp_wide.csv")


# US Average Wages
cim_qcew_us_wage <- cim_qcew %>%
  filter(area_fips == "US000") %>%
  select(3,6,7,16) %>%
  mutate(date = as.Date(paste0(year, "-", sprintf("%02d", qtr *3-2), "-01"))) %>%
  filter(qtr=="3") %>%
  left_join(tech_mapping,by=c("industry_code"="6-Digit Code")) %>%
  left_join(tech_mapping,by=c("industry_code"="2022 NAICS Code")) %>%
  mutate(Segment=ifelse(is.na(Segment.x),Segment.y,Segment.x),
         Technology=ifelse(is.na(Technology.x),Technology.y,Technology.x)) %>%
  mutate(technology=ifelse(Segment=="Manufacturing",paste0(Technology," ",Segment),Technology)) %>%
  distinct(date,technology,industry_code,avg_wkly_wage)

cim_qcew_us_tech_wage <- cim_qcew_us_wage %>%
  group_by(date,technology) %>%
  summarize_at(vars(avg_wkly_wage),mean,na.rm=T) %>%
  group_by(technology) %>%
  mutate(ira_index = 100*avg_wkly_wage/avg_wkly_wage[date=="2022-07-01"]) 

ira_wage_wide <- cim_qcew_us_tech_wage%>%
  select(date,technology,avg_wkly_wage) %>%
  pivot_wider(names_from=technology,values_from=avg_wkly_wage)

write.csv(ira_wage_wide,"C:/Users/LCarey.RMI/Downloads/ira_wage_wide.csv")


# US Establishments
cim_qcew_us_estab <- cim_qcew %>%
  filter(area_fips == "US000") %>%
  select(3,6,7,9) %>%
  mutate(date = as.Date(paste0(year, "-", sprintf("%02d", qtr *3-2), "-01"))) %>%
  left_join(tech_mapping,by=c("industry_code"="6-Digit Code")) %>%
  left_join(tech_mapping,by=c("industry_code"="2022 NAICS Code")) %>%
  mutate(Segment=ifelse(is.na(Segment.x),Segment.y,Segment.x),
         Technology=ifelse(is.na(Technology.x),Technology.y,Technology.x)) %>%
  mutate(technology=ifelse(Segment=="Manufacturing",paste0(Technology," ",Segment),Technology)) %>%
  distinct(date,technology,industry_code,qtrly_estabs)

cim_qcew_us_tech_estab <- cim_qcew_us_estab %>%
  group_by(date,technology) %>%
  summarize_at(vars(qtrly_estabs),mean,na.rm=T) %>%
  group_by(technology) %>%
  mutate(ira_index = 100*qtrly_estabs/qtrly_estabs[date=="2022-07-01"]) 

ira_qtrly_estabs_wide <- cim_qcew_us_tech_estab%>%
  select(date,technology,qtrly_estabs) %>%
  pivot_wider(names_from=technology,values_from=qtrly_estabs)

write.csv(ira_qtrly_estabs_wide,"C:/Users/LCarey.RMI/Downloads/ira_qtrly_estabs_wide.csv")


#state Clean Energy Employment
cim_qcew_state <- cim_qcew %>%
  filter(agglvl_code=="58") %>%  
  select(1,3,6,7,10:12) %>%
  pivot_longer(cols = c(month1_emplvl, month2_emplvl, month3_emplvl), 
               names_to = "month",
               values_to = "emplvl") %>%
  mutate(date = as.Date(paste0(year, "-", sprintf("%02d", qtr *3-(3-as.integer(gsub("\\D", "", month)))), "-01"))) %>%
  left_join(tech_mapping,by=c("industry_code"="6-Digit Code")) %>%
  left_join(tech_mapping,by=c("industry_code"="2022 NAICS Code")) %>%
  mutate(Segment=ifelse(is.na(Segment.x),Segment.y,Segment.x),
         Technology=ifelse(is.na(Technology.x),Technology.y,Technology.x)) %>%
  mutate(technology=ifelse(Segment=="Manufacturing",paste0(Technology," ",Segment),Technology)) %>%
  distinct(area_fips,date,technology,industry_code,emplvl) 

cim_qcew_states <-cim_qcew_state %>%
  group_by(area_fips,date,technology) %>%
  summarize_at(vars(emplvl),sum,na.rm=T)  %>%
  group_by(area_fips,technology) %>%
  mutate(ira_index = 100*emplvl/emplvl[date=="2022-07-01"],na.rm=T) 


qcew_states_2123 <- cim_qcew_states %>%
  filter(date =="2021-01-01"|date =="2023-09-01") %>%
  pivot_wider(names_from=date,values_from=emplvl) %>%
  mutate(emp_2123=`2023-09-01`-`2021-01-01`,
         emp_2123_perc=emp_2123/`2021-01-01`*100) %>%
  left_join(state_gdp %>% select(GeoFips,GeoName,gdp_22),by=c("area_fips"="GeoFips"))

qcew_states_2123_top1 <- qcew_states_2123 %>%
  #filter(technology != "Wind Manufacturing",
  #      technology != "Storage Manufacturing",
  #     technology != "Solar Manufacturing") %>%
  mutate(technology = case_when(
    technology == "Storage"~"Generation, Transmission & Storage Construction" ,
    technology == "Zero Emission Vehicles Manufacturing" ~ "Vehicle Manufacturing",
    technology == "SAF" ~ "Biofuels",
    technology == "Solar" ~ "Solar Operations",
    technology == "Wind" ~ "Wind Operations",
    technology == "Wind Manufacturing" ~ "Turbine (incl Wind) and Related Machinery Manufacturing",
    technology == "Solar Manufacturing" ~ "Electrical & Semiconductor Equipment (incl Solar) Manufacturing",
    TRUE ~ technology
  )) %>%
  group_by(GeoName,area_fips) %>%
  slice_max(`2023-09-01`,n=1) 
write.csv(qcew_states_2123_top1,"C:/Users/LCarey.RMI/Downloads/qcew_states_top1.csv")


cip_qcew_state_spec<-cim_qcew_states %>%
  left_join(state_inv_spec,by=c("area_fips"="GeoFips"))

solar_spec<- cip_qcew_state_spec %>%
  select(State.Name,date,technology.x,emplvl) %>%
  #rbind(cim_qcew_us_tech) %>%
  filter(technology.x=="Batteries Manufacturing") %>%
  filter(!is.na(State.Name))

ggplot(data=solar_spec,aes(x=date,y=emplvl,color=State.Name))+
  geom_line()


#Latest Employment - all industries

eti_long<-left_join(eti_long,naics2022 %>% select("2022 NAICS Code",
                                                  "2022 NAICS Title",
                                                  "2017 NAICS Code"),
                    by=c("6-Digit Code"="2017 NAICS Code"))

state_qcew<-data.frame()
state_gdp<-state_gdp %>% slice(2:52)
for (area_code in state_gdp$GeoFips) {
  current_data <- blsQCEW(method = 'Area',
                          year = '2023',
                          quarter = '3',
                          area = area_code)
  state_qcew <- rbind(state_qcew, current_data)
}

all_industries<-blsQCEW(method = 'Industry',
                        year = '2023',
                        quarter = '3',
                        industry = '10')

all_state_industries <- all_industries %>%
  filter(own_code=="5",agglvl_code=="51") %>%
  select(area_fips,month3_emplvl)
all_us_industries <- all_industries %>%
  filter(own_code=="5",area_fips=="US000") %>%
  select(area_fips,month3_emplvl)

US_emp <-state_qcew %>%
  filter(own_code=="5") %>%
  mutate(industry_code=as.numeric(industry_code)) %>%
  inner_join(eti_long,by=c("industry_code"="2022 NAICS Code")) %>%
  rename(tech="Primary Transition Products / Technologies") %>%
  filter(!is.na(industry_code)) %>%
  distinct(area_fips,tech,industry_code,`2022 NAICS Title`,month3_emplvl) %>%
  group_by(tech) %>%
  summarize_at(vars(month3_emplvl),sum,na.rm=T) %>%
  rename(US_ind_emp=month3_emplvl)%>%
  cbind(all_us_industries) %>%
  mutate(emp_share_us=US_ind_emp/month3_emplvl)


state_emp <- state_qcew %>%
  filter(own_code=="5",
         agglvl_code=="58") %>%
  mutate(industry_code=as.numeric(industry_code)) %>%
  inner_join(eti_long,by=c("industry_code"="2022 NAICS Code")) %>%
  rename(tech="Primary Transition Products / Technologies") %>%
  filter(`Transition Sector Category` %in% c("Energy End-Use Sector",
                                             "Industrial End-Use Sector",
                                             "Transportation End-Use Sector",
                                             "Transition Chemical, Mineral, and Metal Manufacturing Sector",
                                             "Transition Mineral and Metal Mining Sector")) %>%
  filter(!is.na(industry_code)) %>%
  group_by(area_fips,tech) %>%
  summarize_at(vars(month3_emplvl),sum,na.rm=T) %>%
  inner_join(state_gdp %>% select(GeoFips,GeoName,gdp_22) %>% mutate(GeoFips =as.numeric(GeoFips)),by=c("area_fips"="GeoFips")) %>%
  inner_join(all_state_industries %>% mutate(area_fips =as.numeric(area_fips)),by=c("area_fips"="area_fips")) %>%
  mutate(emp_share=month3_emplvl.x/month3_emplvl.y) %>%
  inner_join(US_emp,by=c("tech"="tech")) %>%
  mutate(lq=emp_share/emp_share_us) %>%
  arrange(desc(lq)) %>%
  distinct(GeoName,tech,month3_emplvl.x,emp_share,lq)

state_spec_1 <- state_emp %>%
  group_by(GeoName) %>%
  slice_max(order_by=lq,n=1) %>%
  mutate(tech = ifelse(grepl("Vehicles", tech, ignore.case = TRUE), 
                       "Electric Vehicles", 
                       tech))
write.csv(state_spec_1,'C:/Users/LCarey.RMI/Downloads/state_spec_1.csv')


