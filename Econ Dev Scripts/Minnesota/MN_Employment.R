#Employment Data
# Last edited by Rhea Cong, 08/06/2024

### INPUTS
# County Business Patterns Data
# BLS Quarterly Census of Employment and Wages (QCEW)

### OUTPUTS
# Fossil Fuel Employment Location Quotient Map
# Employment Diversity (Hachman Index) Map
# calculates top LQ in the region by industry
# employment and wage figures for CIM industries at national level

# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "MN"  # Replace with any US state abbreviation
state_name <- "Minnesota"  # Replace with the full name of any US state
region_name <- "Minneapolis-St. Paul-Bloomington, MN-WI"

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

#Filter just for state of interest
region_cbp_2d <- cbp21_2d %>%
  filter(state==states_simple$fips[states_simple$abbr == state_abbreviation]) %>%
  mutate(region_id=ifelse(fips %in% region_id$fips,1,0)) %>%
  mutate(code=ifelse(NAICS2017 %in% c("00","11","21","22","23","31-33","42","48-49","54"),NAICS2017,"Other")) %>%
  group_by(region_id,code) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  #group_by(code) %>%
  mutate(share=EMP/EMP[1]) 

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
mn_counties <- us_counties %>% filter(full=="Minnesota")
  
fossil_map_data <- left_join(mn_counties, fossil_emp_map, by = c("fips" = "FIPS"))
fossil_map_data <- fossil_map_data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
  mutate(lq_bin=cut(lq, breaks = c(-1, 1, 5, 20, 100), labels = c("Below Average (0-1)","Above Average (1-5)", "High (5-20)", "Very High (20+)"))) 

county_labels<-centroid_labels(regions = c("counties"))
county_labels <- county_labels %>%
  inner_join(fossil_emp_map,by=c("fips"="FIPS")) 
#%>%  mutate(NAME=ifelse(region_id==1,NAME,""))

library(ggrepel)
fossil_lq_map<-ggplot() +
  geom_sf(data = fossil_map_data, aes(fill = lq_bin, alpha = region_id), color = "darkgrey") +
  #geom_text_repel(data = county_labels, aes(x = x, y = y, label = county), size = 2, color = "black", fontface = "bold") +
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
  left_join(total_emp %>% select(state,county,,EMP,FIPS), by = c("state"="state","county"="county","FIPS"="FIPS"), suffix = c("", "_total")) %>%
  select(FIPS,state,county,NAICS2017,naics_desc,EMP,EMP_total) %>%
  group_by(FIPS,state,county,NAICS2017,naics_desc) %>%
  mutate(emp_share=EMP/EMP_total) %>%
 # filter(!is.na(full.x)) %>%
  arrange(desc(emp_share))

# National-level proportions
national_proportions <- emp_proportions %>%
  ungroup() %>%
  select(NAICS2017, naics_desc, EMP) %>%
  group_by(NAICS2017, naics_desc) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  ungroup()%>%
  mutate(emp_share_national = EMP / EMP[1])

#Location Quotients
location_quotients_county <- emp_proportions %>%
  left_join(national_proportions, by = c("NAICS2017"="NAICS2017","naics_desc"="naics_desc")) %>%
  mutate(LQ = emp_share / emp_share_national,
         weighted_LQ = LQ * emp_share)  # Weighting by regional share

# Compute the Hachman index
hachman_indices_county <- location_quotients_county %>%
  filter(full.x==state_name) %>%
  mutate(region_id=ifelse(FIPS %in% region_counties$fips,1,0)) %>% # Identify the region of interest
  group_by(county,FIPS,region_id) %>%
  summarize(HI = 100 / sum(weighted_LQ, na.rm = TRUE), .groups = 'drop') %>% # Reciprocal of the sum of weighted LQs scaled by 100
  mutate(HI_bin = cut(HI, breaks = c(0, 50,60,70,80,90, 100), labels = c("Very Low (0-50)","Low (50-60)", "Low-Medium (60-70)","Medium-High (70-80)", "High (80-90)", "Very High (90-100)")))


# Plotting the state map with Hachman Index

# Get US county map data
county_map_data <- inner_join(us_counties, hachman_indices_county, by = c("fips" = "FIPS"))
county_map_data <- county_map_data %>%
  mutate(alpha = ifelse(region_id == 1, 1, 0.5))

county_labels<-centroid_labels(regions = c("counties"))
county_labels <- county_labels %>%
  inner_join(hachman_indices_county,by=c("fips"="FIPS"))

hachman_map_county_d<-ggplot() +
  geom_sf(data = county_map_data, aes(fill = HI_bin, alpha = alpha), color = "grey") +
  #geom_text(data = county_labels, aes(x = x, y = y, label = NAME), size = 2, color = "white", fontface = "bold") +
  scale_fill_manual(values = rmi_palette, na.value = "grey90", name = "Hachman Index") +
  scale_alpha_identity() +
  labs(title = paste("Economic Diversity within", state_name), 
       subtitle = "A Hachman Index score ranges from 0 to 100. A higher score
indicates that the subject area's industrial distribution more closely 
       resembles that of the US as a whole, and is therefore diverse.",
       fill = "Hachman Index",
       caption = "Source: Census Bureau, County Business Patterns") +
  theme_void() +
  theme(legend.position = c(0.9, 0.1),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"))


#Quarterly Census of Employment and Wages

#2018:2023 data for industries in Clean Investment Monitory
#Fastest:
cim_qcew <- read.csv('./Data/QCEW.csv')

#Latest (but very slow)
# cim_qcew <- data.frame()
# 
# # Loop for years before 2022 - note use of NAICS2017 codes
# for (year in c('2018', '2019', '2020', '2021')) {
#   
#   # Loop over each quarter
#   for (quarter in c('1', '2', '3', '4')) {
#     
#     # Loop over each industry
#     for (industry_code in tech_mapping$`6-Digit Code`) {
#       
#       # Pull data for the current combination of year, quarter, and industry
#       current_data <- blsQCEW(method = 'Industry',
#                               year = year,
#                               quarter = quarter,
#                               industry = industry_code)
#       
#       # Append the current data to the overall data frame
#       cim_qcew <- rbind(cim_qcew, current_data)
#       
#     }
#   }
# }
# 
# # Loop for years 2022 and first three quarters of 2023 - note use of NAICS2022 codes
# for (year in c('2022', '2023')) {
#   
#   # Determine quarters to loop over based on the year
#   quarters <- if (year == '2023') c('1', '2', '3') else c('1', '2', '3', '4')
#   
#   # Loop over each quarter
#   for (quarter in quarters) {
#     
#     # Loop over each industry for years 2022 and first three quarters of 2023
#     for (industry_code in tech_mapping$`2022 NAICS Code`) {
#       
#       # Pull data for the current combination of year, quarter, and industry
#       current_data <- blsQCEW(method = 'Industry',
#                               year = year,
#                               quarter = quarter,
#                               industry = industry_code)
#       
#       # Append the current data to the overall data frame
#       cim_qcew <- rbind(cim_qcew, current_data)
#       
#     }
#   }
# }



# US Total Employment
cim_qcew_us <- cim_qcew %>%
  filter(area_fips == "US000") %>%
  select(own_code,size_code,year,qtr,qtrly_estabs,month1_emplvl,month2_emplvl,month3_emplvl,industry_code) %>%
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
  group_by(technology) %>%
  mutate(ira_index = 100*emplvl/emplvl[date=="2022-08-01"]) 

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

write.csv(ira_emp_wide,"./Data/ira_emp_wide.csv")


# US Average Wages
cim_qcew_us_wage <- cim_qcew %>%
  filter(area_fips == "US000") %>%
  select(own_code,size_code,year,qtr,qtrly_estabs,industry_code, avg_wkly_wage) %>%
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

write.csv(ira_wage_wide,"./Data/ira_wage_wide.csv")


# US Establishments
cim_qcew_us_estab <- cim_qcew %>%
  filter(area_fips == "US000") %>%
  select(own_code, size_code, year, disclosure_code, qtr, industry_code, qtrly_estabs) %>%
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

write.csv(ira_qtrly_estabs_wide,"./Data/ira_qtrly_estabs_wide.csv")


#state Clean Energy Employment
cim_qcew_state <- cim_qcew %>%
  filter(agglvl_code=="58") %>%  
  select(1,3,6,7,10:13, qtr, industry_code, area_fips) %>%
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
  group_by(area_fips,technology) #%>%
  mutate(ira_index = 100*emplvl/emplvl[date=="2022-07-01"],na.rm=T) # ERROR?


qcew_states_2123 <- cim_qcew_states %>%
  filter(date =="2021-01-01"|date =="2023-09-01") %>%
  pivot_wider(names_from=date,values_from=emplvl) %>%
  mutate(emp_2123=`2023-09-01`-`2021-01-01`,
         emp_2123_perc=emp_2123/`2021-01-01`*100) %>%
  left_join(state_gdp %>% select(GeoFips,GeoName,X2022),by=c("area_fips"="GeoFips"))

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
write.csv(qcew_states_2123_top1,"./Data/qcew_states_top1.csv")

# ERROR? what is state_inv_spec
state_inv_spec <- facilities%>%
  filter(Current_Facility_Status=="O") %>%
  mutate(technology=ifelse(Segment=="Manufacturing",paste0(Technology," ",Segment),Technology)) %>%
  group_by(State.Name,technology) %>%
  summarize_at(vars(Total_Facility_CAPEX_Estimated),sum,na.rm=T) %>%
  #inner_join(feas %>% select(naics_desc,msa_name,feas_percentile_ind_rank),by=c("tech"="naics_desc","msa_name"="msa_name")) %>%
  inner_join(state_gdp,by=c("State.Name"="GeoName"))%>%
  mutate(capex_gdp=Total_Facility_CAPEX_Estimated/gdp_22)

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
  select(area_fips,month3_emplvl) # month3_emplvl is the employment level in the third month of a quarter
all_us_industries <- all_industries %>%
  filter(own_code=="5",area_fips=="US000") %>%
  select(area_fips,month3_emplvl)

US_emp <-state_qcew %>%
  filter(own_code=="5") %>%
  mutate(industry_code=as.numeric(industry_code)) %>%
  inner_join(eti_long,by=c("industry_code"="2022 NAICS Code")) %>%
#  rename(tech="Primary Transition Products / Technologies") %>%
  filter(!is.na(industry_code)) %>%
  distinct(area_fips,Technology,industry_code,`2022 NAICS Title`,month3_emplvl) %>%
  group_by(Technology) %>%
  summarize_at(vars(month3_emplvl),sum,na.rm=T) %>%
  rename(US_ind_emp=month3_emplvl)%>%
  cbind(all_us_industries) %>%
  mutate(emp_share_us=US_ind_emp/month3_emplvl)


state_emp <- state_qcew %>%
  filter(own_code=="5",
         agglvl_code=="58") %>%
  mutate(industry_code=as.numeric(industry_code)) %>%
  inner_join(eti_long,by=c("industry_code"="2022 NAICS Code")) %>%
  #rename(tech="Primary Transition Products / Technologies") %>%
  filter(`Sector` %in% c("Energy End-Use Sector",
                                             "Industrial End-Use Sector",
                                             "Transportation End-Use Sector",
                                             "Transition Chemical, Mineral, and Metal Manufacturing Sector",
                                             "Transition Mineral and Metal Mining Sector")) %>%
  filter(!is.na(industry_code)) %>%
  group_by(area_fips,Technology) %>%
  summarize_at(vars(month3_emplvl),sum,na.rm=T) %>%
  inner_join(state_gdp %>% select(GeoFips,GeoName,X2022) %>% mutate(GeoFips =as.numeric(GeoFips)),by=c("area_fips"="GeoFips")) %>%
  inner_join(all_state_industries %>% mutate(area_fips =as.numeric(area_fips)),by=c("area_fips"="area_fips")) %>%
  mutate(emp_share=month3_emplvl.x/month3_emplvl.y) %>%
  inner_join(US_emp,by=c("Technology"="Technology")) %>%
  mutate(lq=emp_share/emp_share_us) %>%
  arrange(desc(lq)) %>%
  distinct(GeoName,Technology,month3_emplvl.x,emp_share,lq)

state_spec_1 <- state_emp %>%
  group_by(GeoName) %>%
  slice_max(order_by=lq,n=1) %>%
  mutate(tech = ifelse(grepl("Vehicles", Technology, ignore.case = TRUE), 
                       "Electric Vehicles", 
                       Technology))

write.csv(state_spec_1,'./Data/state_spec_1.csv')

# Mapping LQ to EAs and Feasibility
EA_id <- EAs %>%
  filter(`EA Name` == region_name)

# Run feasibility.R first 
EA_feas_naics <- feasibility_naics %>% filter(msa_name == paste0(region_name, " (EA)"))

EA_state_feas_naics <- EA_feas_naics %>% 
  filter(state_avb == state_abbreviation)

#Four Digit Level
cbp21_4d <- cbp_21 %>%
  select(-fips) %>%
  mutate(state=as.numeric(STATE)) %>%
  filter(INDLEVEL=="4")  %>%
  mutate(FIPS=paste0(STATE, COUNTY)) %>%
  left_join(EAs,by=c("FIPS"="FIPS")) %>%
  left_join(naics2017 %>% select(`2017 NAICS US   Code`,`2017 NAICS US Title`),by=c("NAICS2017"="2017 NAICS US   Code")) %>%
  rename(naics_desc=`2017 NAICS US Title`) %>%
  mutate(NAICS2D = substr(NAICS2017, 1, 2))

#Filter just for state of interest
region_cbp_4d <- cbp21_4d %>%
  filter(state==states_simple$fips[states_simple$abbr == state_abbreviation]) %>%
  mutate(region_id=ifelse(fips %in% region_id$fips,1,0)) %>%
  mutate(code=ifelse(NAICS2D %in% c("00","11","21","22","23","31-33","42","48-49","54"),NAICS2017,"Other")) %>%
  group_by(region_id,code) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  #group_by(code) %>%
  mutate(share=EMP/EMP[1]) 

#Total Employment in Region
region_totalemp<-region_cbp_2d %>%
  filter(code=="00",
         region_id==1) 

#Diversity
emp_proportions4 <- cbp21_4d %>%
  left_join(total_emp %>% select(state,county,,EMP,FIPS), by = c("state"="state","county"="county","FIPS"="FIPS"), suffix = c("", "_total")) %>%
  select(FIPS,state,county,NAICS2017,naics_desc,EMP,EMP_total) %>%
  group_by(FIPS,state,county,NAICS2017,naics_desc) %>%
  mutate(emp_share=EMP/EMP_total) %>%
  # filter(!is.na(full.x)) %>%
  arrange(desc(emp_share))

# National-level proportions
national_proportions4 <- emp_proportions4 %>%
  ungroup() %>%
  select(NAICS2017, naics_desc, EMP) %>%
  group_by(NAICS2017, naics_desc) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  ungroup()%>%
  mutate(emp_share_national = EMP / total_emp_nat$EMP)

#Location Quotients
# filter for counties in EA, in state of interest, then avg emp share to EA level
EA_naics_sum <- emp_proportions4 %>%
  left_join(national_proportions4, by = c("NAICS2017"="NAICS2017","naics_desc"="naics_desc")) %>%
  filter(FIPS %in% EAs$fips[EAs$`EA Name` == region_name],
         state == states_simple$fips[states_simple$full == state_name]) %>%
  group_by(NAICS2017, naics_desc) %>%
  summarise(EA_naics_emp = sum(EMP.x),
            avgempshare = mean(emp_share)) 

location_quotients_EA <- EA_naics_sum %>%
  left_join(national_proportions4, by = c("NAICS2017"="NAICS2017","naics_desc"="naics_desc")) %>%
  mutate(LQ = avgempshare / emp_share_national,
         weighted_LQ = LQ * avgempshare)  # Weighting by regional share

topEA_lq <- location_quotients_EA %>%
  ungroup() %>%
  slice_max(order_by = weighted_LQ, n=10)

write.csv(topEA_lq, file=paste0("./DataWrapper/",state_abbreviation,"_topLQ")

# join LQ with feas by naics code
location_quotients_EA$NAICS2017 <- as.numeric(location_quotients_EA$NAICS2017)
EA_lq_feas <- left_join(location_quotients_EA, EA_state_feas_naics, by=c("NAICS2017"="naics", "naics_desc")) %>%
  select(-statefp, -state_name, -state_avb, -region, -msa, -msa_name)

EA_lq_feas_transition <- EA_lq_feas[-which(is.na(EA_lq_feas$transition_sector_category)),]

write.csv(EA_lq_feas_transition, file = paste0(output_folder, "/EA_lq_Feas_transition.csv"))

# top 5 sectors in the msa
county_feas_lq_6d <- read.csv("./Data/county_naics_feasibility_location_quotient_22_6d_datarun202303.csv")

region_feas_lq <- county_feas_lq_6d %>%
  left_join(county_cbsa, by=c("county"="fips")) %>%
  filter(State.Name == state_name)

regionavgfeaslq <- region_feas_lq %>%
  ungroup() %>%
  group_by(naics, naics_desc) %>%
  summarise(rca_avg=mean(rca), density_avg=mean(density))

top10feas <- regionavgfeaslq %>%
  ungroup() %>%
  left_join(naics6d_data, by=c("naics"="naics_6d")) %>%
  filter(!is.na(naics_desc_6d)) %>% # filters for clean energy transition naics
  slice_max(order_by = rca_avg, n=10) %>%
  left_join(transition, by="transition_sector_category_id")

top10lq <- regionavgfeaslq %>%
  ungroup() %>%
  left_join(naics6d_data, by=c("naics"="naics_6d")) %>%
  filter(!is.na(naics_desc_6d)) %>% # filters for clean energy transition naics
  slice_max(order_by = density_avg, n=10) %>%
  left_join(transition, by="transition_sector_category_id")
