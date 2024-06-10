#Generic Metro/Region Code

# Generic US State Clean Investment Monitor Script

# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "MT"  # Replace with any US state abbreviation
state_name <- "Montana"  # Replace with the full name of any US state
region_name <- "Great Falls, MT"

#Set the Working Directory to your Username
setwd("C:/Users/LCarey.RMI/")


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

rmi_palette<-c("#0BD0D9",
               "#0989B1",
               "#003A61",
               "#FFCA08",
               "#F8931D",
               "#548538",
               "#7F7F7F")

output_folder <- paste0("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Slide Decks/States/")


#Powerpoint
template_path<-paste0(output_folder,"RMI_template.potx")
ppt_template <- read_pptx(template_path)
generate_ppt_with_chart <- function(chart_name, chart_plot) {
  
  # Define the paths
  output_file <- file.path(output_folder, paste0(state_name, ".pptx"))
  plot_path <- file.path(output_folder, paste0(state_abbreviation,"_",chart_name, ".png"))
  
  # Create and save the plot
  ggsave(plot_path, plot = chart_plot, width = 10, height = 6, dpi = 300)
  
  # Add a new slide at the specified position
  ppt <- add_slide(ppt, layout = "Title and Content White")
  
  # Add new title to the slide
  ppt <- ph_with(ppt, value = paste0(chart_name), location = ph_location_type(type = "title",top=0.5))
  
  #Subtitle
  subtitle_text <- subtitle
  subtitle_format <- fp_text(font.size = 20, color = "#0BD0D9")
  subtitle <- fpar(ftext(subtitle_text, prop = subtitle_format))
  
  ppt <- ph_with(ppt, value = subtitle, location =ph_location(left = 0.5, top = 1, width = 10, height = 1.0))
  
  # Define the location and size of the image to fill the slide body
  slide_width <- 9.21
  slide_height <- 5.11  # Standard 16:9 slide dimensions in inches
  
  ppt <- ph_with(ppt, external_img(plot_path), location = ph_location(left = 2.75, top = 2.25, width = slide_width, height = slide_height))
  
  # Save the new PowerPoint file to the output folder
  print(ppt, target = output_file)
}


#ChatGPT
API_KEY="sk-proj-7KGMLr9DNblQoOGtowZJT3BlbkFJfZ7i5W2fXBGCBFCCbgeC"

chatgpt_request<- function(prompt){
  response <- POST(
    "https://api.openai.com/v1/completions",
    add_headers(
      `Authorization` = paste("Bearer", API_KEY) # Corrected: added the missing parenthesis
    ),
    body = list(
      model = "gpt-3.5-turbo-instruct", # Corrected: removed the trailing comma
      prompt = prompt,
      temperature = 0.7,
      max_tokens = 100,
      n=1
    ),
    encode = "json"
  )
  result <- content(response, "text")
  
  # Parse the JSON response
  parsed_result <- jsonlite::fromJSON(result)
  
  # Extract the generated text from the first choice
  generated_text <- parsed_result$choices[[1]]
  return(generated_text[[1]])
}


# Clean investment Monitor Data - Check it's the latest quarter available
investment_data_path <- 'OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/States Data/clean_investment_monitor_q4_23/quarterly_actual_investment.csv'
facilities_data_path <- 'OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/States Data/clean_investment_monitor_q4_23/manufacturing_energy_and_industry_facility_metadata.csv'
socioeconomics_data_path <- 'OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/States Data/clean_investment_monitor_q4_23/socioeconomics.csv'

# Read Data
investment <- read.csv(investment_data_path, skip=5)
facilities <- read.csv(facilities_data_path, skip=5)
socioecon <- read.csv(socioeconomics_data_path, skip=5)

#Tech Mapping

tech_mapping <- data.frame(
  Segment = c("Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Energy and Industry", "Energy and Industry", "Energy and Industry", "Energy and Industry", "Energy and Industry", "Energy and Industry"),
  Technology = c("Batteries", "Solar", "Critical Minerals", "Fueling Equipment", "Zero Emission Vehicles", "Electrolyzers", "Storage", "Wind", "Hydrogen", "SAF", "Storage", "Nuclear", "Solar", "Wind"),
  tech = c("Batteries & Components", "Solar Energy Components", "Low-Carbon Minerals", "Low-Carbon Industrial Equipment", "Electric Vehicles", "Low-Carbon Industrial Equipment", "Batteries & Components", "Wind Energy Components", "Green Hydrogen", "Biofuels", "Energy Utility Systems", "Nuclear Electric Power", "Solar Electric Power", "Wind Electric Power")
)

eti_long<-read_excel("C:/Users/LCarey.RMI/OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/Transition_Industries_FINAL.xlsx",2)

tech_mapping<-inner_join(tech_mapping,eti_long %>% 
                           select("Primary Transition Products / Technologies",
                                  "6-Digit Code",
                                  "6-Digit Description"),
                         by=c("tech"="Primary Transition Products / Technologies"))

#NAICS Codes
file_url <- 'https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx'
temp_file <- tempfile(fileext = ".xlsx")
GET(url = file_url, write_disk(temp_file, overwrite = TRUE))
naics2017 <- read_excel(temp_file, sheet = 1)  # 'sheet = 1' to read the first sheet

naics2022<-read_excel("C:/Users/LCarey.RMI/Downloads/2022_to_2017_NAICS.xlsx",1,skip=2)
tech_mapping<-left_join(tech_mapping,naics2022 %>% select("2022 NAICS Code",
                                                          "2022 NAICS Title",
                                                          "2017 NAICS Code"),
                        by=c("6-Digit Code"="2017 NAICS Code"))


#Electricity


#County Plant-Level Generation

#Retail Service Territories
shapefile <- st_read("C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/Electric_Retail_Service_Territories.shp")

sbrgn_sf_transformed <- st_transform(shapefile, crs = st_crs(counties))
sbrgn_sf_transformed <- st_make_valid(sbrgn_sf_transformed)
# Disable s2 options temporarily
sf::sf_use_s2(FALSE)
# Perform the spatial join
matched <- st_join(sbrgn_sf_transformed, counties, join = st_intersects)
# Re-enable s2 options after the operation
sf::sf_use_s2(TRUE)
matched<-as.data.frame(matched)
matched <- matched %>%
  select(ID,NAME.x,STATE,CNTRL_AREA,HOLDING_CO,NET_GEN,CUSTOMERS,STATEFP,COUNTYFP,GEOID) %>%
  mutate(id=as.numeric(ID))

utilities <- matched %>%
  mutate(fips=as.numeric(GEOID)) %>%
  left_join(county_cbsa %>% select(CBSA.Title,fips),by=c("fips"="fips")) %>%
  left_join(EAs %>% select(`EA Name`,fips),by=c("fips"="fips"))

region_utility <- utilities %>%
  filter(GEOID %in% region_id$FIPS) 

#EPA eGRID Data
file_url <- 'https://www.epa.gov/system/files/documents/2024-01/egrid2022_data.xlsx'
temp_file <- tempfile(fileext = ".xlsx")
GET(url = file_url, write_disk(temp_file, overwrite = TRUE))
epa_plnt <- read_excel(temp_file, sheet = 4,skip=1)

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

write.csv(EA_renshare,"C:/Users/LCarey.RMI/Downloads/EA_renshare.csv")
multi_region_id <- EAs %>% filter(`EA Name`==region_name)

ggplot(data=EA_renshare)+
  geom_point(aes(x=em_rate,y=noncomb_mix,size=total_gen))+
  scale_x_log10() + # Log scale for x-axis
  scale_y_log10() + # Log scale for y-axis
  theme_minimal()  # Minimal theme for cleaner look

#Regional EA Emissions Map
plot_data<- EA_renshare %>%
  filter(region %in% multi_region_id$region) %>%
  mutate(region_id=as.factor(ifelse(`EA Name` %in% multi_region_id$`EA Name`,1,0))) %>%
  slice_min(em_rate,n=20)

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


#Map
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
county_elec_cons <- read.csv("C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/energy_consumption_expenditure_business_as_usual_county.csv")

#Industrial Electricity Expenditure & Consumption out to 2050
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
seds_all <- read.csv('https://www.eia.gov/state/seds/sep_update/Complete_SEDS_update.csv')

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


#Renewable Production
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

#County Business Patterns
cbp21_2d <- cbp_2021 %>%
  mutate(state=as.numeric(state)) %>%
  left_join(states_simple,by=c("state"="fips")) %>%
  filter(INDLEVEL=="2")  %>%
  left_join(counties %>% 
              select(GEOID,STATEFP,COUNTYFP,NAME,NAMELSAD),by=c("STATE"="STATEFP","COUNTY"="COUNTYFP")) %>%
  left_join(naics2017 %>% select(`2017 NAICS US   Code`,`2017 NAICS US Title`),by=c("NAICS2017"="2017 NAICS US   Code")) %>%
  rename(naics_desc=`2017 NAICS US Title`)

#Calculate proportions
total_emp <- cbp21_2d %>%
  group_by(GEOID,state,county,full,NAME,NAMELSAD) %>%
  summarize_at(vars(EMP),sum,na.rm=T) 
total_emp_nat<-cbp21_2d  %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  ungroup() 

#Fossil FUel Employment
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
  group_by(FIPS,full,fossil) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  left_join(total_emp %>% select(GEOID,full,NAME,EMP), by = c("full"="full","FIPS"="GEOID"), suffix = c("", "_total")) %>%
  select(FIPS,state,county,full,NAME,EMP,EMP_total,fossil) %>%
  ungroup() %>%
  mutate(emp_share = EMP / EMP_total) %>%
  left_join(fossil_emp_national, by = c("fossil" = "fossil")) %>%
  mutate(lq=emp_share/emp_share_national)

fossil_emp_map <- fossil_emp_county %>%
  filter(full==state_name) %>%
  select(full,FIPS,NAME,fossil,lq) %>%
  pivot_wider(names_from=fossil,values_from=lq) %>%
  mutate(region_id=ifelse(FIPS %in% region_counties$fips,1,0.7),
         lq = ifelse(is.na(`1`),0,`1`)) %>%
    select(full,FIPS,NAME,region_id,lq) 

#Fossil Fuel Employment-Map
fossil_map_data <- inner_join(us_counties, fossil_emp_map, by = c("fips" = "FIPS"))

county_labels<-centroid_labels(regions = c("counties"))
county_labels <- county_labels %>%
  inner_join(fossil_emp_map,by=c("fips"="FIPS")) %>%
  mutate(NAME=ifelse(region_id==1,NAME,""))

fossil_lq_map<-ggplot() +
  geom_polygon(data = fossil_map_data, aes(x = x, y = y, group = group, fill = lq, alpha = region_id), color = "#0BD0D9") +
  geom_text(data = county_labels, aes(x = x, y = y, label = NAME), size = 2, color = "darkgrey", fontface = "bold") +
  scale_fill_gradient(low="white",high="#003A61", na.value = "grey90", name = "Fossil Fuel Location Quotient") +
  scale_alpha_identity() +
  labs(title = paste("Fossil Fuel Specialization in ", state_name), 
       subtitle = "A location quotient indicates specialization in local employment. A score greater than one indicates that the subject area's employment in fossil fuel industires is greater than the national average.",
       fill = "Location Quotient",
       caption = "Source: Census Bureau, County Business Patterns") +
  theme_void() +
  theme(legend.position = c(0.9, 0.1),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"))

ggsave(file.path(output_folder, paste0(state_abbreviation,"_fossil_lq_map", ".png")), 
       plot = fossil_lq_map,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)


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

ggsave(file.path(output_folder, paste0(state_abbreviation,"_hachman_map_county_categories", ".png")), 
       plot = hachman_map_county_d,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)




#Feasibility
#Load Latest Clean Growth Tool Data
cgt<-readRDS('C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/Raw Data/acre_tool_final_data_042624')

msa_data<-cgt$msa_data
naics_msa_data<-cgt$naics_msa_data
naics_data<-cgt$naics_data
states_msa<-cgt$states_msa
naics6d_data<-cgt$naics6d_data
transition<-cgt$transition_sector_data

#Join dataframes into one large dataset and create industry-specific feasibility ranks
feas<-naics_msa_data %>%
  left_join(msa_data,by=c("msa"="msa")) %>%
  left_join(naics_data %>% 
              select(naics,naics_desc,aggregation_level),
            by=c("naics"="naics","aggregation_level"="aggregation_level")) %>%
  group_by(region,aggregation_level,naics,naics_desc) %>%
  mutate(feas_percentile_ind_rank = percent_rank(-density),
         feas_rank_by_industry=rank(-density))

region_feas <- feas %>%
  filter(msa_name==paste(region_name,"(EA)"))

#Top 10 Feasible Industries
top25_feas<-region_feas %>%
  ungroup() %>%
  filter(aggregation_level==2) %>%
  select(msa_name,naics_desc,density,feas_rank_by_industry,jobs.x,pci,rca,share_good_jobs,percent_change_jobs_l5.x) %>%
  slice_max(density,n=25)

feas_quadrants <- region_feas %>%
  ungroup() %>%
  filter(aggregation_level==2) %>%
  select(msa_name,naics_desc,density,jobs.x,pci,rca)
write.csv(feas_quadrants,paste0(output_folder,"feas_quadrants.csv"))
