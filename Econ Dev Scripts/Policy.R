
#POLICY!

#Federal investments
url <- 'https://www.whitehouse.gov/wp-content/uploads/2023/11/Invest.gov_PublicInvestments_Map_Data_CURRENT.xlsx'
temp_file <- tempfile(fileext = ".xlsx")
GET(url = url, write_disk(temp_file, overwrite = TRUE))
fed_inv <- read_excel(temp_file, sheet = 4)  # 'sheet = 1' to read the first sheet

#Total expenditure by program
program_spend <- fed_inv %>%
  filter(Category=="Clean Energy, Buildings, and Manufacturing") %>%
  mutate(`Funding Amount` = as.numeric(`Funding Amount`)) %>%
  group_by(`Program Name`) %>%
  summarize_at(vars(`Funding Amount`),sum,na.rm=T) %>%
  ungroup() %>%
  mutate(share=round(`Funding Amount`/sum(`Funding Amount`)*100,3)) %>%
  arrange(desc(`Funding Amount`)) 

#State of Interest investment by program
state_spend <- fed_inv %>%
  mutate(`Funding Amount` = as.numeric(`Funding Amount`)) %>%
  filter(Category=="Clean Energy, Buildings, and Manufacturing") %>%
  filter(State==state_name) %>%
  group_by(Subcategory,`Funding Source`,`Program Name`) %>%
  summarize_at(vars(`Funding Amount`),sum,na.rm=T) %>%
  ungroup() %>%
  mutate(share=round(`Funding Amount`/sum(`Funding Amount`)*100,3)) %>%
  arrange(desc(`Funding Amount`)) %>%
  inner_join(program_spend,by="Program Name") %>%
  mutate(state_fed_lq=share.x/share.y,
         fund_m=`Funding Amount.x`/1000000) %>%
  select(`Funding Source`,`Program Name`,Subcategory,fund_m,state_fed_lq) 
write.csv(state_spend,paste0(output_folder,"/",state_abbreviation,"_state_spend.csv"),row.names=F)

state_fedspend_plot<-ggplot(data=state_spend %>% filter(state_fed_lq>2),aes(x=reorder(`Program Name`,state_fed_lq),y=state_fed_lq,fill=Subcategory))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=rmi_palette)+
  labs(title=paste0("Federal Investment in Clean Energy, Buildings, and Manufacturing in ",state_name),
       substitle="Federal programs where investment in New Mexico is more than double the national average share",
       x="Program",
       y="Funding relative to national average)",
       fill="Subcategory")+
  theme_classic()+
  theme(legend.position="bottom",
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +  # Adjust margins as needed
  scale_y_continuous(expand = c(0, 0))+
  theme(plot.title = element_text(hjust = 1, vjust = 0.5),  # Center the plot title
        legend.position = c(0.7,0.2))

ggsave(paste0(output_folder,"/",state_abbreviation,"_state_fedspend_plot.png"),plot=state_fedspend_plot,width=8,height=6,units="in",dpi=300)

state_spend_cleantot<-state_spend %>%
  summarize(total=sum(fund_m)) 

state_spend_cap<-fed_inv %>%
  mutate(`Funding Amount` = as.numeric(`Funding Amount`)) %>%
  filter(Category=="Clean Energy, Buildings, and Manufacturing") %>%
  group_by(State) %>%
  summarize_at(vars(`Funding Amount`),sum,na.rm=T) %>%
  ungroup() %>%
  left_join(socioecon %>%
              filter(quarter=="2024-Q1") %>%
              select(StateName,population),by=c("State"="StateName")) %>%
  filter(State != "Multiple") %>%
  mutate(cap_per_capita=`Funding Amount`/population,
         state_share=`Funding Amount`/sum(`Funding Amount`)) %>%
  arrange(desc(cap_per_capita))

#Map
fed_inv_map <- read_excel(temp_file, sheet = 3)  # 'sheet = 1' to read the first sheet
state_fedinv_map <- fed_inv_map %>%
  filter(State==state_name,
         Category=="Clean Energy, Buildings, and Manufacturing") 
write.csv(state_fedinv_map,paste0(output_folder,"/",state_abbreviation,"_state_fedinv_map.csv"),row.names=F)


#Estimated Federal Tax Credits (from Clean Investment Monitor)


#Federal Tax Credit Incentives State-Level Estimates
tax_inv_cat<-read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/clean_investment_monitor_q1_24/tax_investment_by_category.csv',skip=2)
tax_inv_state<-read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/clean_investment_monitor_q1_24/tax_investment_by_state.csv',skip=2)

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
write.csv(state_estimates2,"OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/IRA_taxcredits_estimate.csv")

#Charts
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


#Regional State Comparisons
division_of_interest<-census_divisions %>%
  filter(State.Code==state_abbreviation))
state_ira <- state_estimates2 %>%
  left_join(census_divisions,by=c("State"="State.Code")) %>%
  
  

  
  
  
  
  
  
#RMI Economic Tides Analysis - June 18 Data
  # Load necessary library
library(readxl)

# List of state abbreviations
state_abbreviations <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# Initialize an empty list to store data frames
data_frames <- list()

# Loop through each state abbreviation
for (state_abbr in state_abbreviations) {
  # Construct the file path
  file_path <- paste0('OneDrive - RMI/Documents - US Program/6_Projects/Sprint24/Analysis/IRA Downscaling/IRA Funding to states_ econ tides 2.0/June 18 data/ira_econ_tides/', state_abbr, '/Allstates_IRA/Allstates_IRA.xlsx')
  
  # Read the Excel file (assuming the data is on sheet 9)
  data <- read_excel(file_path, sheet = 9)
  
  # Add a new column with the state abbreviation
  data$State <- state_abbr
  
  # Append the data frame to the list
  data_frames[[state_abbr]] <- data
}

# Combine all data frames into one
ira_allstates <- do.call(rbind, data_frames)

#clean Superfund
ira_allstates <- ira_allstates %>%
  select(State,Sector,Section,Provision,`CBO National Estimate ($)`,`CBO Downscaled State Estimate ($)`,`Climate-Aligned Estimate ($)`) 

superfund_clean<-ira_allstates %>%
  filter(Provision=="Superfund") %>%
  mutate(state_share=`CBO Downscaled State Estimate ($)`/`CBO National Estimate ($)`,
         national2=as.numeric("12411000000"),
         superfund=national2*state_share) %>%
    select(State,superfund,national2)

ira_allstates <- ira_allstates %>%
  left_join(superfund_clean, by = "State") %>%
  mutate(
    `CBO National Estimate ($)` = if_else(Provision == "Superfund", national2, `CBO National Estimate ($)`),
    `CBO Downscaled State Estimate ($)` = if_else(Provision == "Superfund", superfund, `CBO Downscaled State Estimate ($)`),
    `Climate-Aligned Estimate ($)` = if_else(Provision == "Superfund", superfund, `Climate-Aligned Estimate ($)`)
  ) %>%
  select(-superfund,-national2)  # Remove the temporary superfund column after update

#Clean 45X
ira_allstates <- ira_allstates %>%
  mutate(`Climate-Aligned Estimate ($)`=if_else(Provision=="Advanced Manufacturing Production Credit",`Climate-Aligned Estimate ($)`*0.6,`Climate-Aligned Estimate ($)`))
ira_allstates <-ira_allstates %>%
  mutate(`Climate-Aligned Estimate ($)`=if_else(Provision=="Advanced Manufacturing Production Credit" & State=="NM",`Climate-Aligned Estimate ($)`/0.6/1.3*1.2,`Climate-Aligned Estimate ($)`))
#Clean Energy Efficient Home Credit
ira_allstates <- ira_allstates %>%
  mutate(`Climate-Aligned Estimate ($)`=if_else(`Climate-Aligned Estimate ($)`<0,`CBO Downscaled State Estimate ($)`,`Climate-Aligned Estimate ($)`))

#Totals relative to population/gdp
sum_ira_allstates<-ira_allstates %>%
  group_by(State) %>%
  summarize_at(vars(`CBO National Estimate ($)`,`CBO Downscaled State Estimate ($)`,`Climate-Aligned Estimate ($)`),sum,na.rm=T) %>%
  ungroup() %>%
  left_join(socioecon %>% filter(quarter=="2024-Q1") %>% select(State,StateName,population),by=c("State"="State")) %>%
  left_join(state_gdp,by=c("StateName"="GeoName")) %>%
  mutate(
    cbo_cap = `CBO Downscaled State Estimate ($)` / population,
    climate_cap = `Climate-Aligned Estimate ($)` / population
  ) %>%
  mutate(
    cbp_gdp = `CBO Downscaled State Estimate ($)` /(X2022*1000000),
    climate_gdp= `Climate-Aligned Estimate ($)` /(X2022*1000000)
  ) %>%
  mutate(across(c(`CBO Downscaled State Estimate ($)`,`Climate-Aligned Estimate ($)`),~round(./1000000000,3))) %>%
  
  select(State,`CBO Downscaled State Estimate ($)`,`Climate-Aligned Estimate ($)`,cbo_cap,climate_cap,cbp_gdp,climate_gdp,population,X2022)


#State of Interest largest IRA Provisions
state_abbr_ira <- ira_allstates %>%
  filter(State == state_abbreviation) %>%
  arrange(desc(`Climate-Aligned Estimate ($)`)) %>%
  mutate(across(where(is.numeric), ~round(./1000000000, 3))) %>%
  mutate(share = `Climate-Aligned Estimate ($)` / sum(`Climate-Aligned Estimate ($)`))


state_10ira_plot <- ggplot(data=state_abbr_ira %>% slice_max(order_by=`Climate-Aligned Estimate ($)`,n=10)) +
  geom_col(aes(x=reorder(Provision,`Climate-Aligned Estimate ($)`),y=`Climate-Aligned Estimate ($)`,fill=Sector),position="stack") +
  coord_flip() +
  scale_fill_manual(values=rmi_palette) +
  labs(title = "Top 10 IRA Provisions in New Mexico in a Climate-Aligned Scenario", 
       subtitle = "",
       x="Provision",
       y="Climate-Aligned Estimate ($b)",
       fill = "Sector",
       caption="Source: RMI, June 2024 Update")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(legend.position=c(0.8,0.2)) 

ggsave(paste0(output_folder,"/",state_abbreviation,"_ira_provisions.png"),plot=state_10ira_plot,width=8,height=6,units="in",dpi=300)


#State Climate and Clean Energy Policy

xchange <- read.csv("C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/US Maps etc/Policy/xchange.csv")
xchange_pol_index <- read.csv("C:/Users/LCarey.RMI/OneDrive - RMI/Documents/Data/US Maps etc/Policy/xchange_climate_policy_index.csv")

xchange_label<-xchange_pol_index %>%
  filter(region %in% region_abbrv$region) %>%
  mutate(label=paste(State," \n(Climate Policy Score=",round(value*100,1),")")) %>%
  ungroup() %>%
  rename(total=value) %>%
  select(State,total,label)

state_totals <- xchange %>%
  filter(region %in% region_abbrv$region) %>%
  group_by(State) %>%
  summarize(total = sum(value)) %>%
  left_join(xchange_label %>% select(State,label), by = "State")

policy_radial<-ggplot(xchange %>% filter(region %in% region_abbrv,
                                         Topic != "total_climate") %>%
                        left_join(xchange_label,by=c("State"="State")), 
                      aes(x = State, y = value, fill = Topic)) +
  geom_bar(stat = "identity", position = "stack", width = 1,
           aes(alpha = ifelse(State != state_name, 0.6, 1))) +
  coord_polar(start = 0) +
  scale_fill_manual(values = rmi_palette) +  # Use custom color palette
  scale_alpha_identity() +  # Ensure alpha is interpreted as given
  geom_text(data = state_totals, aes(x = State, y = total+3, label = label), 
            vjust = -0.5, hjust = 0.5, size = 3, color = "black",inherit.aes=F) +
  labs(title = paste("Climate Policy Strength in the ", str_to_sentence(region_abbrv$region)),
       subtitle = "The Total climate policy score is the sum of normalized indices for five policy areas.",
       caption = "Source: Xchange Climate Policy Tracker",
       x = NULL, 
       y = NULL,
       fill = "Policy Area") +
  #facet_wrap(~ Topic) +  # Facet by State
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14))

ggsave(file.path(output_folder, paste0(state_abbreviation,"_policy_radial", ".png")),
       plot = policy_radial,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)

#Economic Development Incentives

#Good Jobs First Data
gjf<- read.csv("C:/Users/LCarey.RMI/RMI/US Program - Regional Investment Strategies/Great Lakes Investment Strategy/Great Lakes Overview/Econ Development/gjf_complete.csv")

#State Totals by Awarding Agency
gjf_stateagency <- gjf %>%
  filter(Location== state_name,
         Year >2019) %>%
  group_by(Location, Awarding.Agency) %>%
  summarize_at(vars(subs_m,investment_m), sum, na.rm = TRUE) %>%
  arrange(desc(subs_m))

#State Totals by Program Name
gjf_stateprogram_1923<-gjf %>%
  filter(Location== state_name,
         Year >2019) %>%
  group_by(Location,Program.Name) %>%
  summarize_at(vars(subs_m,investment_m),sum,na.rm=T) %>%
  ungroup()  %>%
  arrange(desc(subs_m))

#Project Subsidies >2% of investment value
gjf_meaningful_1923 <- gjf %>%
  filter(investment_m != 0,
         Location== "Ohio",
         subs_m/investment_m>0.01,
         Year==2022) %>%
  mutate(subs_share=round(subs_m/investment_m*100,1)) %>%
  select(abbr,Location,Year,subs_share,subs_m,Investment.Data,investment_m,Company,Project.Description,Major.Industry.of.Parent, Sector,Awarding.Agency,Program.Name,Type.of.Subsidy,Notes) %>%
  arrange(desc(subs_share))


#State Taxes
library(rvest)
library(purrr)
# URL of the page
url <- "https://www.ncsl.org/fiscal/state-tax-actions-database"

# Read the HTML content of the page
webpage <- read_html(url)

# Extract tables from the webpage
tables <- html_table(webpage, fill = TRUE)

# Assuming `tables` is your list of data frames
years <- 2023:2015  # Create a vector of years from 2023 to 2015

# Add a 'year' column to each table and rbind them
combined_data <- purrr::map2_df(tables, years, ~ mutate(.x, year = .y))

combined_data <- combined_data %>%
  mutate(State=ifelse(is.na(State),Jurisdiction,State))%>%
  mutate(State=ifelse(is.na(State),Juridiction,State))%>%
  mutate(`Revenue Type`=ifelse(is.na(`Revenue Type`),Type,`Revenue Type`)) %>%
  mutate(total=ifelse(is.na(`FY 2024 (millions)`),`FY 2023 (millions)`,
                      ifelse(is.na(`FY 2023 (millions)`),`Fiscal Year 2022`,
                             ifelse(is.na(`Fiscal Year 2022`),`Fiscal Year 2021`,`FY 2024 (millions)`)))) %>%
  mutate(total=as.numeric(gsub("[^0-9.]", "", total))) 

#Climate Taxes

keywords<- c("carbon","climate","emission",
             "greenhouse","renewable","solar","wind",
             "energy","fuel","gas","electric","vehicle",
             "EV","transportation","manufacturing","job","jobs","oil")
pattern <- paste0("\\b(", paste(keywords, collapse = "|"), ")\\b")

climate_taxes <- combined_data %>%
  filter(grepl(pattern, Description, ignore.case = TRUE)) %>%
  filter(!Description %in% c("Revenue Total=",
                             "Tax Revenue=",
                             "Non-Tax Revenue=",
                             "Non-Tax Revenue =")) 

write.csv(climate_taxes ,"C:/Users/LCarey.RMI/Downloads/climate_taxes.csv")

nm_clim_taxchanges<-climate_taxes %>%
  filter(State=="New Mexico")

sc_clim_taxchanges<-climate_taxes %>%
  filter(State=="South Carolina")



#Climate/Clean ENergy/Manufacturing Incentive Policies

dev_pol <- read.csv("C:/Users/LCarey.RMI/OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/dbo_Program.csv")

climate_dev_pol <- dev_pol %>%
  filter(grepl(pattern, ProgramDescription, ignore.case = TRUE))

climate_dev_pol_sum<-climate_dev_pol %>%
  filter(Program_Status=="Active")%>%
  group_by(State) %>%
  summarize_at(vars(Program_Name),n_distinct) 
ggplot(data=climate_dev_pol_sum, aes(x=reorder(State,Program_Name),y=Program_Name)) +
  geom_col() +
  coord_flip() +
  labs(title = "Climate/Clean Energy/Manufacturing Incentive Policies",
       x = "Program Description",
       y = "Count") +
  theme_classic()


state_climate_pol <- climate_dev_pol %>%
  filter(State==state_name)
