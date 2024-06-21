
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


