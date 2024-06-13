#Fossil Fuels


#Fossil Production - EIA Data\

#Natural Gas
url<-'https://www.eia.gov/dnav/ng/xls/NG_PROD_SUM_A_EPG0_FGW_MMCF_M.xls'
temp_file <- tempfile(fileext = ".xls")
GET(url = url, write_disk(temp_file, overwrite = TRUE))
natgas_prod <- read_excel(temp_file, sheet = 2,skip=2)

nat_gas_prod <- natgas_prod %>%
  pivot_longer(
    cols = 2:ncol(natgas_prod), 
    names_to = "State",
    values_to = "Value"
  ) %>%
  mutate(
    State = gsub(" Natural Gas Gross Withdrawals \\(MMcf\\)", "", State),  # Remove the specific part of the column name
    State = gsub("`", "", State)  # Remove backticks
  ) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(!is.na(Value),   # Remove rows with NA values
         State !="U.S.")

nat_gas_10<-nat_gas_prod %>%
  # Convert the Date column to Date class if it's not already
  # Filter for the latest date
  filter(Date == max(Date)) %>%
  mutate(rank=rank(desc(Value)),
         state=ifelse(rank<7,State,"Other")) %>%
  filter(rank<8) %>%
  arrange(desc(Value))

# Join this ordering back to the main dataset
nat_gas_prod <- nat_gas_prod %>%
  left_join(nat_gas_10 %>% select(State,state),by=c("State")) %>%
  mutate(state=ifelse(is.na(state),"Other",state))%>%
  group_by(Date,state) %>%
  summarize(Value=sum(Value),na.rm=T) %>%
  mutate(state = factor(state, levels = nat_gas_10$state))


#Total Produciton plot
natgas_prod_plot<-ggplot(nat_gas_prod %>%
         filter(state %in% nat_gas_10$state),aes(x=Date,y=Value,fill=state,alpha=ifelse(state==state_name,1,0.6)))+
  geom_area(position='stack')+
  scale_fill_manual(values = expanded_palette)+
  scale_alpha_identity() +  # Ensure alpha is interpreted as given
  labs(title="Natural Gas Production in Largest Producing States",
       x="Year",
       y="Natural Gas Production (MMcf)",
       caption="Source: EIA")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_date(expand = c(0, 0), 
               breaks = scales::date_breaks("5 years"),  # Set breaks every year
               labels = scales::date_format("%Y")) +  # Set labels to display only the year
  theme_classic()+
  theme(legend.position="bottom")

ggsave(file.path(output_folder, paste0(state_abbreviation,"_natgas_prod_plot", ".png")), 
       plot = natgas_prod_plot,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)

#Production in State
# Calculate the maximum date to use in the filter to avoid recalculating it for each row
max_date <- max(nat_gas_prod$Date, na.rm = TRUE)

# State Decade Growth
state_natgas <- nat_gas_prod %>%
  mutate(Year=year(Date)) %>%
  group_by(Year,state) %>%
  summarize_at(vars(Value),sum,na.rm=T) %>%
  filter(Year=="2023"| Year=="2014") %>%
  pivot_wider(names_from = "Year", values_from = "Value", names_prefix = "X_") %>%
  mutate(change_1424 = (`X_2023` - `X_2014`)/`X_2014`*100)



#State Production Index
natgas_index <-nat_gas_prod %>% 
  mutate(Year=year(Date)) %>%
  group_by(Year,state) %>%
  summarize_at(vars(Value),sum,na.rm=T) %>%
  mutate(cum_cap = cumsum(Value)) %>%
  group_by(state) %>%
  mutate(natgas_index_13 = 100*cum_cap/cum_cap[Year=="2013"]) %>%
  mutate(growth_13_23 = round(natgas_index_13-100,1))%>%
  filter(Year != "2024")

plot_natgas_index<-ggplot(data=natgas_index ,
                          aes(x=Year,
                              y=natgas_index_13,
                              group=state,
                              color=state)) +
  geom_line(data = subset(natgas_index%>%filter(state == state_name), state == state_name), size = 2) +  # Plot other lines
  geom_line(data = subset(natgas_index%>%filter(state != state_name), state != state_name), size = 1) +  # Plot other lines
  scale_size_identity() +
  labs(title="Natural Gas Production Growth since 2013",
       subtitle="Cumulative Natural Gas Production, indexed to 2013 levels",
       x="", y="Index (100=2013)",
       color="State")+
  theme_classic()+
  scale_color_manual(values = rmi_palette)

ggsave(file.path(output_folder, paste0(state_abbreviation,"_plot_natgas_index", ".png")), 
       plot = plot_natgas_index,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)


#Oil Production
url<-'https://www.eia.gov/dnav/pet/xls/PET_CRD_CRPDN_ADC_MBBL_M.xls'

temp_file <- tempfile(fileext = ".xls")
GET(url = url, write_disk(temp_file, overwrite = TRUE))
oil_prod <- read_excel(temp_file, sheet = 2,skip=2)

oil_prod <- oil_prod %>%
  pivot_longer(
    cols = 2:ncol(oil_prod), 
    names_to = "State",
    values_to = "Value"
  ) %>%
  mutate(
    State = gsub(" Field Production of Crude Oil \\(Thousand Barrels\\)", "", State),  # Remove the specific part of the column name
    State = gsub("`", "", State)  # Remove backticks
  ) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date>"1990-01-01",   # Remove rows with NA values
         State !="U.S.",
         !grepl("PADD",State))


oil_10<-oil_prod %>%
  # Convert the Date column to Date class if it's not already
  # Filter for the latest date
  filter(Date == max(Date)) %>%
  mutate(rank=rank(desc(Value)),
         state=ifelse(rank<7,State,"Other")) %>%
  filter(rank<8) %>%
  arrange(desc(Value))

# Join this ordering back to the main dataset
oil_prod <- oil_prod %>%
  left_join(oil_10 %>% select(State,state),by=c("State")) %>%
  mutate(state=ifelse(is.na(state),"Other",state))%>%
  group_by(Date,state) %>%
  summarize_at(vars(Value),sum,na.rm=T) %>%
  mutate(state = factor(state, levels = oil_10$state))


oil_prod_plot<-ggplot(oil_prod %>%
                           filter(state %in% oil_10$state),aes(x=Date,y=Value,fill=state,alpha=ifelse(state==state_name,1,0.6)))+
  geom_area(position='stack')+
  scale_fill_manual(values = expanded_palette)+
  scale_alpha_identity() +  # Ensure alpha is interpreted as given
  labs(title="Oil Production in Largest Producing States",
       x="Year",
       y="Oil Production (Thousand Barrels)",
       caption="Source: EIA")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_date(expand = c(0, 0), 
               breaks = scales::date_breaks("5 years"),  # Set breaks every year
               labels = scales::date_format("%Y")) +  # Set labels to display only the year
  theme_classic()+
  theme(legend.position="bottom")

ggsave(file.path(output_folder, paste0(state_abbreviation,"_oil_prod_plot", ".png")), 
       plot = oil_prod_plot,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)


# Filter the data for the specific state and two dates
state_oil <- oil_prod %>%
  mutate(Year=year(Date)) %>%
  group_by(Year,state) %>%
  summarize_at(vars(Value),sum,na.rm=T) %>%
  filter(Year=="2023"| Year=="2014") %>%
  pivot_wider(names_from = "Year", values_from = "Value", names_prefix = "X_") %>%
  mutate(change_1424 = (`X_2023` - `X_2014`)/`X_2014`*100)

#State Production Index
oil_index <-oil_prod %>% 
  mutate(Year=year(Date)) %>%
  group_by(Year,state) %>%
  summarize_at(vars(Value),sum,na.rm=T) %>%
  mutate(cum_cap = cumsum(Value)) %>%
  group_by(state) %>%
  mutate(oil_index_10 = 100*cum_cap/cum_cap[Year=="2010"]) %>%
  mutate(growth_10_23 = round(oil_index_10-100,1))%>%
  filter(Year != "2024")

plot_oil_index<-ggplot(data=oil_index ,
                          aes(x=Year,
                              y=oil_index_10,
                              group=state,
                              color=state)) +
  geom_line(data = subset(oil_index%>%filter(state == state_name), state == state_name), size = 2) +  # Plot other lines
  geom_line(data = subset(oil_index%>%filter(state != state_name), state != state_name), size = 1) +  # Plot other lines
  scale_size_identity() +
  labs(title="Oil Production Growth since 2010",
       subtitle="Cumulative Crude Oil Production, indexed to 2010 levels",
       x="", y="Index (100=2010)",
       color="State")+
  theme_classic()+
  scale_color_manual(values = rmi_palette)

ggsave(file.path(output_folder, paste0(state_abbreviation,"_plot_oil_index", ".png")), 
       plot = plot_oil_index,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)


#Energy Production in a Net Zero Scenario
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

#Tax Revenues
url<'https://www.rff.org/documents/4338/WP_24-01_Dataset.xlsx'

tax_rev <- read_excel('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/rff_taxrev.xlsx', sheet = 2)

state_rev<-tax_rev %>%
  mutate(energy=case_when(
    grepl("Oil",`Energy type (simplified)`) ~ "Oil & Gas",
    grepl("NG",`Energy type (simplified)`) ~ "Oil & Gas",
    grepl("Gas",`Energy type (simplified)`) ~ "Oil & Gas",
    grepl("Coal",`Energy type (simplified)`) ~ "Coal",
    grepl("Electric",`Energy type (simplified)`) ~ "Electricity",
    grepl("Renewable",`Energy type (simplified)`) ~ "Renewable",
    TRUE ~ "Other"
  )) %>%
  group_by(State,`Fiscal Year`,energy) %>%
  summarize_at(vars(`Amount ($2022)`),sum,na.rm=T) %>%
  mutate(amount=round(`Amount ($2022)`/1000000000,3))

plot_taxrev_state<-ggplot(data=state_rev,aes(x=`Fiscal Year`,y=amount,group=energy,fill=energy)) +
  geom_col() +
  facet_wrap(~State, scales="free_y") +  # Adding faceting to create separate plots for each scenario
  scale_fill_manual(values = expanded_palette, name="Energy")+
  labs(title=paste("Energy Tax Revenue in high fossil-producing states"),
       subtitle = " ",
       x="Year", y="Amount (bns $2022)",
       caption="Source: RFF") +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position="bottom")

ggsave(file.path(output_folder, paste0(state_abbreviation,"_plot_taxrev_state", ".png")), 
       plot = plot_taxrev_state,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)



