#Feasibility & Clean Growth Tool

# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "NM"  # Replace with any US state abbreviation
state_name <- "New Mexico"  # Replace with the full name of any US state

#Set the Working Directory to your Username
setwd("C:/Users/LCarey.RMI/")

#Load Latest Clean Growth Tool Data
cgt<-readRDS('OneDrive - RMI/Documents/Data/Raw Data/acre_tool_final_data_042624')

msa_data<-cgt$msa_data
naics_msa_data<-cgt$naics_msa_data
naics_data<-cgt$naics_data
states_msa<-cgt$states_msa
naics6d_data<-cgt$naics6d_data
transition<-cgt$transition_sector_data

#Combine and Clean Data
feasibility<-naics_data %>%
  select(transition_sector_category_id,naics,naics_desc) %>%
  right_join(transition,by=c("transition_sector_category_id"="transition_sector_category_id")) %>%
  select(transition_sector_category,naics,naics_desc) %>%
  inner_join(naics_msa_data,by=c("naics"="naics")) %>%
  filter(aggregation_level=="2") %>%
  left_join(states_msa,by=c("msa"="cbsa")) %>%
  left_join(msa_data %>%
              select(region,msa,msa_name),by=c("msa"="msa")) %>%
  select(statefp,state_name,state_avb,region,msa,msa_name,transition_sector_category, naics,naics_desc,density,pci,rca,jobs,jobs_l5,share_good_jobs,percent_change_jobs_l5) %>%
  group_by(naics,naics_desc) %>%
  mutate(feas_industry_percentile=percent_rank(density)) %>%
  distinct()

#State Average
ea_pop <- county_pop %>%
  select(STATE,COUNTY,POPESTIMATE2022) %>%
  mutate(FIPS=paste0(sprintf("%02d", STATE), sprintf("%03d", COUNTY))) %>%
  left_join(EAs,by=c("FIPS"="FIPS")) %>%
  filter(!is.na(County)) %>%
  select(fips,County,`EA Name`,POPESTIMATE2022) %>%
  group_by(`EA Name`) %>%
  summarize_at(vars(POPESTIMATE2022),sum,na.rm=T) %>%
  arrange(desc(POPESTIMATE2022))

state_totals <- feasibility %>%
  group_by(state_avb,naics_desc) %>%
  summarize(across(c(jobs),sum,na.rm=T)) 

state_feas <- feasibility %>%
  mutate(EA_Name=gsub(" \\(EA\\)","",msa_name)) %>%
  left_join(ea_pop,by=c("EA_Name"="EA Name")) %>%
  filter(region=="EA") %>%
  group_by(state_avb,transition_sector_category,naics_desc) %>%
  summarize(across(c(density,pci,share_good_jobs),
                   ~weighted.mean(.x,w=POPESTIMATE2022,na.rm=T))) %>%
  group_by(naics_desc) %>%
  mutate(feas_industry_percentile=percent_rank(density)) %>%
  left_join(state_totals,by=c("state_avb"="state_avb","naics_desc"="naics_desc") )

state_feas_plot<-ggplot(data=state_feas %>% filter(state_avb==state_abbreviation,
                                                   !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                      "Transition Enabling Sector",
                                                                                      "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
       aes(x=feas_industry_percentile,y=pci,color=transition_sector_category,size=jobs))+
  geom_vline(xintercept = mean(state_feas$feas_industry_percentile),color='darkgrey') +
  geom_hline(yintercept= mean(state_feas$pci) ,color='darkgrey') +
  geom_point()+
  scale_color_manual(values=expanded_palette)+
  geom_text_repel(aes(label=ifelse(feas_industry_percentile>mean(state_feas$feas_industry_percentile) &
                                     pci > mean(state_feas$pci),naics_desc,"")),size=2, max.overlaps = 10)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",state_name),
       subtitle=paste0("High Complexity Clean Energy Industries for which ",state_name," has above-average transition feasibility.", "\n", "Size of bubble represents number of jobs in the sector"),
       x="Feasibility",
       y="Complexity",
       color="Sector",
       caption="Source: RMI, Clean Growth Tool")+
  xlim(0.4,1)+
  ylim(0,7)+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 4),   # Decrease text size
        legend.title = element_text(size = 5),  # Decrease title size
        legend.key.size = unit(0.3, "cm"),      # Decrease key size
        legend.spacing = unit(0.2, "cm")) +
  guides(size = "none")

ggsave(paste0(output_folder,"/",state_abbreviation,"_feasibility.png"),plot=state_feas_plot,width=8,height=6,units="in",dpi=300)

state_feas_plot2<-ggplot(data=state_feas%>% filter(state_avb==state_abbreviation,
                                 !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                    "Transition Enabling Sector",
                                                                    "Transition Forestry, Land, and Agriculture (FLAG) Sector"),
                                 feas_industry_percentile>mean(state_feas$feas_industry_percentile)),
       aes(x=reorder(naics_desc,feas_industry_percentile),y=feas_industry_percentile,fill=transition_sector_category))+
  scale_y_continuous(expand=c(0,0))+
  geom_col()+
  coord_flip() +
  scale_fill_manual(values=expanded_palette)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",state_name),
       subtitle=paste0("High Feasibility Clean Energy Industries for ",state_name),
       x="Feasibility",
       y="Industry",
       fill="Sector",
       caption="Source: RMI, Clean Growth Tool")+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 4),   # Decrease text size
        legend.title = element_text(size = 5),  # Decrease title size
        legend.key.size = unit(0.3, "cm"),      # Decrease key size
        legend.spacing = unit(0.2, "cm"))

ggsave(paste0(output_folder,"/",state_abbreviation,"_feasibility_industry.png"),plot=state_feas_plot2,width=8,height=6,units="in",dpi=300)

#EA Feasibility
EA_table_5<-feasibility %>%
  filter(region=="EA",
         transition_sector_category %in% c("Energy End-Use Sector" ,
                                           "Transition Mineral and Metal Mining Sector",
                                           "Industrial End-Use Sector",
                                           "Transition Chemical, Mineral, and Metal Manufacturing Sector",
                                           "Buildings End-Use Sector",
                                           "Transportation End-Use Sector")) %>%
  mutate(EA_Name=gsub(" \\(EA\\)","",msa_name)) %>%
  group_by(state_avb,EA_Name) %>%
  slice_max(feas_industry_percentile,n=5) %>%
  mutate(rank=rank(-feas_industry_percentile)) %>%
  select(EA_Name,naics_desc,rank) %>%
  pivot_wider(names_from = rank, values_from = naics_desc,
              values_fn = list(naics_desc = function(x) paste(x, collapse = "; "))) 

state_EA_table<-EA_table_5 %>%
  filter(state_avb ==state_abbreviation)

write.csv(state_EA_table,paste0(output_folder,"/",state_abbreviation,"_EA_table.csv"),row.names=F)


#MSA Feasibility
msa_table_5<-feasibility %>%
  filter(region=="MSA",
         transition_sector_category %in% c("Transition Mineral and Metal Mining Sector",
                                           "Industrial End-Use Sector",
                                           "Transition Chemical, Mineral, and Metal Manufacturing Sector",
                                           "Buildings End-Use Sector",
                                           "Transportation End-Use Sector",
                                           "Transition Enabling Sector" 
                                           )) %>%
  mutate(MSA_Name=gsub(" \\(MSA\\)","",msa_name)) %>%
  group_by(state_avb,MSA_Name) %>%
  slice_max(feas_industry_percentile,n=5) %>%
  mutate(rank=rank(-feas_industry_percentile)) %>%
  select(MSA_Name,naics_desc,rank) %>%
  pivot_wider(names_from = rank, values_from = naics_desc,
              values_fn = list(naics_desc = function(x) paste(x, collapse = "; "))) 

state_MSA_table<-msa_table_5 %>%
  filter(state_avb ==state_abbreviation)

write.csv(state_EA_table,paste0(output_folder,"/",state_abbreviation,"_EA_table.csv"),row.names=F)


#MSA Variables
EA_ranks<-msa_data %>%
  filter(region=="EA") %>%
  select(msa_name,percent_change_green_jobs_l5,green_share,unemployment_percent,capex,inv_gdp,ren_share_22,incent_gdp_rank,state_ems_change_1621,state_effective_tax_rate,med_house_inc,pov_rate,emp_pop,ind_elec_price,ren_cagr_20_23,gdp_17_22,property_value_usd,invest_index,worker_pay_x,cnbc_rank) %>%
  mutate(across(c(percent_change_green_jobs_l5,green_share,unemployment_percent,capex,inv_gdp,ren_share_22,incent_gdp_rank,state_ems_change_1621,state_effective_tax_rate,med_house_inc,pov_rate,emp_pop,ind_elec_price,ren_cagr_20_23,gdp_17_22,property_value_usd,invest_index,worker_pay_x,cnbc_rank),
                ~ rank(-.))) 

state_ranks <- msa_data %>%
  left_join(states_msa,by=c("msa"="cbsa")) %>%
  group_by(state_name,state_avb) %>%
  summarize(across(c(percent_change_green_jobs_l5,green_share,unemployment_percent,capex,inv_gdp,ren_share_22,incent_gdp_rank,state_ems_change_1621,state_effective_tax_rate,med_house_inc,pov_rate,emp_pop,ind_elec_price,ren_cagr_20_23,gdp_17_22,property_value_usd,invest_index,worker_pay_x,cnbc_rank),
                mean,na.rm=T)) 


#Misc
feasibility_naics<-naics_data %>%
  select(transition_sector_category_id,naics,naics_desc) %>%
  right_join(transition,by=c("transition_sector_category_id"="transition_sector_category_id")) %>%
  select(transition_sector_category,naics,naics_desc) %>%
  inner_join(naics_msa_data,by=c("naics"="naics")) %>%
  filter(aggregation_level=="3") %>%
  left_join(states_msa,by=c("msa"="cbsa")) %>%
  left_join(msa_data %>%
              select(region,msa,msa_name),by=c("msa"="msa")) %>%
  select(statefp,state_name,state_avb,region,msa,msa_name,transition_sector_category, naics,naics_desc,density,pci,rca,jobs,jobs_l5,share_good_jobs,percent_change_jobs_l5) %>%
  group_by(naics,naics_desc) %>%
  mutate(feas_industry_percentile=percent_rank(density)) %>%
  distinct()

ben<-feasibility_naics %>% 
  filter(grepl("Wenatchee",msa_name),
         grepl("Nonferrous",naics_desc)) 