#Feasibility & Clean Growth Tool
# Last edited by Rhea Cong, 06/24/2024

# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "MT"  # Replace with any US state abbreviation
state_name <- "Montana"  # Replace with the full name of any US state
region_name <- "Great Falls, MT"

#Set the Working Directory to your Username
# setwd("C:/Users/LCarey.RMI/")

# set output folder
output_folder <- "./GFDA"

#Load Latest Clean Growth Tool Data
cgt<-readRDS('./Data/acre_tool_final_data_042624')

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
  mutate(FIPS=paste0(sprintf("%02d", STATE), sprintf("%03d", COUNTY)),
         fip=as.numeric(FIPS)) %>%
  left_join(EAs,by=c("FIPS"="FIPS")) %>%
  filter(!is.na(County)) %>%
  select(fips,County,`EA Name`,POPESTIMATE2022) %>%
  group_by(`EA Name`) %>%
  summarize_at(vars(POPESTIMATE2022),sum,na.rm=T) %>%
  arrange(desc(POPESTIMATE2022))

msa_pop <- county_pop %>%
  select(STATE,COUNTY,POPESTIMATE2022) %>%
  mutate(FIPS=paste0(sprintf("%02d", STATE), sprintf("%03d", COUNTY)),
         fips=as.numeric(FIPS)) %>%
  left_join(county_cbsa,by=c("fips"="fips")) %>%
  #filter(!is.na(County)) %>%
  select(fips,CBSA.Title,POPESTIMATE2022) %>%
  group_by(CBSA.Title) %>%
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

state_feas_msa <- feasibility %>%
  mutate(MSA_Name=gsub(" \\(MSA\\)","",msa_name)) %>%
  left_join(msa_pop,by=c("MSA_Name"="CBSA.Title")) %>%
  filter(region=="MSA") %>%
  group_by(state_avb,transition_sector_category,naics_desc) %>%
  summarize(across(c(density,pci,share_good_jobs),
                   ~weighted.mean(.x,w=POPESTIMATE2022,na.rm=T))) %>%
  group_by(naics_desc) %>%
  mutate(feas_industry_percentile=percent_rank(density)) %>%
  left_join(state_totals,by=c("state_avb"="state_avb","naics_desc"="naics_desc") )

state_feas_plot<-ggplot(data=state_feas_msa %>% filter(state_avb==state_abbreviation,
                                                       !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                          "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
                        aes(x=feas_industry_percentile,y=pci,color=transition_sector_category,size=jobs))+
  geom_vline(xintercept = mean(state_feas$feas_industry_percentile),color='darkgrey') +
  geom_hline(yintercept= mean(state_feas$pci) ,color='darkgrey') +
  geom_point()+
  scale_color_manual(values=expanded_palette)+
  geom_text_repel(aes(label=naics_desc),size=2, max.overlaps = 10)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",state_name),
       subtitle=paste0("Clean Energy Industry Transition Feasibility in ",state_name,".", "\n", "Size of bubble represents number of jobs in the sector"),
       x="Feasibility",
       y="Complexity",
       color="Sector",
       caption="Source: RMI, Clean Growth Tool")+
  #xlim(0.4,1)+
  #ylim(0,7)+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 4),   # Decrease text size
        legend.title = element_text(size = 5),  # Decrease title size
        legend.key.size = unit(0.3, "cm"),      # Decrease key size
        legend.spacing = unit(0.2, "cm")) +
  guides(size = "none")

# Filter for just the EA in the state
EA_feas <- feasibility %>%
  mutate(EA_Name=gsub(" \\(EA\\)","",msa_name)) %>%
  left_join(ea_pop,by=c("EA_Name"="EA Name")) %>%
  filter(region=="EA") %>%
  group_by(EA_Name,state_avb,transition_sector_category,naics_desc, naics) %>%
  summarize(across(c(density,pci,share_good_jobs),
                   ~weighted.mean(.x,w=POPESTIMATE2022,na.rm=T))) %>%
  filter(EA_Name==region_name, state_avb==state_abbreviation) %>%
  ungroup() %>%
  mutate(feas_industry_percentile=percent_rank(density)) %>%
  left_join(state_totals,by=c("state_avb"="state_avb","naics_desc"="naics_desc") )

EA_feas_plot<-ggplot(data=EA_feas %>% filter(!transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
                     aes(x=feas_industry_percentile,y=pci,color=transition_sector_category,size=jobs))+
  geom_vline(xintercept = mean(EA_feas$feas_industry_percentile),color='darkgrey') +
  geom_hline(yintercept= mean(EA_feas$pci) ,color='darkgrey') +
  geom_point()+
  scale_color_manual(values=expanded_palette)+
  geom_text_repel(aes(label=naics_desc),size=2, max.overlaps = 10)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",region_name),
       subtitle=paste0("Clean Energy Industry Transition Feasibility in ",state_name,".", "\n", "Size of bubble represents number of jobs in the sector"),
       x="Feasibility Percentile",
       y="Complexity",
       color="Sector",
       caption="Source: RMI, Clean Growth Tool")+
  #xlim(0.4,1)+
  #ylim(0,7)+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 4),   # Decrease text size
        legend.title = element_text(size = 5),  # Decrease title size
        legend.key.size = unit(0.3, "cm"),      # Decrease key size
        legend.spacing = unit(0.2, "cm")) +
  guides(size = "none")

ggsave(paste0(output_folder,"/",state_abbreviation,"_EA_feasibility.png"),plot=EA_feas_plot,width=8,height=6,units="in",dpi=300)


ggsave(paste0(output_folder,"/",state_abbreviation,"_feasibility.png"),plot=state_feas_plot,width=8,height=6,units="in",dpi=300)
write.csv(state_feas_msa %>% filter(state_avb==state_abbreviation), file=paste0("./DataWrapper",state_abbreviation,"_feasibility_data.csv"))

state_feas_plot2<-ggplot(data=state_feas_msa %>% filter(state_avb==state_abbreviation,
                                                        !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                           "Transition Enabling Sector",
                                                                                           "Transition Forestry, Land, and Agriculture (FLAG) Sector")) %>%
                           group_by(state_avb) %>%
                           slice_max(order_by=feas_industry_percentile,n=10),
                         aes(x=reorder(naics_desc,feas_industry_percentile),y=feas_industry_percentile,fill=transition_sector_category))+
  scale_y_continuous(expand=c(0,0))+
  geom_col()+
  coord_flip() +
  scale_fill_manual(values=expanded_palette)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",state_name),
       subtitle=paste0("High Feasibility Clean Energy Industries for ",state_name),
       x="Industry",
       y="Feasibility",
       fill="Sector",
       caption="Source: RMI, Clean Growth Tool")+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 4),   # Decrease text size
        legend.title = element_text(size = 5),  # Decrease title size
        legend.key.size = unit(0.3, "cm"),      # Decrease key size
        legend.spacing = unit(0.2, "cm"))

ggsave(paste0(output_folder,"/",state_abbreviation,"_feasibility_industry.png"),plot=state_feas_plot2,width=8,height=6,units="in",dpi=300)

# Write for DataWrapper
write.csv(state_feas_msa %>% filter(state_avb==state_abbreviation,
                                    !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                       "Transition Enabling Sector",
                                                                       "Transition Forestry, Land, and Agriculture (FLAG) Sector")) %>%
            group_by(state_avb) %>%
            slice_max(order_by=feas_industry_percentile,n=10), file=paste0("./DataWrapper/",state_abbreviation,"_top_feasibility_sectors"))

#NAICS Code Feasibility
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

state_naics_totals <- feasibility_naics %>%
  group_by(state_avb,naics_desc) %>%
  summarize_at(vars(jobs),sum,na.rm=T)

state_feas_naics <- feasibility_naics %>%
  mutate(EA_Name=gsub(" \\(EA\\)","",msa_name)) %>%
  left_join(ea_pop,by=c("EA_Name"="EA Name")) %>%
  filter(region=="EA",
         EA_Name != "Phoenix-Mesa-Scottsdale, AZ") %>%
  group_by(state_avb,transition_sector_category,naics_desc) %>%
  summarize(across(c(density,pci,share_good_jobs, jobs),
                   ~weighted.mean(.x,w=POPESTIMATE2022,na.rm=T))) %>%
  group_by(naics_desc) %>%
  mutate(feas_industry_percentile=percent_rank(density)) %>%
  left_join(state_naics_totals,by=c("state_avb"="state_avb","naics_desc"="naics_desc") ) %>%
  #left_join(eti_long %>% select(Subsector,`4-Digit Description`),by=c("naics_desc"="4-Digit Description")) %>%
  distinct()


state_feasnaics_plot<-ggplot(data=state_feas_naics %>% filter(state_avb==state_abbreviation,
                                                              !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                                 #"Transition Enabling Sector",
                                                                                                 "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
                             aes(x=density,y=pci,color=transition_sector_category,size=jobs))+
  geom_vline(xintercept = mean(state_feas$density),color='darkgrey') +
  geom_hline(yintercept= mean(state_feas$pci) ,color='darkgrey') +
  geom_point()+
  scale_color_manual(values=expanded_palette)+
  geom_text_repel(aes(label=ifelse(feas_industry_percentile> 0,naics_desc,"")),size=2, max.overlaps = 10)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",state_name),
       subtitle=paste0("High Complexity Clean Energy Industries for which ",state_name," has above-average transition feasibility.", "\n", "Size of bubble represents number of jobs in the sector"),
       x="Feasibility",
       y="Complexity",
       color="Sector",
       caption="Source: RMI, Clean Growth Tool")+
  #xlim(0.4,1)+
  #ylim(0,7)+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 4),   # Decrease text size
        legend.title = element_text(size = 5),  # Decrease title size
        legend.key.size = unit(0.3, "cm"),      # Decrease key size
        legend.spacing = unit(0.2, "cm")) +
  guides(size = "none")

ggsave(paste0(output_folder,"/",state_abbreviation,"_feasibilitynaics.png"),plot=state_feasnaics_plot,width=10,height=6,units="in",dpi=300)

# NAICS feas at EA level
EA_feas_naics <- feasibility_naics %>%
  mutate(EA_Name=gsub(" \\(EA\\)","",msa_name)) %>%
  left_join(ea_pop,by=c("EA_Name"="EA Name")) %>%
  filter(region=="EA",
         EA_Name == region_name)


EA_feasnaics_plot<-ggplot(data=feasibility_naics %>% filter(msa_name == paste0(region_name," (EA)"),
                                                              !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                                 #"Transition Enabling Sector",
                                                                                                 "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
                             aes(x=density,y=pci,color=transition_sector_category,size=jobs))+
  geom_vline(xintercept = mean(EA_feas_naics$density),color='darkgrey') +
  geom_hline(yintercept= mean(EA_feas_naics$pci) ,color='darkgrey') +
  geom_point()+
  scale_color_manual(values=expanded_palette)+
  geom_text_repel(aes(label=ifelse(feas_industry_percentile> 0,naics_desc,"")),size=2, max.overlaps = 10)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",region_name),
       subtitle=paste0("High Complexity Clean Energy Industries for which ",state_name," has above-average transition feasibility.", "\n", "Size of bubble represents number of jobs in the sector"),
       x="Feasibility",
       y="Complexity",
       color="Sector",
       caption="Source: RMI, Clean Growth Tool")+
  #xlim(0.4,1)+
  #ylim(0,7)+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 4),   # Decrease text size
        legend.title = element_text(size = 5),  # Decrease title size
        legend.key.size = unit(0.3, "cm"),      # Decrease key size
        legend.spacing = unit(0.2, "cm")) +
  guides(size = "none")

#write out top above average feasibility for DataWrapper
top_feasibility_naics <- EA_feas_naics %>%
  filter(density>mean(EA_feas_naics$density),
         transition_sector_category %in% c("Energy End-Use Sector" ,
                                           "Transition Mineral and Metal Mining Sector",
                                           "Industrial End-Use Sector",
                                           "Transition Chemical, Mineral, and Metal Manufacturing Sector",
                                           "Buildings End-Use Sector",
                                           "Transportation End-Use Sector",
                                           "Transition Enabling Sector"))

write.csv(top_feasibility_naics, file=paste0("./DataWrapper/",state_abbreviation, "_topfeasnaics"))


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

write.csv(state_MSA_table,paste0(output_folder,"/",state_abbreviation,"_MSA_table.csv"),row.names=F)


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

# Narrower view of top feas
ea_top_10_filter <- EA_feas %>%
  mutate(feas_complex = feas_industry_percentile+pci) %>%
  filter(!transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                            "Transition Forestry, Land, and Agriculture (FLAG) Sector")) %>%
  slice_max(feas_complex, n=10) %>%
  left_join(eti_long %>% select(Technology, Subsector), by=c("naics_desc"="Technology")) %>%
  distinct()

write.csv(msa_top_10_filter, file="./DataWrapper/msa_top_10_feas_filtered")

msa_top_lq <- msa_feasibility %>%
  slice_max(rca, n=10) %>%
  select(-statefp, -state_name, -region, -msa) %>%
  left_join(eti_long %>% select(Technology, Subsector), by=c("naics_desc"="Technology")) %>%
  distinct()

write.csv(msa_top_lq, file="./DataWrapper/msa_top_10_lq")

# Group by transition sector
msa_feasibility_transition <- msa_feasibility %>%
  group_by(transition_sector_category) %>%
  summarise(avglq=mean(rca), avgdensity=mean(density), avgcomplexity=mean(pci), jobs=sum(jobs))

write.csv(state_feasibility_transition, file="./DataWrapper/state_transition")


