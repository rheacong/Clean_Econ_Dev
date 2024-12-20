# Feasibility & Clean Growth Tool
# Last edited by Rhea Cong, 11/11/2024

### INPUTS
# Clean Growth Tool Data from 04/26/2024

### OUTPUTS
# feasibility and complexity graphs at state and regional level, by technology and industry
# DataWrapper files for top complexity and feasibility industries

# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "LA"  # Replace with any US state abbreviation
state_name <- "Louisiana"  # Replace with the full name of any US state
region_name <- "New Orleans-Metairie, LA"

#Set the Working Directory to your Username
# setwd("C:/Users/LCarey.RMI/")

# set output folder
output_folder <- "./LA"

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
                                                                                          #"Transition Enabling Sector",
                                                                                          "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
                        aes(x=feas_industry_percentile,y=pci,color=transition_sector_category,size=jobs))+
  #geom_vline(xintercept = mean(state_feas$feas_industry_percentile),color='darkgrey') +
  geom_hline(yintercept= mean(state_feas_msa$pci) ,color='darkgrey') +
  geom_point()+
  scale_color_manual(values=expanded_palette)+
  geom_text_repel(aes(label=naics_desc),size=2, max.overlaps = 10)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",state_name),
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

ggsave(paste0(output_folder,"/",state_abbreviation,"_feasibility.png"),plot=state_feas_plot,width=8,height=6,units="in",dpi=300)

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
                                                                                "Transition Enabling Sector",
                                                                                "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
                     aes(x=feas_industry_percentile,y=pci,color=transition_sector_category,size=jobs))+
  #geom_vline(xintercept = mean(EA_feas$feas_industry_percentile),color='darkgrey') +
  geom_hline(yintercept= mean(EA_feas$pci) ,color='darkgrey') +
  geom_point()+
  scale_color_manual(values=expanded_palette)+
  geom_text_repel(aes(label=naics_desc),size=2, max.overlaps = 10)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",region_name),
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

ggsave(paste0(output_folder,"/",state_abbreviation,"_EA_feasibility.png"),plot=EA_feas_plot,width=8,height=6,units="in",dpi=300)


# MSA feas
MSA_feas <- feasibility %>%
  mutate(MSA_Name=gsub(" \\(MSA\\)","",msa_name)) %>%
  left_join(msa_pop,by=c("MSA_Name"="CBSA.Title")) %>%
  filter(region=="MSA") %>%
  group_by(MSA_Name,state_avb,transition_sector_category,naics_desc, naics) %>%
  summarize(across(c(density,pci,share_good_jobs),
                   ~weighted.mean(.x,w=POPESTIMATE2022,na.rm=T))) %>%
  filter(MSA_Name==region_name, state_avb==state_abbreviation) %>%
  ungroup() %>%
  mutate(feas_industry_percentile=percent_rank(density)) %>%
  left_join(state_totals,by=c("state_avb"="state_avb","naics_desc"="naics_desc") )

MSA_feas_plot<-ggplot(data=MSA_feas %>% filter(!transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                  "Transition Enabling Sector",
                                                                                  "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
                      aes(x=feas_industry_percentile,y=pci,color=transition_sector_category,size=jobs))+
  geom_vline(xintercept = mean(MSA_feas$feas_industry_percentile),color='darkgrey') +
  geom_hline(yintercept= mean(MSA_feas$pci) ,color='darkgrey') +
  geom_point()+
  scale_color_manual(values=expanded_palette)+
  geom_text_repel(aes(label=naics_desc),size=2, max.overlaps = 10)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",region_name),
       subtitle=paste0("Clean Energy Industry Transition Feasibility in ",state_name,".", "\n", "Size of bubble represents number of jobs in the sector"),
       x="Feasibility percentile",
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

ggsave(paste0(output_folder,"/",state_abbreviation,"_MSA_feasibility.png"),plot=MSA_feas_plot,width=8,height=6,units="in",dpi=300)


# Highest feasibility sectors in state
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

state_feas_msa %>% filter(state_avb==state_abbreviation,
                          !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                             #"Transition Enabling Sector",
                                                             "Transition Forestry, Land, and Agriculture (FLAG) Sector")) %>%
  group_by(state_avb) %>%
  slice_max(order_by=feas_industry_percentile,n=10) %>%
  write.csv(file= paste0("./DataWrapper/", state_abbreviation, "_top10_feasibility"))


# Highest feasibility in EA
EA_feas_plot2<-ggplot(data=EA_feas %>% filter(!transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                 "Transition Enabling Sector",
                                                                                 "Transition Forestry, Land, and Agriculture (FLAG) Sector")) %>%
                        ungroup() %>%
                        slice_max(order_by=feas_industry_percentile,n=10),
                      aes(x=reorder(naics_desc,feas_industry_percentile),y=feas_industry_percentile,fill=transition_sector_category))+
  scale_y_continuous(expand=c(0,0))+
  geom_col()+
  coord_flip() +
  scale_fill_manual(values=expanded_palette)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",region_name),
       subtitle=paste0("High Feasibility Clean Energy Industries for ",region_name),
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

ggsave(paste0(output_folder,"/","EA_feasibility_industry.png"),plot=EA_feas_plot2,width=10,height=6,units="in",dpi=300)


MSA_feas_plot2<-ggplot(data=MSA_feas %>% filter(!transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                   "Transition Enabling Sector",
                                                                                   "Transition Forestry, Land, and Agriculture (FLAG) Sector")) %>%
                         ungroup() %>%
                         slice_max(order_by=feas_industry_percentile,n=10),
                       aes(x=reorder(naics_desc,feas_industry_percentile),y=feas_industry_percentile,fill=transition_sector_category))+
  scale_y_continuous(expand=c(0,0))+
  geom_col()+
  coord_flip() +
  scale_fill_manual(values=expanded_palette)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",region_name),
       subtitle=paste0("High Feasibility Clean Energy Industries for ",region_name),
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

ggsave(paste0(output_folder,"/","MSA_feasibility_industry.png"),plot=MSA_feas_plot2,width=10,height=6,units="in",dpi=300)

MSA_feas %>% filter(!transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                       #"Transition Enabling Sector",
                                                       "Transition Forestry, Land, and Agriculture (FLAG) Sector")) %>%
  ungroup() %>%
  slice_max(order_by=feas_industry_percentile,n=10) %>%
  write.csv(file=paste0("./DataWrapper/",state_abbreviation, "_msa_top10_feas"))

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
  summarize(across(c(density,pci,share_good_jobs),
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

ggsave(paste0(output_folder,"/",state_abbreviation,"_feasibility_naics.png"),plot=state_feasnaics_plot,width=8,height=6,units="in",dpi=300)

# Write csv for DataWrapper
write.csv(state_feas_naics %>% filter(state_avb==state_abbreviation,
                                      !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                         #"Transition Enabling Sector",
                                                                         "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
          file=paste0("./DataWrapper/", state_abbreviation, "_state_feas_naics"))

# MSA feas naics plot
msa_feasnaics <- feasibility_naics %>% filter(state_avb==state_abbreviation, msa_name==paste0(region_name, " (MSA)"),
                                              !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                 #"Transition Enabling Sector",
                                                                                 "Transition Forestry, Land, and Agriculture (FLAG) Sector"))

msa_feasnaics_plot<-ggplot(data=feasibility_naics %>% filter(state_avb==state_abbreviation, msa_name==paste0(region_name, " (MSA)"),
                                                             !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                                #"Transition Enabling Sector",
                                                                                                "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
                           aes(x=density,y=pci,color=transition_sector_category,size=jobs))+
  geom_vline(xintercept = mean(msa_feasnaics$density),color='darkgrey') +
  geom_hline(yintercept= mean(msa_feasnaics$pci) ,color='darkgrey') +
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

ggsave(paste0(output_folder,"/",state_abbreviation,"_msafeasibility_naics.png"),plot=msa_feasnaics_plot,width=10,height=6,units="in",dpi=300)

# Write csv for DataWrapper
write.csv(feasibility_naics %>% filter(state_avb==state_abbreviation, msa_name==paste0(region_name, " (MSA)"),
                                       !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                          #"Transition Enabling Sector",
                                                                          "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
          file=paste0("./DataWrapper/", state_abbreviation, "_msa_feas_naics"))

# Write out top right quadrant for DataWrapper
top_feasibility_naics <- msa_feasnaics %>%
  filter(density>mean(state_feas$density), 
         pci>mean(state_feas$pci)) %>%
  left_join(eti_long %>% select(`4-Digit Description`, Technology), by=c("naics_desc"="4-Digit Description")) %>%
  group_by(Technology) %>%
  summarise(avgdensity=mean(density), avgcomplexity=mean(pci),sumjobs=sum(jobs))

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

# Basic employment data
msa_employment <- msa_data %>%
  filter(msa_name==paste0(region_name, " (MSA)"))

msa_feasibility<-naics_data %>%
  select(transition_sector_category_id,naics,naics_desc) %>%
  right_join(transition,by=c("transition_sector_category_id"="transition_sector_category_id")) %>%
  select(transition_sector_category,naics,naics_desc) %>%
  inner_join(naics_msa_data,by=c("naics"="naics")) %>%
  filter(aggregation_level=="2") %>%
  left_join(states_msa,by=c("msa"="cbsa")) %>%
  inner_join(msa_employment %>%
               select(region,msa,msa_name),by=c("msa"="msa")) %>%
  select(statefp,state_name,state_avb,region,msa,msa_name,transition_sector_category, naics,naics_desc,density,pci,rca,jobs,jobs_l5,share_good_jobs,percent_change_jobs_l5) %>%
  #group_by(naics,naics_desc) %>%
  mutate(feas_industry_percentile=percent_rank(density)) %>%
  filter(state_avb==state_abbreviation)%>%
  distinct()

write.csv(msa_feasibility %>% select(-statefp, -state_avb,-region,-msa,-msa_name) %>%
            filter(!transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                      "Transition Forestry, Land, and Agriculture (FLAG) Sector")), file=paste0("./DataWrapper/",state_abbreviation,"msa_sector_feas"))

msa_top_10 <- msa_feasibility %>%
  slice_max(feas_industry_percentile, n=10) %>%
  select(-statefp, -state_name, -region, -msa) %>%
  left_join(eti_long %>% select(Technology, Subsector), by=c("naics_desc"="Technology")) %>%
  distinct()

write.csv(msa_top_10, file=paste0("./DataWrapper/",state_abbreviation, "_msa_top_10_feas"))

msa_top_10_filter <- msa_feasibility %>%
  filter(!transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                            "Transition Forestry, Land, and Agriculture (FLAG) Sector")) %>%
  slice_max(feas_industry_percentile, n=10) %>%
  select(-statefp, -state_name, -region, -msa) %>%
  left_join(eti_long %>% select(Technology, Subsector), by=c("naics_desc"="Technology")) %>%
  distinct()

write.csv(msa_top_10_filter, file=paste0("./DataWrapper/",state_abbreviation, "_msa_top_10_feas_filtered"))

msa_top_lq <- msa_feasibility %>%
  slice_max(rca, n=10) %>%
  select(-statefp, -state_name, -region, -msa) %>%
  left_join(eti_long %>% select(Technology, Subsector), by=c("naics_desc"="Technology")) %>%
  distinct()

write.csv(msa_top_lq, file=paste0("./DataWrapper/",state_abbreviation, "_msa_top_10_lq"))

# State top right quadrant
state_feas1 <- state_feas %>%
  filter(state_avb == state_abbreviation, 
         !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                            "Transition Forestry, Land, and Agriculture (FLAG) Sector"))
state_top <- state_feas1 %>%
  filter(density>mean(state_feas1$density), 
         pci>mean(state_feas1$pci))

write.csv(state_top, file = paste0("./DataWrapper/", state_abbreviation, "_top_quad_feas_complex_tech"))


# Group by transition sector
msa_feasibility_transition <- msa_feasibility %>%
  group_by(transition_sector_category) %>%
  summarise(avglq=mean(rca), avgdensity=mean(density), avgcomplexity=mean(pci), jobs=sum(jobs))

write.csv(msa_feasibility_transition, file=paste0("./DataWrapper/",state_abbreviation, "_msa_transition"))

# County-level CGT
cgt_county <- read.csv("./Data/cgt_county_data_08_29_2024.csv")

ten_parrish <- c("Washington Parish, LA", "Tangipahoa Parish, LA", "St. Tammany Parish, LA", "St. John the Baptist Parish, LA",
                 "St. James Parish, LA", "St. Charles Parish, LA", "Orleans Parish, LA","St. Bernard Parish, LA",
                 "Jefferson Parish, LA", "Plaquemines Parish, LA")

ten_parrish_names <- c("Washington Parish", "Tangipahoa Parish", "St. Tammany Parish", "St. John the Baptist Parish",
                      "St. James Parish", "St. Charles Parish", "Orleans Parish","St. Bernard Parish",
                      "Jefferson Parish", "Plaquemines Parish")

county_pop_region <- county_pop %>% filter(CTYNAME %in% ten_parrish_names) %>% select(CTYNAME,POPESTIMATE2023, COUNTY)
county_pop_region$County <- paste0(county_pop_region$CTYNAME, ", LA")

cgt_county_region <- cgt_county %>% filter(county_name %in% ten_parrish) %>% 
  left_join(county_pop_region, by=c("county_name"="County")) %>%
  group_by(transition_sector_category,primary_transition_products_technologies) %>%
  summarize(across(c(density,pci, share_good_jobs),~ weighted.mean(.x, w = POPESTIMATE2023, na.rm = TRUE),
                   .names = "weighted_{col}"))

write.csv(cgt_county_region, file = "./DataWrapper/LA_NOLA_region_feas")


  