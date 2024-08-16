# Feasibility & Clean Growth Tool for Great Lakes Region
# Last edited by Rhea Cong, 07/09/2024
# run comparisons for great_lakes states

# State Variables
state_abbreviation <- states_simple$abbr[which(states_simple$region=="great_lakes")]
state_name <- states_simple$full[which(states_simple$region=="great_lakes")]
region_id <- us_counties %>%
  filter(abbr %in% state_abbreviation) 

#Set the Working Directory to your Username
# setwd("C:/Users/LCarey.RMI/")

# set output folder
output_folder <- "./MN"

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

# loop to produce all great lakes states
for (i in (1:length(state_abbreviation))){
  state_feas_plot<-ggplot(data=state_feas_msa %>% filter(state_avb==state_abbreviation[i],
                                                       !transition_sector_category %in% c("Environmental Protection & Management End-Use Sector",
                                                                                          "Transition Enabling Sector",
                                                                                          "Transition Forestry, Land, and Agriculture (FLAG) Sector")),
                        aes(x=feas_industry_percentile,y=pci,color=transition_sector_category,size=jobs))+
  #geom_vline(xintercept = mean(state_feas$feas_industry_percentile),color='darkgrey') +
  geom_hline(yintercept= mean(state_feas$pci) ,color='darkgrey') +
  geom_point()+
  scale_color_manual(values=expanded_palette)+
  geom_text_repel(aes(label=naics_desc),size=2, max.overlaps = 10)+
  theme_classic()+
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",state_name[i]),
       subtitle=paste0("Clean Energy Industry Transition Feasibility in ",state_name[i],".", "\n", "Size of bubble represents number of jobs in the sector"),
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
  
  ggsave(paste0(output_folder,"/",state_abbreviation[i],"_feasibility.png"),plot=state_feas_plot,width=8,height=6,units="in",dpi=300)
}

# loop for top feasibility clean growth sectors in each state
for (i in (1:length(state_abbreviation))){
  state_feas_plot2<-ggplot(data=state_feas_msa %>% filter(state_avb==state_abbreviation[i],
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
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",state_name[i]),
       subtitle=paste0("High Feasibility Clean Energy Industries for ",state_name[i]),
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
  
  ggsave(paste0(output_folder,"/",state_abbreviation[i],"_feasibility_industry.png"),plot=state_feas_plot2,width=8,height=6,units="in",dpi=300)
}

# loop for naics feasibility graph
for (i in 1:length(state_abbreviation)){
state_feasnaics_plot<-ggplot(data=state_feas_naics %>% filter(state_avb==state_abbreviation[i],
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
  labs(title=paste0("Feasibility of Clean Growth Sectors in ",state_name[i]),
       subtitle=paste0("High Complexity Clean Energy Industries for which ",state_name[i]," has above-average transition feasibility.", "\n", "Size of bubble represents number of jobs in the sector"),
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

ggsave(paste0(output_folder,"/",state_abbreviation[i],"_feasibility_naics.png"),plot=state_feasnaics_plot,width=8,height=6,units="in",dpi=300)
}

# Compare job creation across states, specifically in manufacturing, construction, trade
#Net Zero Scenario - Net Zero America
file_url <- 'https://netzeroamerica.princeton.edu/data/nzap-data.csv'
temp_file <- tempfile(fileext = ".csv")
GET(url = file_url, write_disk(temp_file, overwrite = TRUE))
nza <- read.csv(temp_file)  # 'sheet = 1' to read the first sheet

nza<-nza %>%
  mutate(geo=str_to_title(geo)) 
nzap<-left_join(nza,census_divisions,by=c("geo"="State"))

#States
nza_states<-nzap %>%
  filter(scenario %in% c("REF","E+","E+RE+")) %>%
  drop_na(value) %>%
  filter(geo != "national") %>%
  group_by(year,geo,State.Code,scenario,filter_level_1,filter_level_2,filter_level_3,variable_name,unit) %>%
  summarize_at(vars(value),sum) %>%
  spread(year,value) %>%
  filter(!is.na(State.Code)) %>%
  mutate(scenario = factor(scenario, levels = c("REF", "E+", "E+RE+")))

#Jobs by Economic Sector
nza_jobs_econ <- nza_states %>%
  ungroup()%>%
  filter(State.Code%in%state_abbreviation) %>%
  filter(filter_level_2=="Jobs",
         filter_level_3=="By economic sector") %>%
  mutate(variable_name=gsub("By economic sector - ","",variable_name)) %>%
  select(State.Code,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
  pivot_longer(cols=c(`2025`,`2030`,`2035`,`2040`,`2045`,`2050`),names_to="year",values_to="Value") %>%
  mutate(code=case_when(
    variable_name=="Agriculture"~"11",
    variable_name=="Mining"~"21",
    variable_name=="Utilities"~"22",
    variable_name=="Construction"~"23",
    variable_name=="Manufacturing"~"31-33",
    variable_name=="Trade"~"42",
    variable_name=="Pipeline"~"48-49",
    variable_name=="Professional"~"54",
    variable_name=="Other"~"Other"
  ))

nza_jobs_region <- nza_jobs_econ %>%
  left_join(region_cbp_2d %>% filter(region_id==1) %>% select(code,share),by=c("code"="code")) %>%
  mutate(Value=Value*share) 

nza_jobs_region_filtered <- nza_jobs_region %>%
  filter(variable_name %in% c("Manufacturing","Construction","Trade"))

nza_jobs_region_manu <- nza_jobs_region %>%
  filter(variable_name =="Manufacturing") %>%
  pivot_wider(names_from = "State.Code", values_from = "Value")

write.csv(nza_jobs_region_manu,file = "./DataWrapper/Great_Lakes_Manu_Jobs.csv")

nza_jobs_region_const <- nza_jobs_region %>%
  filter(variable_name =="Construction") %>%
  pivot_wider(names_from = "State.Code", values_from = "Value")

write.csv(nza_jobs_region_const,file = "./DataWrapper/Great_Lakes_Const_Jobs.csv")

nza_jobs_region_trade <- nza_jobs_region %>%
  filter(variable_name =="Trade") %>%
  pivot_wider(names_from = "State.Code", values_from = "Value")

write.csv(nza_jobs_region_trade,file = "./DataWrapper/Great_Lakes_Trade_Jobs.csv")



# plot_nza_jobs_econ<-ggplot(data=nza_jobs_region_filtered, aes(x=year,y=Value,fill=variable_name)) +
#   geom_col(position='stack') +
#   facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
#   scale_fill_manual(values = expanded_palette)+
#   labs(title=paste("Job Creation in", region_name[i],",","in a Net Zero Scenario"), 
#        x="Year", y="Jobs",
#        fill="Economic Sector",
#        caption="Source: Net Zero America (2021), Princeton University") +
#   scale_y_continuous(expand = c(0,0))+
#   theme_classic()+
#   theme(legend.position="bottom")
# 
# ggsave(paste0(output_folder,"/",state_abbreviation,"_nza_jobs_econ.png"),plot=plot_nza_jobs_econ,width=8,height=6,units="in",dpi=300)
# 
# 
# #Jobs by Resource Sector
# nza_jobs_resource <- nza_states %>%
#   ungroup()%>%
#   filter(State.Code%in%state_abbreviation) %>%
#   filter(filter_level_2=="Jobs",
#          filter_level_3=="By resource sector") %>%
#   mutate(variable_name=gsub("By resource sector - ","",variable_name)) %>%
#   select(State.Code,variable_name,scenario,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`) %>%
#   pivot_longer(cols=c(`2025`,`2030`,`2035`,`2040`,`2045`,`2050`),names_to="year",values_to="Value")
# 
# nza_jobs_resource_region <- nza_jobs_resource %>%
#   mutate(Value=Value*region_totalemp$share) 
# 
# 
# plot_nza_jobs_resource<-ggplot(data=nza_jobs_resource_region, aes(x=year,y=Value,fill=variable_name)) +
#   geom_col(position='stack') +
#   facet_wrap(~scenario) +  # Adding faceting to create separate plots for each scenario
#   scale_fill_manual(values = expanded_palette)+
#   labs(title=paste("Job Creation in", region_name,",","in a Net Zero Scenario"), 
#        x="Year", y="Jobs",
#        fill="Resource Sector",
#        caption="Source: Net Zero America (2021), Princeton University") +
#   scale_y_continuous(expand = c(0,0))+
#   theme_classic()+
#   theme(legend.position="bottom")
# 
# ggsave(paste0(output_folder,"/",state_abbreviation,"_nza_jobs_resource.png"),plot=plot_nza_jobs_resource,width=8,height=6,units="in",dpi=300)
# 
