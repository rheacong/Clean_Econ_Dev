#Clean Energy Innovation

#ITIF - US State and Regional Energy Innovation Index

#https://itif.org/publications/2024/05/28/state-regional-energy-innovation-index/

url <- 'https://cdn.sanity.io/files/03hnmfyj/production/77717b609392dedba6f8ba316ce16d6629bf6666.xlsx'
temp_file <- tempfile(fileext = ".xlsx")
GET(url = url, write_disk(temp_file, overwrite = TRUE))
innov_state <- read_excel(temp_file, sheet = 3)  # 'sheet = 1' to read the first sheet
innov_vars<- read_excel(temp_file, sheet = 1,skip=58)  # 'sheet = 1' to read the first sheet

#Federal RD&D Expenditure

fed_rdd <- innov_state %>%
  select(year,statecode,statename,publicrdd_all,publicrd_lowcarbon,publicdemo_lowcarbon,publicseed_lowcarbon,realgdp,population) %>%
  group_by(statecode,statename) %>%
  summarize(across(c(publicrdd_all,publicrd_lowcarbon,publicdemo_lowcarbon,publicseed_lowcarbon,realgdp),sum,na.rm=T)) %>%
  ungroup() %>%
  mutate(rd_share=publicrd_lowcarbon/publicrdd_all*100,
         demo_share=publicdemo_lowcarbon/publicrdd_all*100,
         seed_share=publicseed_lowcarbon/publicrdd_all*100,
         total_lowc_share=(publicrd_lowcarbon+publicdemo_lowcarbon+publicseed_lowcarbon)/publicrdd_all,
         lowc_rdd_gdp=(publicrd_lowcarbon+publicdemo_lowcarbon+publicseed_lowcarbon)/realgdp) %>%
  arrange(desc(total_lowc_share))

write.csv(fed_rdd,paste0(output_folder,"/fed_rdd.csv"))

fed_loc_rdd_plot<-ggplot(data=fed_rdd,aes(x=reorder(statecode,lowc_rdd_gdp),y=lowc_rdd_gdp,fill=lowc_rdd_gdp))+
  geom_col()+
  coord_flip()+
  scale_fill_gradient(low="#F8931D",high="#0BD0D9")+
  labs(title="Federal Low-Carbon RD&D Expenditure",
       subtitle="2016-2021 relative to state GDP",
       x="Statee",
       y="Federal Low-Carbon RD&D/GDP")+
  theme_minimal()


fed_rdd_sector <- innov_state %>%
  select(year,statecode,statename,starts_with("public")) %>%
  group_by(statecode,statename) %>%
  summarize(across(c(starts_with("public")),sum,na.rm=T)) %>%
  ungroup() %>%
  select(-ends_with("_all"),-ends_with("_lowcarbon"),-starts_with("publications")) %>%
  pivot_longer(cols=publicrd_bioenergy:publicseed_wind,names_to="Sector",values_to="publicfund") %>%
  mutate(Sector=gsub("publicrd_","",Sector),
         Sector=gsub("publicdemo_","",Sector),
         Sector=gsub("publicseed_","",Sector)) %>%
  group_by(statecode,statename,Sector) %>%
  summarize_at(vars(publicfund),sum,na.rm=T)


fed_rdd_5<-fed_rdd_sector %>%
  group_by(Sector) %>%
  slice_max(order_by=publicfund,n=5)  %>%
  group_by(Sector) %>%
  mutate(Order = reorder(statecode, -publicfund)) %>%
  ungroup()

fed_rdd_5_plot<-ggplot(data=fed_rdd_5,aes(x=Order,y=publicfund,fill=Sector))+
  geom_col()+
  facet_wrap(~Sector, scales="free")+
  scale_fill_manual(values=expanded_palette)+
  labs(title="Top 5 Public RDD States by Sector",
       subtitle="2016-2021",
       x="State",
       y="Public Funding ($)")+
  theme_classic()+
  theme(legend.position="none")

fed_rdd_state_plot <- ggplot(data=fed_rdd_sector %>% filter(statecode==state_abbreviation,
                                                            publicfund>0),aes(x=reorder(Sector,publicfund),y=publicfund,fill=publicfund))+
  geom_col()+
  coord_flip() +
  scale_fill_gradient(low="#F8931D",high="#0BD0D9")+
  labs(title=paste0("Public RD&D Funding by Low-Carbon Sector in ",state_name),
       subtitle="2016-2021",
       x="Sector",
       y="Public Funding",
       caption = "Source: ITIF, US State and Regional Energy Innovation Index")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(legend.position="none")

ggsave(paste0(output_folder,"/",state_abbreviation,"_fed_rdd_state_plot.png"),plot=fed_rdd_state_plot,width=6,height=8,units="in",dpi=300)


fed_rdd_sector_plot <- ggplot(data=fed_rdd_sector,aes(x=reorder(Sector,publicfund),y=publicfund,fill=publicfund))+
  geom_col()+
  coord_flip() +
  scale_fill_gradient(low="#F8931D",high="#0BD0D9")+
  labs(title=paste0("Public RD&D Funding by Low-Carbon Sector"),
       subtitle="2016-2021",
       x="Sector",
       y="Public Funding",
       caption = "Source: ITIF, US State and Regional Energy Innovation Index")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(legend.position="none")

#Patents

patents <- innov_state %>%
  select(year,statecode,statename,starts_with("patents")) %>%
  group_by(statecode,statename) %>%
  summarize(across(c(starts_with("patents")),sum,na.rm=T)) %>%
  ungroup() %>%
  rename_with(~ sub("patents_", "", .), starts_with("patents")) %>%
  select(-all,-lowcarbon) %>%
  pivot_longer(cols=materials:wind,names_to="Sector",values_to="patents") 

patents_5<-patents %>%
  group_by(Sector) %>%
  slice_max(order_by=patents,n=5)  %>%
  group_by(Sector) %>%
  mutate(Order = reorder(statecode, -patents)) %>%
  ungroup()

patents_5_plot<-ggplot(data=patents_5,aes(x=Order,y=patents,fill=Sector))+
  geom_col()+
  facet_wrap(~Sector, scales="free")+
  scale_fill_manual(values=expanded_palette)+
  labs(title="Top 5 Patenting States by Sector",
       subtitle="2016-2021",
       x="State",
       y="Number of Patents")+
  theme_classic()+
  theme(legend.position="none")

patents_state_plot <- ggplot(data=patents %>% filter(statecode==state_abbreviation),aes(x=reorder(Sector,patents),y=patents,fill=patents))+
  geom_col()+
  coord_flip() +
  scale_fill_gradient(low="#F8931D",high="#0BD0D9")+
  labs(title=paste0("Patents by Low-Carbon Sector in ",state_name),
       subtitle="2016-2021",
       x="Sector",
       y="Number of Patents",
       caption = "Source: ITIF, US State and Regional Energy Innovation Index")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(legend.position="none")

ggsave(paste0(output_folder,"/",state_abbreviation,"_patents_state_plot.png"),plot=patents_state_plot,width=6,height=8,units="in",dpi=300)


#VC

vc <- innov_state %>%
  select(year,statecode,statename,starts_with("vcinvestments")) %>%
  group_by(statecode,statename) %>%
  summarize(across(c(starts_with("vcinvestments")),sum,na.rm=T)) %>%
  ungroup() %>%
  rename_with(~ sub("vcinvestments_", "", .), starts_with("vcinvestments")) %>%
  select(-lowcarbon) %>%
  pivot_longer(cols=materials:wind,names_to="Sector",values_to="vcinvestments") 

vc_5<-vc %>%
  group_by(Sector) %>%
  slice_max(order_by=vcinvestments,n=5) %>%
  group_by(Sector) %>%
  mutate(Order = reorder(statecode, -vcinvestments)) %>%
  ungroup()

ggplot(data=vc,aes(x=Order,y=vcinvestments,fill=Sector))+
  geom_col()+
  facet_wrap(~Sector, scales="free")+
  scale_fill_manual(values=expanded_palette)+
  labs(title="Top 5 VC Investing States by Sector",
       subtitle="2016-2021",
       x="State",
       y="VC Investments")+
  theme_classic()+
  theme(legend.position="none")


vc_state_plot <- ggplot(data=vc %>% filter(statecode==state_abbreviation,
                                           vcinvestments != "0"),aes(x=reorder(Sector,vcinvestments),y=vcinvestments,fill=vcinvestments))+
  geom_col()+
  coord_flip() +
  scale_fill_gradient(low="#F8931D",high="#0BD0D9")+
  labs(title=paste0("VC Investment by Low-Carbon Sector in ",state_name),
       subtitle="2016-2021",
       x="Sector",
       y="VC Investment",
       caption = "Source: ITIF, US State and Regional Energy Innovation Index")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(legend.position="none")

ggsave(paste0(output_folder,"/",state_abbreviation,"_vc_state_plot.png"),plot=vc_state_plot,width=6,height=8,units="in",dpi=300)
