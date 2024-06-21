#Industry


#Direct Emitters
emit<-read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/States Data/direct_emitters.csv')

#GGplot Map of Direct Emitters by industry in state_abbreviation
state_emit_map <- emit %>%
  mutate(direct_emit=as.numeric(gsub(",","",Total.reported.direct.emissions))) %>%
  filter(State==state_abbreviation) %>%
  select(Facility.Name,Latitude,Longitude,Industry.Type,direct_emit) 

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
state_map_data<-usa %>%
  filter(ID==str_to_lower(state_name))
state_sf<-st_as_sf(state_map_data,coords=c("x","y"),crs=3857)

bbox<-st_bbox(state_sf)
bbox_state <- c(left = bbox["xmin"],
                bottom = bbox["ymin"],
                right = bbox["xmax"],
                top = bbox["ymax"])
names(bbox_state) <- c("left", "bottom", "right", "top")


base_map <- get_map(location = bbox_state, maptype = "roadmap", source = "google",zoom=6)

# Plot the bubble map
ggmap(base_map, darken = c(0.5, "white")) +
  
  coord_sf(xlim = c(bbox_state["left"], bbox_state["right"]),
           ylim = c(bbox_state["bottom"], bbox_state["top"]),
           crs=3857) +
  # State boundaries
  # Location bubbles
  geom_point(data = state_emit_map, aes(x=Longitude,y=Latitude,size = direct_emit, color = Industry.Type), 
             alpha = 0.6, inherit.aes = FALSE) +
  # Customize scales
  scale_size_continuous(range = c(1, 15), guide = "none", name = "Variable Name") +
  scale_color_manual(values = expanded_palette, name="Industry Type",guide = guide_legend(override.aes = list(size = 5))) +
  # Additional settings
  labs(title = paste0("Direct CO2 emitting facilities across ",state_name), 
       subtitle = "Subtitle if needed",
       caption="Source: EPA Flight") +
  #geom_sf(data = state_map_data, inherit.aes = FALSE, color = "darkgrey", fill = NA) +  # Explicitly turn off inheritance
  theme_void()+
  theme(legend.position="bottom")


#Emissions Location Quotient1
us_directemit_naics <- emit %>%
  mutate(US_direct_emit=as.numeric(gsub(",","",Total.reported.direct.emissions))) %>%
  group_by(Primary.NAICS.Code,Industry.Type) %>%
  summarize_at(vars(US_direct_emit),sum,na.rm=T) %>%
  ungroup() %>%
  mutate(US_emit_share=US_direct_emit/sum(US_direct_emit))

state_directemit_naics <- emit %>%
  mutate(direct_emit=as.numeric(gsub(",","",Total.reported.direct.emissions))) %>%
  select(State,Primary.NAICS.Code,Industry.Type,direct_emit) %>%
  group_by(State,Primary.NAICS.Code,Industry.Type) %>%
  summarize_at(vars(direct_emit),sum,na.rm=T) %>%
  group_by(State) %>%
  mutate(emit_share=direct_emit/sum(direct_emit)) %>%
  left_join(us_directemit_naics,by=c("Primary.NAICS.Code","Industry.Type")) %>%
  left_join(naics2017 %>% 
              mutate(`2017.NAICS.US.Code`=as.integer(`2017 NAICS US   Code`),
                     NAICS_name=`2017 NAICS US Title`) %>%
              select("2017.NAICS.US.Code","NAICS_name"),by=c("Primary.NAICS.Code"="2017.NAICS.US.Code")) %>%
  mutate(emit_lq = emit_share/US_emit_share) %>%
  arrange(desc(emit_lq))

us_directemit <- emit %>%
  mutate(US_direct_emit=as.numeric(gsub(",","",Total.reported.direct.emissions))) %>%
  group_by(Industry.Type) %>%
  summarize_at(vars(US_direct_emit),sum,na.rm=T) %>%
  ungroup() %>%
  mutate(US_emit_share=US_direct_emit/sum(US_direct_emit))

state_directemit <- emit %>%
  mutate(direct_emit=as.numeric(gsub(",","",Total.reported.direct.emissions))) %>%
  select(State,Industry.Type,direct_emit) %>%
  group_by(State,Industry.Type) %>%
  summarize_at(vars(direct_emit),sum,na.rm=T) %>%
  group_by(State) %>%
  mutate(emit_share=direct_emit/sum(direct_emit)) %>%
  left_join(us_directemit,by=c("Industry.Type")) %>%
  mutate(emit_lq = emit_share/US_emit_share) %>%
  arrange(desc(emit_lq))

#State Largest Direct Emitters, relative to National Average
directemit_lq_plot<-ggplot(data=state_directemit_naics %>%
         filter(State==state_abbreviation) %>%
         slice_max(order_by=direct_emit,n=25) %>%
         slice_max(order_by=emit_lq,n=10),aes(y=reorder(NAICS_name,-emit_lq),
                                              x=emit_lq,
                                              fill=Industry.Type)) +
  geom_col(position='stack') +
  labs(title=paste0("Largest Direct Emitters in ",state_name), 
       subtitle="Share of direct emissions in the state relative to the national average",
       y="", x="Direct Emissions Location Quotient") +
  scale_x_continuous(expand = c(0,0))+
  geom_vline(xintercept = 1,color='darkgrey') +
  theme_classic()+
  scale_fill_manual(values = rmi_palette)+
  theme(legend.position=c(0.9, 0.9))

ggsave(paste0(output_folder,"/",state_abbreviation,"_directemit_lq_plot.png"),plot=directemit_lq_plot,width=8,height=6,units="in",dpi=300)


#Industry Energy Intensity
asm_vars<-listCensusMetadata("timeseries/asm",
                             vintage=2021,
                             type="variables")
asm_2021 <- getCensus(
  name = "timeseries/asm/area2017",
  vars=c("STATE",
         "NAICS2017",
         "SECTOR",
         "SUBSECTOR",
         "INDLEVEL",
         "EMP",
         "VALADD",
         "YEAR",
         "RCPTOT",
         "CSTELEC",
         "CSTFU",
         "ELECPCH"),
  region = "us:*")

states<-us_map("states")

asm_21<-asm_2021 %>%
  left_join(states %>% 
              distinct(fips,abbr,full),by=c("STATE"="fips")) %>%
  left_join(naics2017 %>%
              select(`2017 NAICS US   Code`,`2017 NAICS US Title`),by=c("NAICS2017"="2017 NAICS US   Code")) %>%
  filter(INDLEVEL=="6") %>%
  mutate(across(c(EMP,VALADD,RCPTOT,CSTELEC,CSTFU,ELECPCH),as.numeric)) %>%
  mutate(energy_intensity=(CSTELEC+CSTFU)/RCPTOT,
         elec_intensity=CSTELEC/RCPTOT) 