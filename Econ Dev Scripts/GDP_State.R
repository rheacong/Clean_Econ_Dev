#GDP R Script

# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "NM"  # Replace with any US state abbreviation
state_name <- "New Mexico"  # Replace with the full name of any US state
region_abbrv <- states_simple %>% filter(abbr == state_abbreviation)

#Set the Working Directory to your Username and update output folder for saved charts etc
setwd("C:/Users/LCarey.RMI/")
output_folder <- paste0("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Slide Decks/States/",state_abbreviation)

#GDP by Industry
url <- "https://apps.bea.gov/regional/zip/SAGDP.zip"
temp_zip <- tempfile(fileext = ".zip")
download(url, temp_zip, mode = "wb")
temp_dir <- tempdir()
unzip(temp_zip, exdir = temp_dir)
files <- list.files(temp_dir, full.names = TRUE)

gdp_ind <- read.csv(files[grepl("SAGDP9N__ALL_AREAS_1997_2023.csv", files)], stringsAsFactors = FALSE)

#Make relevant columns numeric and add GDP growth variable
years <- 1997:2023
year_cols <- paste0("X", years)
gdp_ind <- gdp_ind %>%
  mutate(across(all_of(year_cols), ~ as.numeric(gsub(",", "", .)))) %>%
  mutate(gdp_growth_1722 = (X2022 - X2017) / X2017 * 100,
         gdp_growth_1823 = (X2023 - X2018) / X2018 * 100) 

#Filter for 'All industry total'
gdp_state_total<-gdp_ind %>%
  filter(Description=="All industry total ")%>%
  mutate(fips=as.numeric(GeoFIPS)) %>%
  filter(fips<60000)

#GDP Growth Map
us_states<-usmap::us_map(regions = "states")
state_map_data <- left_join(us_states, gdp_state_total, by = c("full" = "GeoName"))

state_labels<-centroid_labels(regions = c("states"))
state_labels <- state_labels %>%
  left_join(gdp_state_total,by=c("full"="GeoName"))

gdp_state_total_map<-ggplot() +
  geom_polygon(data = state_map_data, aes(x=x,y=y,group=group, fill = gdp_growth_1823), color = "white") +
  geom_text(data = state_labels, aes(x = x, y = y, label = abbr), size = 2, color = "black", fontface = "bold") +
  scale_fill_gradient2(low="#F8931D",mid="white",high="#0989B1", midpoint=mean(gdp_state_total$gdp_growth_1823), na.value = "grey90", name = "% Growth") +
  labs(title = "Economic Growth by State", 
       subtitle = "Percentage Growth, 2018-2023",
       fill = "% Growth",
       caption="Source: Bureau of Economic Analysis") +
  theme(legend.position=c(0.9,0.1))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "white"),  # Sets plot background to white
        panel.background = element_rect(fill = "white", color = "white"))  # Sets panel background to white
ggsave(file.path(output_folder, paste0(state_abbreviation,"_gdp_state_total_map", ".png")), 
       plot = gdp_state_total_map,width=8,height=6,units="in",dpi=300)

#Filter to 2 or 3-digit NAICS Level
gdp_ind2 <- gdp_ind %>%
  filter(str_detect(IndustryClassification, "^\\d{2}$")|IndustryClassification=="31-33")%>%
  filter(IndustryClassification!="92")


gdp_ind3<-gdp_ind %>%
  filter(!IndustryClassification %in% c("...","11","21","31-33","311-316,322-326",
                                        "321,327-339","48-49","54,55,56",
                                        "61,62","71,72","51","52","53","56","62","71","72"),
         LineCode<83) 

#2-Digit NAICS: Calculate State and Local Shares for LQ Calculations
total_gdp_by_year <- gdp_ind2 %>%
  mutate(across(all_of(year_cols), ~ as.numeric(gsub(",", "", .)))) %>%
  group_by(GeoName) %>%
  summarize_at(vars(X2023),sum,na.rm=T) 

gdp_proportions <- gdp_ind2 %>%
  left_join(total_gdp_by_year %>% select(GeoName,X2023), by = "GeoName", suffix = c("", "_total")) %>%
  select(GeoName,Description, X2023,X2023_total) %>%
  mutate(gdp_share=X2023/X2023_total)

# National-level proportions
national_proportions <- gdp_proportions %>%
  filter(GeoName == "United States") %>%
  select(Description, X2023, gdp_share_national = gdp_share)
# State-level data, exclude national totals
state_proportions <- gdp_proportions %>%
  filter(GeoName != "United States")

#Location Quotients
location_quotients <- state_proportions %>%
  left_join(national_proportions, by = "Description") %>%
  mutate(LQ = gdp_share / gdp_share_national,
         weighted_LQ = LQ * gdp_share)  # Weighting by regional share

# Compute the Hachman index
hachman_indices <- location_quotients %>%
  inner_join(states_simple,by=c("GeoName"="full")) %>%
  mutate(sqrt_product = sqrt(gdp_share * gdp_share_national)) %>%
  group_by(GeoName) %>%
  summarize(HI = 100*sum(sqrt_product,na.rm=T)^2, .groups = 'drop') %>% # Reciprocal of the sum of weighted LQs scaled by 100
  mutate(HI_bin = cut(HI, breaks = c(0, 80,90,92,94,96,98, 100), labels = c("Very Low (0-80)","Low (80-90)", "Medium (90-92)","Medium-High (92-94)", "High (94-96)", "Very High (96-98)","Highest (98-100)")))

# Plotting the US state map with Hachman Index
state_map_data <- left_join(us_states, hachman_indices, by = c("full" = "GeoName"))

state_labels<-centroid_labels(regions = c("states"))
state_labels <- state_labels %>%
  left_join(hachman_indices,by=c("full"="GeoName"))

hachman_map<-ggplot() +
  geom_polygon(data = state_map_data, aes(x=x,y=y,group=group,fill = HI_bin), color = "white") +
  geom_text(data = state_labels, aes(x = x, y = y, label = abbr), size = 2, color = "black", fontface = "bold") +
  scale_fill_manual(values = rev(rmi_palette), na.value = "grey90", name = "Investment Share Relative to National Average") +
  labs(title = "Economic Diversity by State", 
       subtitle = "A Hachman Index score ranges from 0 to 100. A higher score indicates that the subject area's industrial distribution more closely resembles that of the US as a whole, and is therefore diverse.",
       fill = "Hachman Index",
       caption="Source: Bureau of Economic Analysis") +
  theme(legend.position=c(0.9,0.1))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "white"),  # Sets plot background to white
        panel.background = element_rect(fill = "white", color = "white"))  # Sets panel background to white


ggsave(file.path(output_folder, paste0(state_abbreviation,"_hachman_map", ".png")), 
       plot = hachman_map,width=8,height=6,units="in",dpi=300)

#State Location Quotient
state_lq<-location_quotients %>%
  filter(GeoName==state_name) %>%
  select(Description,LQ) %>%
  arrange(desc(LQ))%>%
  left_join(gdp_ind %>% 
              filter(GeoName==state_name) %>% 
              select(GeoName,Description,X2023,gdp_growth_1823),by=c("Description"="Description")) %>%
  mutate(label=case_when(
    gdp_growth_1823>weighted.mean(gdp_growth_1823,w=X2023) & LQ >1 ~ "High Growth/High Specialization",
    gdp_growth_1823<weighted.mean(gdp_growth_1823,w=X2023)  & LQ >1 ~ "Low Growth/High Specialization",
    gdp_growth_1823>weighted.mean(gdp_growth_1823,w=X2023)  & LQ <1 ~ "High Growth/Low Specialization",
    gdp_growth_1823<weighted.mean(gdp_growth_1823,w=X2023) & LQ <1 ~ "Low Growth/Low Specialization"))


#Chart: State Location Quotient
state_lq_plot<-ggplot(data=state_lq,aes(x=LQ,y=gdp_growth_1823,size=X2023,color=label)) +
  geom_point(aes(fill=label),shape=21,color="black",stroke=0.5,alpha=0.75) +
  geom_text_repel(aes(label = ifelse(LQ>1,paste(Description,"= ", round(gdp_growth_1823,0),"% growth."),"")), 
                  box.padding = 0.5, 
                  point.padding = 0.3, 
                  segment.color = 'grey',
                  size=2,
                  color='black') +
  labs(title=paste("Growth and Specialization in ",state_name), 
       y="GDP Growth (18-23)", x="Location Quotient",
       caption="Source: BEA") +
  geom_vline(xintercept = 1,color='darkgrey') +
  geom_hline(yintercept= weighted.mean(state_lq$gdp_growth_1823,w=state_lq$X2023) ,color='darkgrey') +
  theme_classic()+
  scale_fill_manual(values = rmi_palette)+
  scale_size(range = c(3, 20)) +  # Controlling the size of the bubbles
  theme(legend.position="none")

ggsave(file.path(output_folder, paste0(state_abbreviation,"_state_lq_plot", ".png")), 
       plot = state_lq_plot,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)


#3-Digit NAICS: Calculate State and Local Shares for LQ Calculations
total_gdp_by_year_3 <- gdp_ind3 %>%
  mutate(across(all_of(year_cols), ~ as.numeric(gsub(",", "", .)))) %>%
  group_by(GeoName) %>%
  summarize_at(vars(X2022),sum,na.rm=T) 

gdp_proportions_3 <- gdp_ind3 %>%
  left_join(total_gdp_by_year_3 %>% select(GeoName,X2022), by = "GeoName", suffix = c("", "_total")) %>%
  select(GeoName,Description, X2022,X2022_total) %>%
  mutate(gdp_share=X2022/X2022_total)

# National-level proportions
national_proportions_3 <- gdp_proportions_3 %>%
  filter(GeoName == "United States") %>%
  select(Description, X2022, gdp_share_national = gdp_share)
# State-level data, exclude national totals
state_proportions_3 <- gdp_proportions_3 %>%
  filter(GeoName != "United States")

#Location Quotients
location_quotients_3 <- state_proportions_3 %>%
  left_join(national_proportions_3, by = "Description") %>%
  mutate(LQ = gdp_share / gdp_share_national,
         weighted_LQ = LQ * gdp_share)  # Weighting by regional share

state_lq_3<-location_quotients_3 %>%
  filter(GeoName==state_name) %>%
  select(Description,LQ) %>%
  arrange(desc(LQ))%>%
  left_join(gdp_ind %>% 
              filter(GeoName==state_name) %>% 
              select(GeoName,Description,X2022,gdp_growth_1722),by=c("Description"="Description")) %>%
  mutate(label=case_when(
    gdp_growth_1722>weighted.mean(gdp_growth_1722,w=X2022) & LQ >1 ~ "High Growth/High Specialization",
    gdp_growth_1722<weighted.mean(gdp_growth_1722,w=X2022)  & LQ >1 ~ "Low Growth/High Specialization",
    gdp_growth_1722>weighted.mean(gdp_growth_1722,w=X2022)  & LQ <1 ~ "High Growth/Low Specialization",
    gdp_growth_1722<weighted.mean(gdp_growth_1722,w=X2022) & LQ <1 ~ "Low Growth/Low Specialization"))

state_quadrant <- state_lq_3 %>%
  group_by(label) %>%
  summarize_at(vars(X2022),sum,na.rm=T) %>%
  mutate(share=X2022/sum(X2022))

#Treemap
library(treemap)

# Specify the output path and filename
output_file <- file.path(output_folder, paste0(state_abbreviation, "_gdp_treemap.png"))

# Open a PNG device
png(filename = output_file, width = 8, height = 8, units = 'in', res = 300)

# Create the treemap
treemap(
  state_lq_3,
  index = c("label", "Description"),
  vSize = "X2022",
  vColor="label",
  palette = rmi_palette,
  title = paste0(state_name,"'s Economy by Industry Growth and Specialization"),
  #caption = "Source: Bureau of Economic Analysis",
  fontsize.title = 16,
  fontsize.labels = 12,
  align.labels = list(
    c("left", "top"),
    c("center", "center"),
    fontface.labels = list(
      2,  # Bold text for better visibility
      1
    ),
    
    fontsize.labels = list(
      12,  # Bold text for better visibility
      10
    )
  )
)

# Close the device
dev.off()


#For Clean Energy Industries
clean_gdpind<-gdp_ind %>%
  left_join(eti_long %>% 
              select(`3-Digit Code`,`Sector`) %>%
              mutate(naics3=as.character(`3-Digit Code`)),by=c("IndustryClassification"="naics3")) %>%
  left_join(eti_long %>% 
              select(`2-Digit Code`,`Sector`) %>%
              mutate(naics2=as.character(`2-Digit Code`)),by=c("IndustryClassification"="naics2")) %>%
  mutate(sector=ifelse(is.na(`Sector.x`),`Sector.y`,`Sector.x`)) %>%
  filter(!is.na(sector)) %>%
  distinct(GeoName,IndustryClassification,Description,X2023,gdp_growth_1722,sector)

state_lq_clean <-state_lq %>%
  filter(Description %in% clean_gdpind$Description) 
state_lq_dirty<-state_lq %>%
  filter(!(Description %in% clean_gdpind$Description))

state_lq_clean_plot<-ggplot() +
  geom_point(data=state_lq_dirty,aes(x=LQ,y=gdp_growth_1722,size=X2023),color='grey',alpha=0.5) +
  geom_point(data=state_lq_clean,aes(x=LQ,y=gdp_growth_1722,size=X2023,fill=label),shape=21,color="black",stroke=0.5,alpha=0.75) +
  geom_text_repel(data=state_lq_clean,aes(x=LQ,y=gdp_growth_1722,label = paste(Description,"= ", round(gdp_growth_1722,0),"% growth.")), 
                  box.padding = 0.2, 
                  point.padding = 0.2, 
                  segment.color = 'grey',
                  size=3.5,
                  color='black') +
  labs(title=paste("Growth and Specialization in ",state_name," in clean energy-related industries."),
       subtitle="Clean energy-related industries are defined as those with the potential to participate in the energy transition technology supply chain.",
       y="GDP Growth (17-22)", x="Location Quotient",
       caption="Source: BEA, Clean Growth Tool") +
  geom_vline(xintercept = 1,color='darkgrey') +
  geom_hline(yintercept= weighted.mean(state_lq$gdp_growth_1722,w=state_lq$X2023) ,color='darkgrey') +
  theme_classic()+
  scale_fill_manual(values = rmi_palette)+
  scale_size(range = c(3, 20)) +  # Controlling the size of the bubbles
  theme(legend.position="none")

ggsave(file.path(output_folder, paste0(state_abbreviation,"_state_lq_clean_plot", ".png")), 
       plot = state_lq_clean_plot,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)

#Clean Location Quotients
gdp_proportions_clean <- clean_gdpind %>%
  group_by(GeoName,sector) %>%
  summarize_at(vars(X2023),sum,na.rm=T) %>%
  left_join(total_gdp_by_year %>% select(GeoName,X2023), by = "GeoName", suffix = c("", "_total")) %>%
  mutate(gdp_share=X2023/X2023_total)

# National-level proportions
national_proportions <- gdp_proportions_clean %>%
  filter(GeoName == "United States") %>%
  select(sector, X2023, gdp_share_national = gdp_share)
# State-level data, exclude national totals
state_proportions <- gdp_proportions_clean %>%
  filter(GeoName != "United States")

#Location Quotients
location_quotients <- state_proportions %>%
  left_join(national_proportions, by = "sector") %>%
  mutate(LQ = gdp_share / gdp_share_national,
         weighted_LQ = LQ * gdp_share)  # Weighting by regional share

# Prepare the data: Filter for top 5 sectors by LQ for each GeoName
top_sectors <- location_quotients %>%
  left_join(states_simple,by=c("GeoName.x"="full")) %>%
  filter(region %in% region_abbrv$region) %>%
  group_by(region,GeoName.x,sector) %>%
  summarize_at(vars(LQ), mean, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(state=ifelse(GeoName.x==state_name,1,0)) %>%
  arrange(desc(LQ)) 

# Create the plot
top_sectors_plot<-ggplot(top_sectors, aes(x = reorder(sector, LQ), y = LQ, fill = (GeoName.x))) +
  geom_bar(stat = "identity", position = position_dodge(),
           aes(alpha = ifelse(state == "0", 0.4, 1))) +
  coord_flip() +  # Flip coordinates to make horizontal bars
  scale_fill_manual(values = expanded_palette, 
                    name = paste(str_to_sentence(top_sectors$region), " States")) +
  scale_alpha_identity() +  # Ensure alpha is interpreted as given
  labs(title = paste("Transition Sector Specializations in ", state_name),
       x = "Transition Sector",
       y = "Location Quotient (LQ)",
       subtitle = "Average Location Quotient in Transition Sector Categories") +
  theme_minimal() +
  geom_hline(yintercept=1,color='darkgrey')+
  geom_text(aes(x = 0, y = 1), 
            label = "National Average", hjust = 0.5, vjust = -0.25)+
  theme(legend.position = c(0.8,0.25),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))

