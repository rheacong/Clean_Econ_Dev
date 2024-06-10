#GDP R Script

# State Variable - Set this to the abbreviation of the state you want to analyze
state_abbreviation <- "NM"  # Replace with any US state abbreviation
state_name <- "New Mexico"  # Replace with the full name of any US state

#Set the Working Directory to your Username
setwd("C:/Users/LCarey.RMI/")

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
  mutate(gdp_growth_1722 = (X2022 - X2017) / X2017 * 100) 

#Filter for 'All industry total'
gdp_state_total<-gdp_ind %>%
  filter(Description=="All industry total ")

#GDP Growth Map
us_states<-usmap::us_map(regions = "states")
state_map_data <- left_join(us_states, gdp_state_total, by = c("full" = "GeoName"))

state_labels<-centroid_labels(regions = c("states"))
state_labels <- state_labels %>%
  left_join(gdp_state_total,by=c("full"="GeoName"))

gdp_state_total_map<-ggplot() +
  geom_polygon(data = state_map_data, aes(x=x,y=y,group=group, fill = gdp_growth_1722), color = "white") +
  geom_text(data = state_labels, aes(x = x, y = y, label = abbr), size = 2, color = "black", fontface = "bold") +
  scale_fill_gradient2(low="#F8931D",mid="white",high="#0989B1", midpoint=0, na.value = "grey90", name = "GDP Growth") +
  labs(title = "Economic Growth by State", 
       subtitle = "2017-2023",
       fill = "% Growth",
       caption="Source: Bureau of Economic Analysis") +
  theme(legend.position=c(0.9,0.1))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "white"),  # Sets plot background to white
        panel.background = element_rect(fill = "white", color = "white"))  # Sets panel background to white


#Filter to 2 or 3-digit NAICS Level
gdp_ind2 <- gdp_ind %>%
  filter(str_detect(IndustryClassification, "^\\d{2}$")|IndustryClassification=="31-33")%>%
  filter(IndustryClassification!="92")


gdp_ind3<-gdp_ind %>%
  filter(!IndustryClassification %in% c("...","11","21","31-33","311-316,322-326",
                                        "321,327-339","48-49","54,55,56",
                                        "61,62","71,72","51","52","53","56","62","71","72"),
         LineCode<83) 

# Step 3: Calculate proportions
total_gdp_by_year <- gdp_ind3 %>%
  mutate(across(all_of(year_cols), ~ as.numeric(gsub(",", "", .)))) %>%
  group_by(GeoName) %>%
  summarize_at(vars(X2022),sum,na.rm=T) 

gdp_proportions <- gdp_ind3 %>%
  left_join(total_gdp_by_year %>% select(GeoName,X2022), by = "GeoName", suffix = c("", "_total")) %>%
  select(GeoName,Description, X2022,X2022_total) %>%
  mutate(gdp_share=X2022/X2022_total)

# National-level proportions
national_proportions <- gdp_proportions %>%
  filter(GeoName == "United States") %>%
  select(Description, X2022, gdp_share_national = gdp_share)
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
  group_by(GeoName) %>%
  summarize(HI = 100 / sum(weighted_LQ, na.rm = TRUE), .groups = 'drop') %>% # Reciprocal of the sum of weighted LQs scaled by 100
  mutate(HI_bin = cut(HI, breaks = c(0, 50,60,70,80,90, 100), labels = c("Very Low (0-50)","Low (50-60)", "Low-Medium (60-70)","Medium-High (70-80)", "High (80-90)", "Very High (90-100)")))
# Plotting the US state map with Hachman Index

state_map_data <- left_join(us_states, hachman_indices, by = c("full" = "GeoName"))

state_labels<-centroid_labels(regions = c("states"))
state_labels <- state_labels %>%
  left_join(hachman_indices,by=c("full"="GeoName"))

hachman_map<-ggplot() +
  geom_polygon(data = state_map_data, aes(x=x,y=y,group=group,fill = HI_bin), color = "white") +
  geom_text(data = state_labels, aes(x = x, y = y, label = abbr), size = 2, color = "black", fontface = "bold") +
  scale_fill_manual(values = rmi_palette, na.value = "grey90", name = "Investment Share Relative to National Average") +
  labs(title = "Economic Diversity by State", 
       subtitle = "A Hachman Index score ranges from 0 to 100. A higher score indicates that the subject area's industrial distribution more closely resembles that of the US as a whole, and is therefore diverse.",
       fill = "Hachman Index",
       caption="Source: Bureau of Economic Analysis") +
  theme(legend.position=c(0.9,0.1))+
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "white"),  # Sets plot background to white
        panel.background = element_rect(fill = "white", color = "white"))  # Sets panel background to white

ggsave(file.path(output_folder, paste0("hachman_map", ".png")), 
       plot = hachman_map,
       width = 8,   # Width of the plot in inches
       height = 8,   # Height of the plot in inches
       dpi = 300)