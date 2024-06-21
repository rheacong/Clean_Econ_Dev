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