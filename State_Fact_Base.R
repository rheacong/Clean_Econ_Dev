#State Fact Base

# Load libraries
library(lubridate)
library(tidyverse)
library(ggplot2)
library(zoo)
library(usmapdata)
library(readxl)
library(jsonlite)
library(httr)
library(stringr)
library(readxl)
library(sf)
library(tigris)
library(censusapi)
library(dataverse)
library(cspp)


#From Clean Growth Tool Dataset
feas <- read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/feasibility_complete_dataset.csv')


#Climate Policy Index
xchange_pol_index<-read.csv("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/US Maps etc/Policy/xchange_climate_policy_index.csv")

xchange_pol_index<-xchange_pol_index %>%
  select(abbr, climate_policy_index)

#Renewables Share 2022
seds_all <- read.csv('https://www.eia.gov/state/seds/sep_update/Complete_SEDS_update.csv') #NB: BIG file

ren_elec_state_22<-seds_all %>%
  select(Year,StateCode,MSN, Data) %>%
  filter(Year %in% c("2022"),
         MSN %in% c("SOTCB",
                    "WYEGB",
                    "ESTCB")) %>%
  pivot_wider(names_from=MSN,values_from=Data) %>%
  mutate(ren=SOTCB+WYEGB,
         ren_share_22=round(ren/ESTCB*100,1)) %>%
  select(StateCode,ren_share_22)

#State Emissions Change 2016-2021
url<-'https://www.eia.gov/environment/emissions/state/excel/table1.xlsx'
temp_file <- tempfile(fileext = ".xlsx")
GET(url = url, write_disk(temp_file, overwrite = TRUE))
state_ems <- read_excel(temp_file, sheet = 1,skip=4)

state_ems<-state_ems %>%
  mutate(state_ems_change_1621 = round((`2021`-`2016`)/`2016`*100,3)) %>%
  select(State,state_ems_change_1621)

#State Effective Tax Rate
#from here: https://taxfoundation.org/data/all/state/tax-burden-by-state-2022/ 
tax<-read.csv("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/Tax_burden_tax_foundation.csv")
colnames(tax)[1]<-"State"
tax<-tax %>% 
  select(-Rank) %>%
  rename(State_Effective_Tax_Rate=Effective.Tax.Rate)

#Right to Work State

#Right to Work
righttowork<-read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/right_to_work.csv')
colnames(righttowork)[1]<-"State"
righttowork <- righttowork %>%
  mutate(Right_to_work = "Yes") %>%
  select(State,Right_to_work)

#Electricity Price in Industrial Sector
seds_elec_pric_ind <- seds_all %>%
  filter(MSN %in% c("ESICD") & Year==2022) %>%
  select(StateCode,Data) %>%
  rename(ind_elec_price = Data) 

#manufacturing Employment & Pay
library(censusapi)
#https://github.com/hrecht/censusapi
# Add key to .Renviron
Sys.setenv(CENSUS_KEY='0b3d37ac56ab19c5a65cbc188f82d8ce5b36cfe6')
state_man_2021 <- getCensus(
  name = "cbp",
  vars=c("STATE",
         "NAICS2017",
         "INDLEVEL",
         "PAYANN",
         "EMP"),
  region = "state:*",
  vintage = 2021)

state_manshare <- state_man_2021 %>%
  filter(NAICS2017 %in% c("31-33","00")) %>%
  select(STATE,NAICS2017,EMP) %>%
  pivot_wider(names_from=NAICS2017,values_from=EMP) %>%
  mutate(man_share=`31-33`/`00`*100) 

state_manpay <- state_man_2021 %>%
  filter(NAICS2017 %in% c("31-33")) %>%
  mutate(worker_pay=PAYANN/EMP*1000)

#GDP Growth
state_gdp<- read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/state_gdp_22.csv',skip=3)
state_gdp_17_22 <- state_gdp %>%
  mutate(gdp_17_22 = round((X2022-X2017)/X2017*100,1)) %>%
  select(GeoName,gdp_17_22)

#CNBC Business rankings
cnbc <- read.csv("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/cnbc_bus_rankings.csv")
colnames(cnbc)[1]<-"cnbc_rank"
colnames(cnbc)[2]<-"state"
cnbc<-cnbc %>%
  select(state,cnbc_rank)

#Politics - National

pres <- read.csv("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/US Maps etc/Politics/Presidential_County/countypres_2000-2020.csv")

pres_2020_state<-pres %>%
  spread(party,candidatevotes) %>%  
  group_by(year,state,state_po,office) %>%
  summarize_at(vars(DEMOCRAT,REPUBLICAN),sum,na.rm=T) %>%
  mutate(demshare=DEMOCRAT/(DEMOCRAT+REPUBLICAN)) %>%
  filter(year==2020) %>%
  mutate(partisan = case_when(demshare<0.4 ~ 1,
                              demshare<0.475~2,
                              demshare<0.5252~3,
                              demshare<0.6~4,
                              demshare>0.6~5)) %>%
  mutate(partisan = factor(partisan,
                           levels=c(1:5),
                           labels=c("Strong Republican",
                                    "Leans Republican",
                                    "Battleground",
                                    "Lean Democratic",
                                    "Strong Democratic"))) %>%
  ungroup()

state_leg<-read.csv("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/state_leg_apr_24.csv")
state_leg<-state_leg %>%
  mutate(across(2:9, as.numeric)) %>%
  mutate(demshare_statesenate=Senate.Dem./Total.Senate,
         demshare_statehouse=House.Dem./Total.House,
         dem_governor = ifelse(Gov..Party=="Dem",1,0),
         dem_statecontrol = ifelse(State.Control=="Dem",1,ifelse(State.Control=="Rep",0,0.5))) %>%
  select(STATE,demshare_statesenate,demshare_statehouse,dem_governor,dem_statecontrol)


#Political Ideology
cspp_poli <- get_cspp_data(var_category = "policy-ideology") %>%
  filter(year>=2010) %>%
  select( where(~sum(!is.na(.x)) > 0)) %>%
  select(c("st","state","liberal","pro_environment","pro_welfare","pro_race","LCVavg")) %>%
  drop_na()

state_vars <- left_join(state_vars,cspp_poli,by=c("State"="st","full"="state"))

#Incentives
gjf<- read.csv("RMI/US Program - Regional Investment Strategies/Great Lakes Investment Strategy/Great Lakes Overview/Econ Development/gjf_complete.csv")

gjf_statetotal_19_22<-gjf %>%
  filter(Year>2019) %>%
  group_by(region,Location) %>%
  summarize_at(vars(subs_m),sum,na.rm=T) %>%
  arrange(desc(subs_m)) %>%
  ungroup() %>%
  inner_join(state_gdp, by=c("Location"="GeoName")) %>%
  mutate(incent_gdp = subs_m/X2022)


#Corruption
corrupt<- read.csv("C:/Users/LCarey.RMI/Downloads/state_corruption.csv")
#source = file:///C:/Users/LCarey.RMI/Downloads/The_Impact_of_Public_Officials_Corruptio.pdf
corrupt<-corrupt %>%
  rename(corruption_rank=`ï..Rank`)

state_vars <- left_join(state_vars,corrupt,by=c("full"="State"))

#Fossil Fuel Workers
fossil_codes <- tibble(
  NAICS_code = c(211, 2121, 213111, 213112, 213113, 32411, 4861, 4862),
  Description = c("Oil and Gas Extraction",
                  "Coal Mining",
                  "Drilling Oil and Gas Wells",
                  "Support Activities for Oil and Gas Operations",
                  "Support Activities for Coal Mining",
                  "Petroleum Refineries",
                  "Pipeline Transportation of Crude Oil",
                  "Pipeline Transportation of Natural Gas"))
total_emp_nat<-state_man_2021  %>%
  filter(INDLEVEL=="2") %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  ungroup() 


fossil_emp_national <- state_man_2021 %>%
  mutate(fossil = ifelse(NAICS2017 %in% fossil_codes$NAICS_code,1,0)) %>%
  group_by(fossil) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  mutate(emp_share_national = EMP / sum(EMP))

fossil_emp_state <- state_man_2021 %>%
  mutate(fossil = ifelse(NAICS2017 %in% fossil_codes$NAICS_code,1,0)) %>%
  group_by(STATE,fossil) %>%
  summarize_at(vars(EMP),sum,na.rm=T) %>%
  mutate(emp_share = EMP / sum(EMP)) %>%
  left_join(fossil_emp_national, by = c("fossil" = "fossil")) %>%
  mutate(lq=emp_share/emp_share_national)

#Energy Import/Export


#Fiscal Capacity
state_other_1722 <- read.csv("OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/state_other_incent_1722.csv")

state_other_1722<-state_other_1722 %>%
  select(abbr,other_total,total_gdp) %>%
  rename("other_spend_budget"=other_total,
         "budget_gdp"=total_gdp) 


