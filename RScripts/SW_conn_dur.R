#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: SW Connectivity Duration 
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 5/24/2021
#Purpose: Estimate duration of surface water connectivity accross DMV wetlands
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls(all=TRUE))

#Download packages 
library(segmented)
library(lubridate)
library(tidyverse)

#Read data
df<-read_csv('data/waterLevel.csv') %>% 
  #Select data from shallow wells
  filter(str_detect(Site_Name, "Shallow")) %>% 
  #Define WetID
  mutate(WetID = str_remove(Site_Name, " Wetland Well Shallow")) %>% 
  select(-Site_Name)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Estimate spill point ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create list of sites
sites<-df %>% select(WetID) %>% unique() %>% as_vector()

#Create function to estimate duration 
fun<-function(n){
  
  #Aquire wetland name
  WetID <- sites[n] 

  #Isolate time series
  ts<-df %>% 
    filter(WetID == sites[n]) %>% 
    select(Timestamp, waterLevel) %>% 
    filter(waterLevel>0) %>% 
    drop_na() %>% 
    arrange(Timestamp)

  #Limit to recession days
  ts<-ts %>% 
    mutate(dwL = waterLevel - lag(waterLevel)) %>% 
    filter(dwL<0) %>% 
    filter(waterLevel>quantile(waterLevel, 0.50))
  
  #Create Segmented Model and estimate spill elevation
  lin_mod <- lm(dwL~waterLevel,data=ts)
  segmented_mod <- segmented(lin_mod, seg.Z = ~waterLevel, psi=quantile(ts$waterLevel, 0.75))
  spill<-segmented_mod$psi[2]
  
  #Export
  tibble(WetID, spill)
}

#Apply function
spill<-lapply(FUN = fun, X = seq(1, length(sites))) %>% bind_rows()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Estimate duration by water year -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate duration of connectivyt by water year
output<-df %>% 
  #join spill dataframe
  left_join(., spill) %>% 
  #Determine water year
  mutate(
    month = month(Timestamp), 
    year = year(Timestamp), 
    waterYear = if_else(month>=10, year + 1, year)) %>%
  #Determine if connected
  mutate(con = if_else(waterLevel>spill, 1, 0)) %>% 
  #Summarise by wetland and water year
  group_by(WetID, waterYear) %>% 
  summarise(
    days = n(), 
    connected = sum(con, na.rm=T)
  ) %>% 
  filter(days>300) %>% 
  arrange(WetID, waterYear)

#Export results
write_csv(output, "docs/duration.csv")

# -----------------------------------------------------------------------------
#Plot for funzies
sites
n<-14
df %>% 
  filter(WetID == sites[n]) %>% 
  ggplot(aes(Timestamp, waterLevel)) + 
    geom_line() +
    geom_hline(yintercept = spill$spill[spill$WetID==sites[n]]) +
    theme_bw() +
    ggtitle(paste0(sites[n], " Wetland"))
