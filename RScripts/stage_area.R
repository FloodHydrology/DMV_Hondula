#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Stage Relationships 
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 5/24/2021
#Purpose: Estimate stage dependent relationships for QB Wetland
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Semi code
# -clip dem to catchment
# -disaggregate into small subshed
# -estiamte elevation-area relationship for each subshed
# -combine for entire catchment

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls(all=TRUE))

#Download packages 
library(sf)
library(raster)
library(stars)
library(whitebox)
library(leaflet)
library(tidyverse)

#Define temp dir
dir.create(paste0(getwd(), '/data/temp'))
temp_dir<-paste0(getwd(), '/data/temp/')

#load data
dem<-raster('data/dem.tif')
shed<-st_read('docs/sheds.shp') %>% filter(sheds==2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Delineate subsheds---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#crop dem
dem_shed<-crop(dem, shed)
dem_shed<-mask(dem_shed, shed)

#write shed dem to temp
writeRaster(dem_shed, paste0(temp_dir,'dem_shed.tif'), overwrite=T)

#Define depressions with stochastic depression tool
wbt_stochastic_depression_analysis(
  dem='dem_shed.tif',
  output = 'giw_prob.tif',
  rmse = 0.18, 
  range = 10, 
  iterations = 1000,
  wd = temp_dir
)

#Reclassify wbt_stochastic_depression output -- depressions occur 80% of the time
wbt_reclass(
  input = 'giw_prob.tif', 
  output = 'giw_bianary.tif', 
  reclass_vals = "'0.0;0.0;0.99'", 
  wd = temp_dir)
wbt_reclass(
  input = 'giw_bianary.tif', 
  output = 'giw_bianary.tif', 
  wd= temp_dir,
  reclass_vals = "'1.0;0.99;1.0'")

#Create polygons of wetland shapes
giws<-raster(paste0(temp_dir, "giw_bianary.tif"))
giws[giws==0]<-NA
giws<- giws %>% st_as_stars(.) %>% st_as_sf(., merge = TRUE)

#Filter giw shapes
giws <- giws %>% 
  mutate(area_m2 = st_area(.), 
         area_m2 = paste0(area_m2), 
         area_m2 = as.double(area_m2)) %>% 
  filter(area_m2>50)

#Add unique id to each giw
giws<-giws %>% 
  mutate(uid = seq(1, nrow(.)))

#Export to workspace
st_write(giws, paste0(temp_dir, "giws.shp"), delete_layer = T)

#Estimate flowpaths using breach depression, flow accumulation, and reclass tools
wbt_breach_depressions(
  dem = 'dem_shed.tif', 
  output = 'dem_breach.tif',
  fill_pits = T, 
  wd = temp_dir)
wbt_d8_flow_accumulation(
  input = 'dem_breach.tif',
  output = 'fac.tif',
  wd = temp_dir)
wbt_reclass(
  input="fac.tif",
  output = "flowpath.tif",
  reclass_vals = "'0.0;0.0;1000'", 
  wd = temp_dir, 
)
wbt_reclass(
  input="flowpath.tif",
  output = "flowpath.tif",
  reclass_vals = "'1.0;1000;50000'", 
  wd = temp_dir, 
)

#Estimate pour points for each subshed based on highest fac value along flowpath
#contained within individual giws
wbt_vector_polygons_to_raster(
  input = 'giws.shp',
  output = 'giws.tif',
  field = 'uid',
  base = 'dem_shed.tif',
  wd = temp_dir)
wbt_raster_to_vector_points(
  input = 'flowpath.tif',
  output = 'fp_pnts.shp',
  wd = temp_dir)
fac<-raster(paste0(temp_dir,"fac.tif"))
giws<-raster(paste0(temp_dir,'giws.tif'))
pnts<-st_read(paste0(temp_dir,"fp_pnts.shp")) %>% 
  mutate(
    fac = raster::extract(fac, pnts), 
    giws = raster::extract(giws, pnts)) 
pp<-pnts %>% 
  st_drop_geometry() %>% 
  drop_na() %>% 
  group_by(giws) %>% 
  summarise(fac = max(fac)) %>% 
  mutate(keep=1)
pp<-left_join(pnts, pp) %>% drop_na()

#Write pour points to temp
st_write(pp, paste0(temp_dir,"pp.shp"))

#delineate subsheds
wbt_d8_pointer(
  dem = 'dem_breach.tif',
  output = 'fdr.tif',
  wd = temp_dir)
wbt_watershed(
  d8_pntr = 'fdr.tif',
  pour_pts = 'pp.shp',
  output = 'sheds.tif',
  wd = temp_dir
)

#extract polygons
wbt_raster_to_vector_polygons(
  input='sheds.tif',
  output = 'sheds.shp',
  wd = temp_dir
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Delineate subsheds---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Load subsheds into R env
sheds<-st_read(paste0(temp_dir,'sheds.shp'))

#Create function to estimate stage volume by subshed
subshed_inundate<-function(n){

  #Isolate subshed of interest
  shed_temp<-sheds %>% filter(FID == n)
  
  #mask dem
  dem_temp<-mask(dem_shed, shed_temp)
  
  #Create function to return conditional raster
  inner_fun<-function(ele){
    #Conditional statement
    con<-function(condition, trueValue, falseValue){
      return(condition * trueValue + (!condition)*falseValue)
    }
    
    #inundation raster
    wet<-con(dem_temp<=ele,1, 0)
    
    #Export total area
    tibble(
      FID = n,
      ele_m = ele,
      area_m2 = cellStats(wet, sum))
  }
  
  #Create vector of inundation elevations
  z<-seq(round(minValue(dem_shed), 1), round(maxValue(dem_shed), 1), by = 0.1)
  z<-z[z>=minValue(dem_temp)]
  
  #Estimate inundation extent at each elevation
  lapply(X=z, FUN = inner_fun) %>% bind_rows
}

#Apply function
inun<-lapply(X = sheds$FID, FUN = subshed_inundate) %>% bind_rows()

#Estimate for entire basin and QB
basin<-inun %>% 
  group_by(ele_m) %>% 
  summarise(area_m2 = sum(area_m2))
QB<-inun %>% 
  filter(FID==2) %>% 
  select(-FID)

#Export
write_csv(basin,"docs/QB_watershed_elevation_area.csv")
write_csv(basin,"docs/QB_subshed_elevation_area.csv")
