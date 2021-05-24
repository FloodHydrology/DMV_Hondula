#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Watershed Shapes
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 5/23/2021
#Purpose: Export ML and QB Watershed shapes for Kelly Hondula
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
rm(list=ls(all=TRUE))

#Define temp dir
dir.create(paste0(getwd(), '/data/temp'))
temp_dir<-paste0(getwd(), '/data/temp/')

#Download packages 
library(sf)
library(raster)
library(stars)
library(whitebox)
library(leaflet)
library(tidyverse)

#Load DEM 
dem<-raster('data/dem.tif')

#Load outlet information
wells<-st_read('data/well_location.shp') %>% 
  #Filter to points of interest
  filter(
    Site_ID == 'Jones Road South Catchment Outlet' | 
    Site_ID == 'Mikey Likey Catchment Outlet') 

#Load wetland shape
wetlands<-st_read('data/wetlands.shp')
  


#Plot for funzies
plot(dem)
wetlands %>% st_geometry() %>% plot(., add=T, col="dark blue")
wells %>% st_geometry() %>% plot(., add=T, col="dark red", pch=19, cex=3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Watershed Delineation------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Burn Wetlands into DEM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create DEM mask
mask<-rasterize(wetlands, dem, 1)
dem_mask<-mask*dem
  
#Create minimum raster
dem_min<-cellStats(dem_mask, base::min, na.rm=T)
dem_min<-dem_mask*0+dem_min
dem_min[is.na(dem_min)]<-0
  
#Replace masked location with min raster value
dem_mask<-dem_mask*0
dem_mask[is.na(dem_mask)]<-1
dem<-dem*dem_mask+dem_min
  
#2.2 Prep DEM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Export DEM to tmep dir
writeRaster(dem, paste0(temp_dir,"dem.tif"), overwrite=T)

#Apply gaussian filter
wbt_gaussian_filter(
  input = 'dem.tif',
  output = 'dem_filter.tif',
  wd = temp_dir
)

#breach depressions
wbt_breach_depressions(
  dem = 'dem_filter.tif',
  output = 'dem_breached.tif',
  fill_pits = T,
  wd = temp_dir
)

#2.3 Watershed Delineation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Export pour points to temp dir
st_write(wells, paste0(temp_dir,"pp.shp"), delete_dsn =T)

#Flow Direction
wbt_d8_pointer(
  dem='dem_breached.tif',
  output = 'fdr.tif',
  wd = temp_dir
)

#Flow accumulation
wbt_d8_flow_accumulation(
  input = 'dem_breached.tif',
  output = 'fac.tif',
  wd = temp_dir
)

#Create streams raster
wbt_reclass(
  input = 'fac.tif',
  output = 'streams.tif',
  reclass_vals = '0;1;40000;1;40001;15000000', 
  wd= temp_dir
)

#Snap pour point to streams raster
wbt_jenson_snap_pour_points(
  pour_pts = 'pp.shp',
  streams = 'streams.tif',
  snap_dist = 100,
  output = 'snap.shp',
  wd = temp_dir
)

#Watershed Delineation
wbt_watershed(
  d8_pntr = "fdr.tif",
  pour_pts = "snap.shp", 
  output = "sheds.tif" ,
  wd=temp_dir
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Export Shapes -------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create polygon from raster
sheds<-raster(paste0(temp_dir, 'sheds.tif'))
sheds<-sheds %>% st_as_stars(.) %>% st_as_sf(., merge=T)

#Export watershed shapes
st_write(sheds, "docs/sheds.shp")

#Delete temp folder
unlink("data/temp", recursive = T)
