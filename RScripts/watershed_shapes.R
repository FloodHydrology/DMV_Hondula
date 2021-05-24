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

#Defin relevant working directories
data_dir<-"//nfs/palmer-group-data/choptank/Nate/Spatial_Analysis/"

#Download packages 
library(sf)
library(raster)
library(whitebox)
library(leaflet)
library(tidyverse)

#Load DEM 
dem<-raster('data/I_Input/2007_1m_DEM.tif')
  dem[dem<0]<-NA

#Load outlet information
wells<-st_read('data/I_input/well_location.shp') %>% 
  #Filter to points of interest
  filter(
    Site_ID == 'Jones Road South Catchment Outlet' |
    Site_ID == 'Mikey Likey Catchment Outlet') 
  
#Plot for funzies
plot(dem)
wells %>% st_geometry() %>% plot(., add=T, col="dark red", pch=19, cex=3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Watershed Delineation------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Setup WBT workspace ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define temp dir
temp_dir<-'C://WorkspaceR//DMV_Hondula//data//II_Scratch//'

#Export DEM and pp to tmep dir
writeRaster(dem, paste0(temp_dir,"dem.tif"))
st_write(wells, paste0(temp_dir,"pp.shp"))

#2.2 Prep DEM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#fill single cell pits
wbt_fill_single_cell_pits(
  dem = 'dem.tif', 
  output = 'dem_filled.tif',
  wd = temp_dir
)

#Apply gaussian filter
wbt_gaussian_filter(
  input = 'dem_filled.tif',
  output = 'dem_filter.tif',
  wd = temp_dir
)

#breach depressions
wbt_breach_depressions(
  dem = 'dem.tif',
  output = 'dem_breached.tif',
  wd = temp_dir
)

#2.3 Watershed Delineation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Flow Direction
wbt_fd8_pointer(
  dem='dem_breached.tif',
  output = 'fdr.tif',
  wd = temp_dir
)

#Flow accumulation
wbt_fd8_flow_accumulation(
  dem = 'dem_breached.tif',
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

#Convert pour points to raster
wbt_vector_points_to_raster(
  input = 'snap.shp',
  output = 'snap.tif',
  base = 'dem_breached.tif',
  wd = temp_dir
)

#Watershed Delineation
wbt_watershed(
  d8_pntr = 'fdr.tif',
  pour_pts = 'snap.tif',
  output = 'watershed.tif',
  wd = temp_dir,  
  verbose_mode = T
)
