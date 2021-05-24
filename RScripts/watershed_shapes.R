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
#Define temp dir
temp_dir<-'C://WorkspaceR//DMV_Hondula//data//II_Scratch//'
