library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
library(tidyr)
library(gtools)

# first change the working directory, so that the relative paths below work.
basepath <- paste0(Sys.getenv("HOME"), "/darwin-models")
if (Sys.getenv("USER") == "lassegs") basepath <- paste0(Sys.getenv("HOME"), "/dev/darwin-models/")
if (Sys.getenv("USER") == "lkarsten") basepath <- paste0(Sys.getenv("HOME"), "/oppdrag/sensonomic/darwin-models/")
if (Sys.getenv("HOME") == "/home/babbage") basepath <- "/babbage-data/"
if (Sys.getenv("SystemDrive") == "C:") basepath <- "C:/Users/Sensonomic Admin/Desktop/darwin-models/"

setwd(basepath)

# set community

communities <- c("Bandafassi", "Makacoulibantang", "Ndorna",
                 "Grid_distributed", "Radial_distributed", "Random_distributed",
                 "Grid","Radial","Random",
                 "Grid_trade_offs","Radial_trade_offs","Random_trade_offs")

for(j in 1:length(communities)) {
  
  if ( communities[j] != "Bandafassi" & communities[j] != "Makacoulibantang" & communities[j] != "Ndorna" ) {
    
    roads <- readOGR(paste0("senegal/Bandafassi_data/bandafassi_roads_connected.geojson"))
    
  } else {
    
    roads <- readOGR(paste0("senegal/", communities[j], "_data/", tolower(communities[j]), "_roads_connected.geojson"))
    
  }
  
  roads <- spTransform(roads, CRS( "+init=epsg:4326" ) )
  
  ext <- extent(roads)
  
  xdist <- ext[2]-ext[1]
  
  ydist <- ext[4]-ext[3]
  
  diff_dist <- xdist-ydist
  
  ext[3] <- ext[3]-diff_dist/2
  
  ext[4] <- ext[4]+diff_dist/2
  
  file_list <- list.files(paste0("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/world_raster_export_files_",tolower(communities[j])),
                          full.names = TRUE)
  
  file_list <- mixedsort(file_list)
  
  
  grid_seeds_maps <- scan("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/grid_seeds_maps")
  
  grid_seeds_maps <- grid_seeds_maps[order(grid_seeds_maps)]
  
  radial_seeds_maps <- scan("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/radial_seeds_maps")
  
  radial_seeds_maps <- radial_seeds_maps[order(radial_seeds_maps)] 
  
  random_seeds_maps <- scan("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/random_seeds_maps")
  
  random_seeds_maps <- random_seeds_maps[order(random_seeds_maps)] 
  
  
  for (i in 1:length(file_list)) {
    
    r <- raster(file_list[i])
    
    extent(r) <- ext
    
    if (communities[j] != "Grid_trade_offs" & communities[j] != "Radial_trade_offs" & communities[j] != "Random_trade_offs") {
      
      writeRaster(r,
                  paste0("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/world_raster_export_files_georeferenced_",tolower(communities[j]),"/world_raster_export",i,".asc"),
                  overwrite=TRUE)
      
    } else if ( communities[j] == "Grid_trade_offs") {
      
      writeRaster(r,
                  paste0("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/world_raster_export_files_georeferenced_",tolower(communities[j]),"/world_raster_export",grid_seeds_maps[i],".asc"),
                  overwrite=TRUE)
      
    } else if ( communities[j] == "Radial_trade_offs") {
      
      writeRaster(r,
                  paste0("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/world_raster_export_files_georeferenced_",tolower(communities[j]),"/world_raster_export",radial_seeds_maps[i],".asc"),
                  overwrite=TRUE)
      
    } else if ( communities[j] == "Random_trade_offs") {
      
      writeRaster(r,
                  paste0("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/world_raster_export_files_georeferenced_",tolower(communities[j]),"/world_raster_export",random_seeds_maps[i],".asc"),
                  overwrite=TRUE)
      
    }
    
  }
  
}