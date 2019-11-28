library(raster)
library(gtools)
library(rgdal)
library(rasterVis)
library(gridExtra)

# Generate intermediate maps plot list

plot_list <- list()

# communities <- c("Bandafassi","Makacoulibantang","Ndorna")

# communities <- c("Grid","Radial","Random")

communities <- c("Grid_distributed","Radial_distributed","Random_distributed")

for (j in 1:length(communities)) {
  
  file_list <- list.files(paste0("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/world_raster_export_files_georeferenced_",tolower(communities[j])),
                          full.names = TRUE)
  
  file_list <- mixedsort(file_list)
  
  # show four steps
  
  file_list_sub <- file_list[grep("t1.asc|t6.asc|t11.|t17.asc",file_list)]
  
  rast_stack <- stack(file_list_sub)
  
  rast_stack_rat <- stack()
  
  for (i in 1:nlayers(rast_stack)) {
    
    r <- raster(rast_stack,layer=i)
    
    r <- ratify(r)
    
    rat <- levels(r)[[1]]
    
    rat$landcover <- c("undeveloped","paved roads","villages","unpaved roads","irrigated crops","rainfed crops","flood risk roads")
    
    levels(r) <- rat
    
    rast_stack_rat <- stack(rast_stack_rat,r)
    
  }
  
  plot_titles <- c("2019","2024","2029","2035")
  
  plot_list[[j]] <- levelplot(rast_stack_rat,
                              col.regions=c("black","gray","red","tan3","forestgreen","lightgreen","cornflowerblue"),
                              names.attr=plot_titles)
  
}

# Plot intermediate maps plot list

folder_path <- "C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/"

for (j in 1:length(communities)) {
  
  file_path <- paste0(folder_path,communities[j],"_intermediate_maps_112519.png")
  
  png(file_path,width=10,height=8,units="in",res=300)
  
  print(plot_list[[j]])
  
  dev.off()
  
}

# Generate trade off maps

plot_list <- list()

communities <- c("Grid_trade_offs","Radial_trade_offs","Random_trade_offs")

network_seeds_maps <- read.csv( "C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/network_seeds_maps.csv")

for (j in 1:length(communities)) {
  
  file_list <- list.files(paste0("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/world_raster_export_files_georeferenced_",tolower(communities[j])),
                          full.names = TRUE)
  
  file_list <- mixedsort(file_list)
  
  seeds_file_order <- as.numeric(gsub(".*?([0-9]+).*", "\\1", file_list))
  
  seeds_RMSE_order <- network_seeds_maps[,j]
  
  file_list <- file_list[order(match(seeds_file_order,seeds_RMSE_order))]
  
  rast_stack <- stack(file_list)
  
  rast_stack_rat <- stack()
  
  for (i in 1:nlayers(rast_stack)) {
    
    r <- raster(rast_stack,layer=i)
    
    r <- ratify(r)
    
    rat <- levels(r)[[1]]
    
    rat$landcover <- c("undeveloped","paved roads","villages","unpaved roads","irrigated crops","rainfed crops","flood risk roads")
    
    levels(r) <- rat
    
    rast_stack_rat <- stack(rast_stack_rat,r)
    
  }
  
  plot_titles_seeds <- paste("seed",seeds_RMSE_order)
  
  plot_titles <- paste0("Rank ",seq(1,60,10),": ",plot_titles_seeds)
  
  plot_list[[j]] <- levelplot(rast_stack_rat,
                              col.regions=c("black","gray","red","tan3","forestgreen","lightgreen","cornflowerblue"),
                              names.attr=plot_titles,
                              margin=FALSE,
                              layout=c(2,3))
  
}


folder_path <- "C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Roads model/Intermediate_and_trade_off_maps/"

for (j in 1:length(communities)) {
  
  file_path <- paste0(folder_path,communities[j],"_maps_112619.png")
  
  png(file_path,width=8,height=10,units="in",res=300)
  
  print(plot_list[[j]])
  
  dev.off()
  
}