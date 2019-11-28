library(rgdal)
library(ggmap)
library(ggrepel)
library(gridExtra)
library(sp)

setwd("C:/Users/Sensonomic Admin/Desktop/darwin-models/")

communities <- c("Bandafassi","Makacoulibantang","Ndorna")

roads_list <- list()

for (i in 1:length(communities)) {
  
  roads <- readOGR(paste0("senegal/",tolower(communities[i]),"_data/",communities[i],"_roads_connected.geojson"))
  
  roads_trans <- spTransform(roads, CRS( "+init=epsg:4326" ) )
  
  roads_list[[i]] <- roads_trans
  
}

villages_list <- list()

villages_list[[1]] <- readOGR("senegal/Bandafassi_data/villages_storage_merged.shp")

for (i in 2:length(communities)) {
  
  villages <- readOGR(paste0("senegal/",tolower(communities[i]),"_data/",communities[i],"_villages.shp"))
  
  villages_list[[i]] <- villages
  
}

poly_list <- list()

for (i in 1:length(communities)) {
  
  coords <- matrix(c(xmin(roads_list[[i]]),ymin(roads_list[[i]]),
                     xmin(roads_list[[i]]),ymax(roads_list[[i]]),
                     xmax(roads_list[[i]]),ymax(roads_list[[i]]),
                     xmax(roads_list[[i]]),ymin(roads_list[[i]]),
                     xmin(roads_list[[i]]),ymin(roads_list[[i]])),
                   ncol = 2, byrow = TRUE)
  
  p <- Polygon(coords)
  
  ps <- Polygons(list(p),1)
  
  sps <-  SpatialPolygons(list(ps), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  poly_list[[i]] <- sps
  
}



register_google(key = "INSERT_API_KEY")

# banda_map <- get_map(location=rowMeans(bbox(banda_roads)), maptype = "satellite",zoom=10)   # get Google map

banda_plot <- ggmap(banda_map) + 
  geom_path(data=roads_list[[1]],aes(long,lat,group=group),color="yellow",size=1.25) +
  geom_point(data=as.data.frame(villages_list[[1]]), aes(coords.x1,coords.x2),
             color="red", size=2, shape=19) +
  geom_label_repel(data=as.data.frame(villages_list[[1]]), 
                   aes(coords.x1,coords.x2,label = villages_list[[1]]$Localites),
                   size=2.5) +
  scale_x_continuous(limits = c(-12.5, -11.9), expand = c(0, 0)) +
  scale_y_continuous(limits = c(12.4, 12.8), expand = c(0, 0)) +
  ggtitle("Bandafassi") +
  theme(plot.title = element_text(hjust = 0.5))

# maka_map <- get_map(location=rowMeans(bbox(maka_roads)), maptype = "satellite",zoom=10)   # get Google map

maka_plot <- ggmap(maka_map) + 
  geom_path(data=roads_list[[2]],aes(long,lat,group=group),color="yellow",size=1.25) +
  geom_point(data=as.data.frame(villages_list[[2]]), aes(coords.x1,coords.x2),
             color="red", size=2, shape=19) +
  geom_label_repel(data=as.data.frame(villages_list[[2]]), 
                   aes(coords.x1,coords.x2,label = villages_list[[2]]$Localites),
                   size=2.5) +
  scale_x_continuous(limits = c(-14.5, -14), expand = c(0, 0)) +
  scale_y_continuous(limits = c(13.4, 13.8), expand = c(0, 0)) +
  ggtitle("Makacoulibantang") +
  theme(plot.title = element_text(hjust = 0.5))
  
# ndorna_map <- get_map(location=rowMeans(bbox(ndorna_roads)), maptype = "satellite",zoom=10)   # get Google map

ndorna_plot <- ggmap(ndorna_map) + 
  geom_path(data=roads_list[[3]],aes(long,lat,group=group),color="yellow",size=1.25) +
  geom_point(data=as.data.frame(villages_list[[3]]), aes(coords.x1,coords.x2),
             color="red", size=2, shape=19) +
  geom_label_repel(data=as.data.frame(villages_list[[3]]), 
                   aes(coords.x1,coords.x2,label = villages_list[[3]]$Localites),
                   size=2.5) +
  scale_x_continuous(limits = c(-15.4, -14.9), expand = c(0, 0)) +
  scale_y_continuous(limits = c(13, 13.35), expand = c(0, 0)) +
  ggtitle("Ndorna")  +
  theme(plot.title = element_text(hjust = 0.5))

# senegal_map <- get_map(location= "senegal", maptype = "hybrid", zoom = 7)

lon<- c(mean(bbox(poly_list[[1]])[1,]),mean(bbox(poly_list[[2]])[1,]),mean(bbox(poly_list[[3]])[1,]))
lat<- c(ymin(poly_list[[1]])-0.1,ymin(poly_list[[2]])-0.1,ymin(poly_list[[3]])-0.1)
label <- c("Bandafassi","Makacoulibantang","Ndorna")
df<-data.frame(lon,lat,label)

senegal_plot <- ggmap(senegal_map) +
  geom_polygon(data=poly_list[[1]],aes(long,lat,group=group),color="green",fill="green",alpha=0.1,label="test") +
  geom_polygon(data=poly_list[[2]],aes(long,lat,group=group),color="green",fill="green",alpha=0.1) +
  geom_polygon(data=poly_list[[3]],aes(long,lat,group=group),color="green",fill="green",alpha=0.1) +
  geom_label(data = df, aes(x = lon, y = lat, label = label), 
            size = 3, vjust = 1, hjust = 0) +
  scale_y_continuous(limits = c(12, 17), expand = c(0, 0))

dev.new(width=10,height=8,noRStudioGD = TRUE)

grid.arrange(banda_plot,maka_plot,ndorna_plot,senegal_plot,nrow=2)
  