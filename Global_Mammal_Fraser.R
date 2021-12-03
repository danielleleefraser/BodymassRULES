# read in IUCN ranges

library(rgdal)
library(raster)

setwd("C:\\Users\\dfraser\\Dropbox\\Manuscripts\\Body mass rules\\")

mammal_IUCN <- readOGR(dsn = ".", layer = "TERRESTRIAL_MAMMALS")

# This code was needed to install dggridR
# 
# #"C:\\rtools40\\usr\\bin\\make.exe"
# PATH="${RTOOLS40_HOME}//usr//bin;${PATH}"
# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# Sys.which("make")
# 
# #install.packages("rtools40")
# #install_github("r-barnes/dggridR") #Will not install
# install.packages("dggridR")

library(dggridR)

#######################################################################################3
dggs <- dgconstruct(area=10000) # 10,000  km2 grid cells
grid <- dgearthgrid(dggs, frame = TRUE) 


grid_list <- split(grid[,c("long","lat")], grid$cell)#extract each hexagon, 6 points (1 repeated)
ps <- sapply(grid_list, Polygon)
p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), ID = names(grid_list)[i]))
gridpolygon <- SpatialPolygons(p1, proj4string = CRS(proj4string(mammal_IUCN)))
gridpolygon <- SpatialPolygonsDataFrame(gridpolygon, 
                                        data.frame(layer = names(grid_list), 
                                                   row.names = names(grid_list)))

grid_coord <- cbind(gridpolygon@data, setNames(data.frame(coordinates(gridpolygon)), c("x", "y"))) #midpiont of each hexagon

#remove grid cells that aren't over land
grid_points <- SpatialPoints(coordinates(gridpolygon), proj4string=CRS(proj4string(mammal_IUCN)))

library(maptools)
data(wrld_simpl)
rgeos::set_RGEOS_CheckValidity(2L)
grid_overlap <- grid_points %over% as.SpatialPolygons.PolygonsList(wrld_simpl@polygons, proj4string=CRS(proj4string(mammal_IUCN)))
gridpolygon_filt <- gridpolygon[!is.na(grid_overlap),]
gridpolygon_filt_test<-as(gridpolygon_filt, "SpatialPolygons")
coordinates_filt<-coordinates(gridpolygon_filt_test)

maps <- readOGR(dsn = ".", layer = "TERRESTRIAL_MAMMALS")
names<-maps$binomial
names<-unique(names)

setwd("C:\\Users\\dfraser\\Dropbox\\Manuscripts\\Body mass rules\\Shapefiles\\")

files <- list.files(pattern=".shp",recursive=TRUE,full.names=TRUE)

PA_table_temp<-matrix(ncol=length(files),nrow=nrow(coordinates_filt))

for(z in 1:length(files)) {
  print(paste("Species",z,"of 5666"))
  flush.console()
  map <- readOGR(files[z])
  map <- as(map, "SpatialPolygons")
  for (y in 1:length(gridpolygon_filt_test)) {
    if (is.na(over(gridpolygon_filt_test[y], map))) {
      PA_table_temp[y, z] <- 0
    } else {
      PA_table_temp[y, z] <- 1
    }
  }
}

colnames(PA_table_temp)<-files
write.table(PA_table_temp,"Global occurrence matrix November 2021.csv")

# Plot richness on a map
PA_table<-read.csv("Global occurrence matrix December 2021.csv",header=T)
coords_filt<-read.csv("PAtable coordinates.csv",header=T,row.names=1)
Rich<-rowSums(PA_table)

coord_func<-cbind(coords_filt,Rich)

colnames(coord_func)<-c("x","y","Rich")
coord_func<-data.frame(coord_func)

e<-extent(coord_func[,1:2]) # make e the actual grid, grid polygon filt
wgs1984.proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
r<-raster(e,nrow=200,ncol=200,crs=wgs1984.proj)
r_new<-rasterize(coord_func[,1:2],r,coord_func[,3])
colors<-rainbow(nrow(r))
plot(r_new,col=colors)
plot(world,add=T)



##