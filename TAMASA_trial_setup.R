#' TAMASA Ethiopia maize yield data setup
#' 2016 Ethiopia maize yield data courtesy TAMASA
#' M. Walsh, J. v. Heerwaarden, March 2017

# install.packages(c("downloader","rgdal","raster"), dependencies=T)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
})

# Data setup --------------------------------------------------------------
# Create a data folder in your current working directory
dir.create("ET_data", showWarnings=F)
setwd("./ET_data")

# Download 
# Yield data
download("https://www.dropbox.com/s/yz9om81jn9m0nui/TAMASA_train.csv.zip?raw=1", "TAMASA_train.csv.zip", mode="wb")
unzip("TAMASA_train.csv.zip", overwrite=T)
yield <- read.table("TAMASA_train.csv", header=T, sep=",")
yield <- yield[c(1:2,4,6)]
names(yield) <- c("lat","lon","vtype","yield")

# Starter grids
download("https://www.dropbox.com/s/b4rs1nwwy6ww3qt/ET_grids.zip?raw=1", "ET_grids.zip", mode="wb")
unzip("ET_grids.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)

# Overlay with gridded covariates -----------------------------------------
# Project survey coords to grid CRS
yield.proj <- as.data.frame(project(cbind(yield$lon, yield$lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(yield.proj) <- c("x","y") ## laea coordinates
yield <- cbind(yield, yield.proj)
coordinates(yield) <- ~x+y
projection(yield) <- projection(grids)

# Extract gridded variables at trial locations
ygrid <- extract(grids, yield)
yield <- as.data.frame(yield)
yield <- cbind.data.frame(yield, ygrid)
# yield <- unique(na.omit(yield)) ## includes only unique & complete records

# Plots
plot(grids$CRP, axes=F)
points(yield.proj, pch=3, col="red", cex=1)

