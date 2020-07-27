#final workthrough of panel assembly code
library(sp)
library(sf)
library(tidyverse)
library(maptools)
library(rgdal)
library(rgeos)
library(raster)
rastlist <- list.files(path = "~/Dropbox/CDL_CO_Panel/Rasters/", pattern='.tif$', all.files=TRUE, full.names=TRUE)
allrasters <- lapply(rastlist, raster)
#test_grid<-readOGR("~/Dropbox/CDL_CO_Panel/test grid/grid.shp")
#test_grid<- spTransform(test_grid, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
#co_grid<- readOGR("~/Dropbox/CDL_CO_Panel/co_grid.shp")
#sp_600ac_grid<-readOGR("~/Dropbox/CDL_CO_Panel/600acre_SP/600ac_grid_SP.shp")
co_county<- readOGR("~/Dropbox/CDL_CO_Panel/CO_cnty/lm_cnty.shp")
co_county<- spTransform(co_county, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
co_county_sub<- readOGR("~/Dropbox/CDL_CO_Panel/CO_county/CO_counties.shp")
co_county<- spTransform(co_county, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#test_grid<-readOGR("~/Dropbox/CDL_CO_Panel/test grid/grid.shp")


for(j in 1:length(allrasters)) {
  bound<-co_county
  ras<-allrasters[[j]]
  pref<- "df_"
  suff<- 2008:2019
  result<-data.frame()
  
  for (i in 1:length(bound)) { #this is the number of polygons to iterate through
    single <- bound[i,] #selects a single polygon
    clip1 <- crop(ras, extent(single)) #crops the raster to the extent of the polygon, I do this first because it speeds the mask up
    clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
    ext<-raster::extract(clip2,single) #extracts data from the raster based on the polygon bound
    tab<-lapply(ext,table) #makes a table of the extract output
    s<-sum(tab[[1]])  #sums the table for percentage calculation
    mat<- as.data.frame(tab) 
    mat2<- as.data.frame(tab[[1]]/s) #calculates percent
    rep<- bound$COUNTYFP[i] #have to choose identifier here.
    final<-cbind(rep(rep,NROW(mat)),mat,mat2$Freq) #combines into single dataframe
    names(final)[1] <- "Fips"
    names(final)[2] <- "CDL_ID"
    names(final)[3] <- "Freq"
    names(final)[4] <- "Percent"
    result <-rbind(final,result)
  }
  result1<- cbind(rep(suff[j], NROW(result)),result)
  names(result1)[1]<- "Year"
  assign(paste(pref, suff[j], sep = ""), result1)
}
data_panel<- as_tibble(bind_rows(df_2008,df_2009,df_2010,df_2011,df_2012,df_2013,df_2014,df_2015,df_2016,df_2017,df_2018,df_2019))
CDL_Code_Name <- read_csv("~/Dropbox/CDL_CO_Panel/CDL_Code_Name.csv")
data_panel<- merge(CDL_Code_Name, data_panel, by = 'CDL_ID')
write_csv(data_panel_sub, file = "~/Dropbox/CDL_CO_Panel/CDL_CO_panel.csv")



bound<-co_county
ras<-allrasters[[1]]
pref<- "df_"
suff<- 2008:2019
result<-data.frame()

for (i in 1:length(bound)) { #this is the number of polygons to iterate through
  single <- bound[i,] #selects a single polygon
  clip1 <- crop(ras, extent(single)) #crops the raster to the extent of the polygon, I do this first because it speeds the mask up
  clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
  ext<-raster::extract(clip2,single) #extracts data from the raster based on the polygon bound
  tab<-lapply(ext,table) #makes a table of the extract output
  s<-sum(tab[[1]])  #sums the table for percentage calculation
  mat<- as.data.frame(tab) 
  mat2<- as.data.frame(tab[[1]]/s) #calculates percent
  rep<- bound$COUNTYFP[i] #have to choose identifier here.
  final<-cbind(rep(rep,NROW(mat)),mat,mat2$Freq) #combines into single dataframe
  names(final)[1] <- "Fips"
  names(final)[2] <- "CDL_ID"
  names(final)[3] <- "Freq"
  names(final)[4] <- "Percent"
  result <-rbind(final,result)
}
result1<- cbind(rep(suff[1], NROW(result)),result)
names(result1)[1]<- "Year"
assign(paste(pref, suff[1], sep = ""), result1)

####
data_panel<- as_tibble(bind_rows(df_2008,df_2009,df_2010,df_2011,df_2012,df_2013,df_2014,df_2015,df_2016,df_2017,df_2018,df_2019))
CDL_Code_Name <- read_csv("~/Dropbox/CDL_CO_Panel/CDL_Code_Name.csv")
data_panel<- merge(CDL_Code_Name, data_panel, by = 'CDL_ID')
write_csv(data_panel_sub, file = "~/Dropbox/CDL_CO_Panel/CDL_CO_panel.csv")



