library(here)
library(stars)
library(sf)
library(glue)
library(tidyverse)
library(rajudas)
library(data.table)
library(mapview)

source(here("analysis/utils.R"))

# params ------------------------------------------------------------------
output_dir = here("output/data/statstionsDezJanDW_map/")
input_dir = here("data_raw/dailyMaxEntireYear/")
if(!dir.exists(output_dir)){
  dir.create(output_dir)
}


# get all the stations ----------------------------------------------------
meta = httr::GET("https://dataset.api.hub.zamg.ac.at/v1/station/historical/klima-v1-1d/metadata") %>% httr::content()

stations = map(meta$stations, function(x){
 
  # replace null with na   
  isnull = which(map(x, is.null) %>% unlist) %>% unname()
  x[isnull] = NA
  
  # make df
  df = as.data.frame(x)
  
  return(df)
  
}) %>% bind_rows()

#########
# GEODATA
#########

geo = st_as_sf(stations, coords=c("lon","lat"), crs=4326)

#########
# TEMPDATA FOR EACH STATION
#########

map(seq_along(1:nrow(stations)), function(i){

  row = stations[i,] 
  id = row$id
  altitude = row$altitude
  state = row$state
  
  ## find all the files
  dirStation = here(input_dir, id)
  yearFiles = dir(dirStation, "\\.csv", full.names = T)
  
  # if there are no files yet 
  if(length(yearFiles) == 0) return(NA)
  
  
  # else read and transform the files
  dataSeason = formatRawData(yearFiles)
  
  # get the mean
  perSeason = formatSeasonData(dataSeason)
  
  # make the plot
  plot = makeHoverPlot(perSeason,row, fontsize=2)
  
  # make plotplot
  plotPath = makePath(here(glue("output/graphs/hoverplots/{id}.png")))
  ggsave(plotPath, plot, width=300, height=200, units="px")
  
})



# tes thte plot
stations %>% 
  filter(id == 5901) %>%
  st_as_sf(coords=c("lon", "lat"), crs=4326) %>% 
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2],
    photo = glue("https://github.com/RobinKohrs/neujahr2023/blob/master/output/graphs/hoverplots/{id}.png")
  ) -> dat

write_sf(dat, "~/Desktop/dat.geojson")
write_csv(dat %>% st_drop_geometry(), "~/Desktop/dat.csv")































