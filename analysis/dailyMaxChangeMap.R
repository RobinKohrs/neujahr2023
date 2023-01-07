library(here)
library(stars)
library(sf)
library(glue)
library(tidyverse)
library(rajudas)
library(data.table)
library(mapview)
library(rlist)


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


stations %>% 
  filter(is_active == T) -> stations

data = map(seq_along(1:nrow(stations)), function(i){
  
  print(i)

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
  
  if(nrow(dataSeason) == 0) return(NA)
  
  # get the mean
  perSeason = formatSeasonData(dataSeason)
  perSeason[["id"]] = id
  
  if(!perSeason$has2023[[1]]){
    print("has no 2023")
    return(NA)
  }
  if(nrow(perSeason) == 0) return(NA)
  
  
  # make the plot
  plot = makeHoverPlot(perSeason,row, fontsize=3, margin = 0.05)
  
  # make plotplot
  plotPath = makePath(here(glue("output/graphs/hoverplots/{id}.png")))
  ggsave(plotPath, plot, width=200, height=110, units="px")
  
  return(perSeason)
  
})



# make the map datea ------------------------------------------------------
dataNoNa = data[!is.na(data)]

geoData = imap(dataNoNa, function(d, i){
  print(i)

  day_of_diff = as.Date("2001-01-01")
  day_of_diff_str = day_of_diff %>% str_replace_all("-", "")
  
  mean_and_2023 = d %>%
    filter(season == last(season[season != "mean"]) |
             season == "mean")
  
  
  d %>%
    filter(season != "mean")  %>% 
    filter(data == max(data, na.rm = T)) %>%
    mutate(
      years= str_split(season, "/"),
    ) %>% 
    mutate(
      month = lubridate::month(fakeDate),
      day = lubridate::day(fakeDate),
      year = case_when(month == 12 ~ sapply(years, "[[", 1),
                       T ~ sapply(years, "[[", 2)),
      data_max = data,
      date_data_max = as.Date(glue("{year}-{month}-{day}"))
    ) %>%
    select(data_max,
           date_data_max) -> dmax
  
  mean_and_2023 %>%
    filter(fakeDate == day_of_diff) %>%
    mutate("diff_{day_of_diff_str}" := data[season == "2022/2023"] - data[season ==
                                                                            "mean"]) %>% filter(season == "2022/2023") %>%
    left_join(stations, by = "id") %>%
    select(data,
           id,
           matches("diff.*"),
           name,
           state,
           lat,
           lon,
           altitude,
           valid_from) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) -> d
 
  
  dfFinal = bind_cols(d, dmax)   
  
  
  
 
})


gData = bind_rows(geoData)
gDataCoords = gData %>% 
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  )
mapview(gData, zcol="diff_20010101")



# save geodata ------------------------------------------------------------
write_sf(gData, here(makePath("output/data/geodata/stationsDiff.geojson")))
write_csv(gDataCoords, here(makePath("output/data/geodata/stationsDiff.csv")))






































