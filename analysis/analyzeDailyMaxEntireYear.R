library(here)
library(stars)
library(sf)
library(glue)
library(tidyverse)
library(rajudas)
library(data.table)

source(here("analysis/utils.R"))

# params ------------------------------------------------------------------
output_dir = here("data_raw/dailyMaxEntireYear/")
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


# filter potential stations -----------------------------------------------
selected_stations = stations %>% 
  filter(
    state == "Wien"
  )


# get the data for that stations ------------------------------------------
allDirs = list.dirs(output_dir, full.names = T) %>% .[2:length(.)]

# select only the directories (stations) that are selected
bns = basename(allDirs)
idxs = which(bns %in% selected_stations$id )
selectedDirs = allDirs[idxs]


# read in all the data
stationsData = map(selectedDirs, function(d){
  years = dir(d, "\\.csv", full.names = T)
  id = basename(d)
  data = map(years, fread) %>% bind_rows()
  data[["id"]]  = id
  data
})


# select the 1st dec to 31 jan for each station  ---------------------------
dataEachStation = imap(stationsData, function(s, i){
  
  cat(i, "/", length(stationsData), "\r")
  s %>%
    mutate(
      day = lubridate::day(time),
      month = lubridate::month(time),
      winter = case_when((month == 12 | month == 1) ~ T,
                         T ~ F)
    ) %>%
    filter(winter == T) %>% 
    mutate(periodNo = ifelse((month == 12 &
                                day == 1), row_number(),  ifelse(
                                  row_number() == 1,
                                  row_number(), NA
                                ))) %>% 
    tidyr::fill(periodNo) %>% 
    group_by(periodNo) %>%
    mutate(
      year = lubridate::year(first(time)),
      season = case_when(
        first(month) == 12 ~ glue("{year}/{year+1}"),
        T ~ glue("{year - 1}/{year}")
      ),
      fakeDate = case_when(month == 12 ~ as.Date(glue(
        "2000-{month}-{day}"
      )),
      T ~ as.Date(glue(
        "2001-{month}-{day}"
      )))
    ) %>% select(data, geo.x, geo.y, time, id, periodNo, fakeDate, season) %>%
    group_by(periodNo) %>%
    filter(!is.na(data)) %>% 
    mutate(t = cur_group_id()) -> data
  
})    



force = T
iwalk(dataEachStation, function(s, i){
  print(i)
  
  data = formatOneStationDW(s)
  id = s$id[[1]] 
  name = data$name[[1]] %>% janitor::make_clean_names()
  print(name)
  
  start = s$time[[1]] %>% as.Date() %>% lubridate::year()
  end = s$time[[length(s$time)]] %>% as.Date() %>% lubridate::year()
  
  data[["start"]] = start
  data[["end"]] = end
  
  
  outfile = makePath(here(glue("output/data/statstionsDezJanDW/{id}_{name}_{start}_{end}.csv")))
  if(file.exists(outfile) & !force) return()
  
  write_csv(data, outfile)
  
})







