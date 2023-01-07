library(here)
library(stars)
library(sf)
library(glue)
library(tidyverse)
library(rajudas)

stations %>% mutate(
  ystart = lubridate::year(valid_from),
  yend = ifelse(valid_to > Sys.Date(), lubridate::year(Sys.Date()), lubridate::year(valid_to)),
) %>% glimpse %>% rowwise() %>% 
  mutate(
    l = length(ystart:yend)
  ) %>% glimpse %>% pull(l) %>% sum


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


# variables ---------------------------------------------------------------
params = meta$parameters
params_df = map(params, as.data.frame) %>% bind_rows()


# lufttemperatur in 2m
selected_params = c("tmax")
selected_params = selected_params %>% paste0(collapse = ",")



# download data -----------------------------------------------------------

## only wien for now
# stations %>%
#   filter(
#     state == "Wien"
#   ) -> stations

ids = split(stations, stations$id)

library(future.apply)
plan(multisession, workers=12)
# for(i in seq_along(ids)){
lapply(seq_along(ids), function(i){
  
  output_dir = here("data_raw/dailyMaxEntireYear/")
  selected_params = c("tmax")
  selected_params = selected_params %>% paste0(collapse = ",")
  
    
  print(paste0(i, "/", length(ids)))
  row = ids[[i]] 
  id = row$id
  print(id)
  start = row$valid_from
  end = row$valid_to
  
  # end  
  if(end > Sys.Date()){
    end = Sys.Date()
  }else{
    end = as.Date(end)
  }
  
  # start
  start_overall = as.Date(start)
  
  # for each year in the timerange
  years_of_data = seq(start_overall, end, by="day") %>% 
    lubridate::year() %>% unique()
  
  # for each years
  data = vector("list", length=length(years_of_data)) %>% setNames(years_of_data)
  for(j in seq_along(years_of_data)){
    
    print(paste0("     ", years_of_data[[j]]))
    
    # outfile
    outfile = makePath(here(glue("{output_dir}/{id}/{years_of_data[[j]]}.csv")))
    if(file.exists(outfile)) next
    
    start=glue("{years_of_data[[j]]}-01-01")
    end= glue("{years_of_data[[j]]}-12-31")
     
    
    # url
    url = glue(
      "https://dataset.api.hub.zamg.ac.at/v1/station/historical/klima-v1-1d?parameters={selected_params}&start={start}&end={end}&station_ids={id}"
    )
  
   resp = httr::GET(url)
   if(!resp$status_code == 200){
     data[[j]] = NA
     next
   }
   
   # get the content
   cont = httr::content(resp)
   times = cont$timestamps %>% unlist %>% data.frame(time = .)
   
   # data
   feature1 = cont$features[[1]]
   geo = feature1$geometry$coordinates %>% unlist %>% as.list() %>% setNames(c("x", "y"))
   parameters = feature1$properties$parameters
   data = map(parameters, function(x) {
     name = x$name
     unit = x$unit
     data = x$data
     # replace NULL with NA
     isnull = map(data, is.null) %>% unlist %>% which
     data[isnull] = NA
     data = unlist(data)
     
     df = data.frame(
       name = name,
       unit = unit,
       data = data,
       geo = geo,
       times=times
     )
     
     return(df)
   }) %>% bind_rows()
   
   write_csv(data, outfile)
   
  }
})
x  























