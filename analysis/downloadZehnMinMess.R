library(here)
library(stars)
library(sf)
library(glue)
library(tidyverse)
library(rajudas)


# params ------------------------------------------------------------------
output_dir = here("data_raw/zehnmin/")
if(!dir.exists(output_dir)){
  dir.create(output_dir)
}

years = 2000:2023




# get all the stations ----------------------------------------------------
meta = httr::GET("https://dataset.api.hub.zamg.ac.at/v1/station/historical/klima-v1-10min/metadata") %>% httr::content()

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
selected_params = c("TL", "RR", "TS")
selected_params = selected_params %>% paste0(collapse = ",")



# download data -----------------------------------------------------------
ids = split(stations, stations$id)



for(i in seq_along(ids)){
  
  print(paste0(i, "/", length(ids)))
  row = ids[[i]] 
  id = row$id
  print(id)
  start = ids[[1]]$valid_from
  end = ids[[1]]$valid_to
  
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
    if(j == length(years_of_data)) next
    
    # outfile
    outfile = makePath(here(glue("output/stations10Min/{id}/{years_of_data[[j]]}.csv")))
    if(file.exists(outfile)) next
    
    start=glue("{years_of_data[[j]]}-12-31T18:00")
    end= glue("{years_of_data[[j+1]]}-01-01T18:00")
     
    
    # url
    url = glue(
      "https://dataset.api.hub.zamg.ac.at/v1/station/historical/klima-v1-10min?parameters={selected_params}&start={start}&end={end}&station_ids={id}"
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
       geo = geo
     )
     
     return(df)
   }) %>% bind_rows()
   
   write_csv(data, outfile)
   
  }
}
  

