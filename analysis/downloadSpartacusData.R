library(here)
library(stars)
library(sf)
library(glue)
library(tidyverse)


# params ------------------------------------------------------------------
output_dir = here("data_raw/spartacus/")
if(!dir.exists(output_dir)){
  dir.create(output_dir)
}

years = 2000:2023




# bounding box ------------------------------------------------------------
oe = "46.308396%2C9.372346%2C49.191016%2C17.390089"
vi = "48.076665%2C16.114197%2C48.353924%2C16.626434"

bbox = "oe"


# download data -----------------------------------------------------------
for(y in years){
  
  print(y)
  
  # start and end 
  start_date = glue("{y}-01-01") 
  end_date = glue("{y}-01-01")
  
  # region
  if(bbox == "wien"){
    bb = vi
    region = "wien"
  }else{
    region = "oesterreich"
    bb = oe
  }
  
  # url
  url = glue("https://dataset.api.hub.zamg.ac.at/v1/grid/historical/spartacus-v1-1d-1km?parameters=Tx&parameters=Tn&start={start_date}T00%3A00%3A00.000Z&end={end_date}T00%3A00%3A00.000Z&bbox={bb}&output_format=netcdf&filename=SPARTACUS+-+Spatial+Reanalysis+Dataset+for+Climate+in+Austria+Datensatz_{y}")
  
  # destfile
  destfile = here(output_dir, glue("{y}_{region}.nc"))
  
  # download file
  if(!file.exists(destfile)){
    download.file(url, destfile = destfile)
  }
  
}


