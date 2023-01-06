library(here)
library(stars)
library(sf)
library(glue)
library(tidyverse)
library(rajudas)
library(data.table)
library(rlist)


# in dir ------------------------------------------------------------------
in_dir = here("output/stationsDailyMax/")


# read the data ----------------------------------------------------------

######
# list all dirs (which are all the stations)
######

dirs = list.dirs(in_dir, full.names = T)
dirs = dirs[2:length(dirs)]


#####
# read the data for each station
#####
data = imap(dirs, function(x,i){
  
  # id
  id = basename(x)
  
  # list all the files (the years) 
  year_files = dir(x, full.names = T)
  
  
  data_one_station = map(year_files, function(y) {
    
    season = basename(y)  %>% str_split("\\.") %>% .[[1]] %>% .[[1]]
    data = fread(y)
    data[["season"]] = season
    data
  }) %>% bind_rows()
  
  data_one_station[["stationId"]] = id
  
  data_one_station
  
})




# plot function -----------------------------------------------------------

meta = httr::GET("https://dataset.api.hub.zamg.ac.at/v1/station/historical/klima-v1-1d/metadata") %>% httr::content()

stations = map(meta$stations, function(x){
 
  # replace null with na   
  isnull = which(map(x, is.null) %>% unlist) %>% unname()
  x[isnull] = NA
  
  # make df
  df = as.data.frame(x)
  
  return(df)
  
}) %>% bind_rows()




plotStation = function(id){
  library(ggrepel) 
  
  
  # find the data 
  df = data %>% list.filter(.$stationId[[1]] == id)  %>% .[[1]]
  
  # the info about the station
  station = stations %>% 
    filter(
      id == as.character({{id}})
    )
  
  bl = station$state
  name = station$name
  height = station$altitude
  
  
  
  dfWithYear = df %>% 
    mutate(
      year = lubridate::year(time),
      day = lubridate::day(time),
      month = lubridate::month(time),
      fakeDate = case_when(
        month == 12 ~ as.Date(glue("2000-{month}-{day}")),
        TRUE ~ as.Date(glue("2001-{month}-{day}"))
      )
    )
  
  # find the max vals
  dfWithYear %>% 
    filter(!is.na(data)) %>% 
    arrange(desc(data)) %>% 
    filter(
      row_number() %in% c(1:2, (nrow(.)-1):nrow(.))
    ) -> dataLabel
  
  
  
  ggplot(dfWithYear) +
    geom_line(
      aes(
        fakeDate,
        data,
        group = season,
        color=as.numeric(season)
      ),
      alpha=.3
    ) +
    labs(
      x = NULL,
      y = "Tagesmaximaltemperatur in °C",
      title = "Temperaturen vom 1. Dezember bis 31. Januar",
      subtitle = glue("(Für die Station: {name} in {bl} auf {height} m.ü.NN)")
    ) +
    scale_color_continuous(low="white", high="black",
                           guide = guide_colorbar(
                            barwidth = unit(15, "lines"),
                            title = NULL
                           )) +
    geom_point(data = dataLabel,
               aes(fakeDate,
                   data), size=2) +
    geom_text_repel(
      data = dataLabel,
      aes(
        fakeDate,
        data,
        label = glue("{as.Date(time)} ({data} °C)")
      ),
      size = 3
    ) +
    theme_light() +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(size=.1, color="black"),
      panel.ontop = F
    ) -> pl


  return(pl) 
  
}


plotStation2 = function(id){
  library(ggrepel) 
  
  
  # find the data 
  df = data %>% list.filter(.$stationId[[1]] == id)  %>% .[[1]]
  
  # the info about the station
  station = stations %>% 
    filter(
      id == as.character({{id}})
    )
  
  bl = station$state
  name = station$name
  height = station$altitude
  
  
  
  dfWithYear = df %>% 
    mutate(
      year = lubridate::year(time),
      day = lubridate::day(time),
      month = lubridate::month(time),
      fakeDate = case_when(
        month == 12 ~ as.Date(glue("2000-{month}-{day}")),
        TRUE ~ as.Date(glue("2001-{month}-{day}"))
      ),
      season = as.numeric(season)
    )
  
  # medians
  dfWithYear %>% 
    group_by(season) %>% 
    summarise(
      med = median(data, na.rm = T)
    ) -> medians
  
  dfWithYear %>% 
    filter(!is.na(data)) %>% 
    arrange(desc(data)) %>% 
    filter(
      row_number() %in% c(1:2, (nrow(.)-1):nrow(.))
    ) -> dataLabel
  
  ggplot(dfWithYear) +
    geom_point(
      aes(
        season,
        data,
        color=data
      )
    ) +
    labs(
      x = "Saison",
      y = "Tagesmaximaltemperatur in °C",
      title = "Temperaturen vom 1. Dezember bis 31. Januar",
      subtitle = glue("(Für die Station: {name} in {bl} auf {height} m.ü.NN)")
    ) +
    scico::scale_color_scico(palette = "bilbao",
                           guide = guide_colorbar(
                            barwidth = unit(15, "lines"),
                            title = "°C",
                            title.position = "top",
                            title.hjust = .5
                           )) +
    geom_point(data = dataLabel,
               color="red",
               shape = 1,
               aes(season,
                   data), size=3) +
    geom_point(data = medians,
               color="black",
               shape = 8,
               aes(season,
                   med), size=3) +
    geom_smooth(
      data = medians,
      aes(
        season,
        med
      ),
      color="cornflowerblue",
      size=1,
      se = F
    ) +
    scale_x_continuous(
      breaks = floor(seq(min(dfWithYear$year),max(dfWithYear$year), length.out=10))
    ) +
    geom_text_repel(
      data = dataLabel,
      aes(
        season,
        data,
        label = glue("{as.Date(time)} ({data} °C)")
      ),
      size = 3
    ) +
    theme_light() +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(size=.1, color="black"),
      panel.ontop = F
    ) -> pl

  return(pl)
  
  
}


plotStation(100)
plotStation2(100)



# prepare data for datawrapper --------------------------------------------
# wien uni (seit 1775): 5905
# wien hohe warte: 5901
# wien hohe warte: 5904
# wien favoriten: 5902
prepareData = function(df){

  dfWithYear = df %>% 
    mutate(
      year = lubridate::year(time),
      day = lubridate::day(time),
      month = lubridate::month(time),
      fakeDate = case_when(
        month == 12 ~ as.Date(glue("2000-{month}-{day}")),
        TRUE ~ as.Date(glue("2001-{month}-{day}"))
      )
    ) %>% select(fakeDate, data, season)
  
  # mean excluding 2022
  dfWithYear %>% 
    filter(season!=2022) %>% 
    group_by(fakeDate) %>% 
    summarise(
      data = mean(data, na.rm=T),
      season = "mean"
    ) -> mean
 
 all =  bind_rows(dfWithYear, mean)   

  pivot_wider(
    all,
    names_from = season,
    values_from = data
  )  -> dfWide
 
  return(dfWide) 
  
   
}

df = prepareData(data[[1]])
write_csv(df, file=here("output/test.csv"))



