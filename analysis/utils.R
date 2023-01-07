library(here)
library(stars)
library(sf)
library(glue)
library(tidyverse)
library(rajudas)
library(data.table)

meta = httr::GET("https://dataset.api.hub.zamg.ac.at/v1/station/historical/klima-v1-1d/metadata") %>% httr::content()

stationsInfo = map(meta$stations, function(x){
 
  # replace null with na   
  isnull = which(map(x, is.null) %>% unlist) %>% unname()
  x[isnull] = NA
  
  # make df
  df = as.data.frame(x)
  
  return(df)
  
}) %>% bind_rows() 


formatOneStationDW = function(
    df,
    seasonVar = "season",
    dateVar = "time",
    fakeDateVar = "fakeDate",
    periodNoVar = "t"
) {
  
  # mean without 2023
  df %>% 
    ungroup() %>% 
    select(seasonVar, data, fakeDate, id) -> seasonData
  
   # if there is no 2023 season dont compute mean 
   contains23 = "2022/2023" %in% seasonData$season
   if(!contains23){
    seasonData %>% 
       filter(season != last(season)) %>% 
       group_by(fakeDate) %>%
       summarise(data = mean(data, na.rm = T)) %>%
       mutate(season = "mean_upToLastYear",
              id = seasonData$id[[1]]) -> meen
     
     bind_rows(seasonData, meen) %>% pivot_wider(names_from = {
       {
         seasonVar
       }
     },
     values_from = data) -> dfWide
       
   }else{
    
     # mean without 23 
     seasonData %>%
       filter(season != "2022/2023") %>%
       group_by(fakeDate) %>%
       summarise(data = mean(data, na.rm = T)) %>%
       mutate(season = "mean",
              id = seasonData$id[[1]]) -> meen
     
     bind_rows(seasonData, meen) %>% pivot_wider(names_from = {
       {
         seasonVar
       }
     },
     values_from = data) -> dfWide
   }
  
  
  dfWide %>% 
    select(
      where(
        function(x){
          sum(is.na(x)) != length(x)
        }
      )
    ) -> dfFinal
  
  stationsInfo %>% 
    select(id, lat, lon, name, state, altitude) %>% right_join(dfFinal, by="id") -> dfWithInfo
  

  return(dfWithInfo)  
    
   
}

formatRawData = function(files,
                         months = c(12,1)) {
  
  df = map(files, fread) %>% bind_rows
  
  
   df %>%
    mutate(
      day = lubridate::day(time),
      month = lubridate::month(time),
      winter = case_when((month %in% months) ~ T,
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
    ) %>%
    group_by(periodNo) %>%
    filter(!is.na(data)) %>% 
    mutate(t = cur_group_id()) %>% ungroup() -> data
  
  
}


formatSeasonData = function(df){
  
  #####
  # has 2023
  ####
  has2023 = "2022/2023" %in% df$season
  
  # mean until last season
  df %>%
    mutate(lastSeason = last(season)) %>%
    filter(season != lastSeason) %>%
    group_by(fakeDate) %>%
    summarise(
      data = mean(data, na.rm = T),
      mean_period = glue("{first(season)}_{last(season)}"),
      season = "mean"
    ) %>% ungroup -> meanData
  
  df %>%
    select(data,
           fakeDate,
           season) %>% bind_rows(meanData) -> dataFinal
  
  dataFinal[["has2023"]] = has2023
  
  
  return(dataFinal)
  
}


makeHoverPlot = function(dfPerSeason, row, fontsize=2){
 
  # colors 
  colorMean = "black"
  color2023 = "firebrick"
  colorOtherLastSeason = "darkgreen"
  colorRest = "#cccccc"
  
  # has 2023
  has2023 = dfPerSeason$has2023[[1]]
  firstSeason = min(dfPerSeason$season[dfPerSeason$season != "mean"])
  lastSeason = max(dfPerSeason$season[dfPerSeason$season != "mean"])
  secondLastSeason = sort(dfPerSeason$season) %>% .[. != "mean"] %>% unique %>% .[1:length(.)-1] %>% max
  
  
  dfPerSeason %>% 
    mutate(
      color = case_when(
        season == "mean" ~ colorMean,
        season == "2022/2023" ~ color2023,
        season == last(season[season!= "mean"])  ~ colorOtherLastSeason,
        T ~ colorRest,
      ),
      alpha = case_when(
        season != last(season[season != "mean"]) & season != last(season) ~ .2,
        T ~ 1
      )
    ) -> plotdf
  
  ######## 
  # legend 
  ######## 
  if (!has2023) {
    labels = c(
      setNames(glue("{firstSeason} - {secondLastSeason}"), colorRest),
      setNames(
        glue("Mittelwert ({firstSeason} - {secondLastSeason})"),
        colorMean
      ),
      setNames(lastSeason, colorOtherLastSeason)
    )
  } else{
    labels = c(
      setNames(glue("{firstSeason} - {secondLastSeason}"), colorRest),
      setNames(
        glue("Mittelwert ({firstSeason} - {secondLastSeason})"),
        colorMean
      ),
      setNames(lastSeason, colorOtherLastSeason)
    )
  }
  
  scale = scale_color_identity(
    labels = labels,
    guide = guide_legend(
      title = NULL,
      override.aes = list(size=3),
      label.position = "top"
    )
  )
  
  #####
  # annotations
  #####
  labelLast = plotdf %>% 
    filter(season == lastSeason) %>% 
    filter(fakeDate == max(fakeDate)) %>% 
    mutate(
      label = season,
      color = ifelse(has2023, color2023, colorOtherLastSeason),
      x = as.Date("2000-12-03"),
      y = data
    )
  
  labelMean = plotdf %>% 
    filter(season == lastSeason) %>% 
    filter(fakeDate == max(fakeDate)) %>% 
    mutate(
      label = glue("Ø {firstSeason} - {secondLastSeason}"),
      color = colorMean,
      x = as.Date("2000-12-03"),
      y = data -3
    )

  bind_rows(labelLast, labelMean) %>% 
    select(
      label, color, x,y
    ) -> dfLabel
    
  
  
  
  #######
  # Plot
  #######
  baseDate = as.Date("2000-12-15")
  ggplot(plotdf) +
    geom_line(
      aes(
        fakeDate,
        data,
        group = season,
        alpha = alpha,
        color = color
      ),
      show.legend = T
    ) +
    scale_color_identity() +
    scale_alpha_identity() +
    scale_y_continuous(labels = function(x) return(paste0(x, "°C"))) +
    scale_x_date(expand = c(0,0), breaks = c(baseDate, baseDate+15, baseDate+31),
                 date_labels = "%d. %m") +
    theme_void(base_size = fontsize) +
    theme(
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.key.width = unit(2, "cm"),
      axis.text.x = element_text(family = "Roboto"),
      axis.text.y = element_text(family = "Roboto"),
      panel.grid.major.y = element_line(size=.2, color="black"),
      plot.margin = margin(.2, .2, .2, .2,"cm")
    ) -> pl # + scale 
    
  gt = geom_text(
      data = dfLabel,
      aes(
        x = x,
        y = y,
        color = color,
        label = label
      ),
      size=fontsize/2,
      family="Roboto",
      fontface = "bold",
      hjust=0
    ) 
  
  
  return(pl)  
  
  
  
}



