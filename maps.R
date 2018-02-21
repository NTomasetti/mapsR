library(tidyverse)

#http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa2_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&A09309ACB3FA50B8CA257FED0013D420&0&July%202016&12.07.2016&Latest
#http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa4_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&C65BC89E549D1CA3CA257FED0013E074&0&July%202016&12.07.2016&Latest

popData <- readr::read_csv('population.csv') %>%
  mutate(State = case_when(
    State == 'New South Wales' ~ 'NSW',
    State == 'Victoria' ~ 'VIC',
    State == 'Queensland' ~ 'QLD',
    State == 'South Australia' ~ 'SA',
    State == 'Western Australia' ~ 'WA',
    State == 'Tasmania' ~ 'TAS', 
    State == 'Northern Territory' ~ 'NT',
    State == 'Australian Capital Territory' ~ 'ACT',
    TRUE ~ as.character(State))) 

popData %>% 
  group_by(State) %>%
  summarise(pop = sum(`Total Persons`)) -> statePopulation

popData %>%
  group_by(`SA4 code`, State) %>%
  summarise(pop = sum(`Total Persons`)) %>%
  rename(SA4_CODE16 = `SA4 code`) -> sa4Population

popData %>%
  group_by(`SA2 code`, State) %>%
  summarise(pop = sum(`Total Persons`)) %>%
  rename(SA2_CODE16 = `SA2 code`) -> sa2Population

sa4Shp <- rgdal::readOGR('SA4')
sa4Small <- rmapshaper::ms_simplify(sa4Shp, keep = 0.05)

sa4_data <- sa4Small@data
sa4_data$id <- row.names(sa4_data)
sa4_data$SA4_CODE16 <- as.integer(as.character(sa4_data$SA4_CODE16))
sa4_map <- ggplot2::fortify(sa4Small)
sa4_map$group <- paste("g",sa4_map$group,sep=".")
sa4_map$piece <- paste("p",sa4_map$piece,sep=".")

sa4_data %>%
  select(id, SA4_CODE16, SA4_NAME16) %>%
  rename(name = SA4_NAME16) %>%
  right_join(sa4Population) %>%
  right_join(sa4_map) -> sa4_map

ggplot(sa4_map) + geom_polygon(aes(long, lat, group = group), colour = 'grey')


sa2Shp <- rgdal::readOGR('SA2')
sa2Small <- rmapshaper::ms_simplify(sa2Shp, keep = 0.05)

sa2_data <- sa2Small@data
sa2_data$id <- row.names(sa2_data)
sa2_data$SA2_CODE16 <- as.integer(as.character(sa2_data$SA2_5DIG16))
sa2_map <- ggplot2::fortify(sa2Small)
sa2_map$group <- paste("g",sa2_map$group,sep=".")
sa2_map$piece <- paste("p",sa2_map$piece,sep=".")

sa2_data %>%
  select(id, SA2_CODE16, SA2_NAME16) %>%
  rename(name = SA2_NAME16) %>%
  right_join(sa2Population) %>%
  right_join(sa2_map)-> sa2_map

ggplot(sa2_map) + geom_polygon(aes(long, lat, group = group), colour = 'grey')

plotOz <- function(region = c('AUS', 'ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA'), 
                   division = c('sa2', 'sa4'),
                   fillPop = FALSE,
                   ...){
  div <- match.arg(division)
  if(div == 'sa2'){
    map <- sa2_map
  } else if(div == 'sa4') {
    map <- sa4_map
  }
  
  reg <- match.arg(region)
  if(reg != 'AUS'){
    map <- filter(map, State == reg)
  }
  if(fillPop){
    p <- ggplot(map) + geom_polygon(aes(long, lat, group = group, fill = pop, label = name), ...)  
  } else {
    p <- ggplot(map) + geom_polygon(aes(long, lat, group = group, label = name), ...)
  }
  return(p)
}

plotOz('VIC', 'sa2', fillPop = TRUE, colour = 'grey90', size = 0.1) %>% plotly::ggplotly()
