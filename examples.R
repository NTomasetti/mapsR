aus_map <- readr::read_csv('Data/aus_map.csv')
sa2_map <- readr::read_csv('Data/SA2_map.csv')
sa4_map <- readr::read_csv('Data/SA4_map.csv')
elect_map <- readr::read_csv('Data/elect_map.csv')

colnames(aus_map)
colnames(sa2_map)
colnames(sa4_map)
colnames(elect_map)

plotOz(aus_map, fillVariable = pop)

load('sa2_map.Rda')


aus_map %>%
  ggplot() + geom_polygon(aes(long, lat, group = group),
                          fill = 'grey90',
                          colour = 'black') +
  theme_bw()

elect_map %>%
  group_by(group) %>%
  summarise(maxLat = max(lat),
            minLat = min(lat),
            maxLong = max(long), 
            minLong = min(long)) %>%
  ungroup() %>%
  right_join(elect_map) %>%
  filter(maxLong > 155 | minLong < 110) %>%
  .$group %>% unique() -> outlier

elect_map %>%
  filter(!group %in% outlier) -> elect_map
ggplot(elect_map) + geom_polygon(aes(long, lat, group = group))
  
save(elect_map, file= 'elect_map.Rda')



elect_map %>%
  ggplot() + geom_polygon(aes(long, lat, group = group, fill = State)) +
  theme_bw() +
  scale_fill_ochre(palette = "namatjira_qual") +
  coord_map(xlim = c(110, 155))

sa4_map %>%
  filter(State %in% c('VIC', 'NSW', 'ACT', 'TAS')) %>%
  ggplot() + geom_polygon(aes(long, lat, group = group, fill = pop)) +
  theme_bw() +
  coord_map(xlim = c(140, 155))

