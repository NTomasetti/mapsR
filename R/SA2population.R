#Create population dataset, lowest level SA2
library(readr)
ABS_ANNUAL_ERP <- read_csv("data/ABS_ANNUAL_ERP_ASGS2016_14032018111559407.csv")
View(ABS_ANNUAL_ERP)


ABS_ANNUAL_ERP %>% filter(REGIONTYPE=="SA2") %>% 
  select(SA2_NAME16 = Region, population = Value) -> SA2population

#write_csv(SA2population, "data/SA2population.csv")
