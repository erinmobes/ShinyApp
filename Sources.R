library(tidyverse)

----- Data360r -----
  # View indicators from Data360
  # Trade Data: https://tcdata360.worldbank.org/
  # Governance Data: https://govdata360.worldbank.org/
  

library(data360r)

data360_indicators <- get_metadata360(site = 'tc', metadata_type = 'indicators') %>%
  filter(byPartner == TRUE)



RCA <- get_data360(site = 'tc', indicator_id = 40085)
# Revealed Comparative Advantage
RCA <- get_data360(indicator_id = 40085, country_iso3 = c('USA', 'CHN', 'RUS'))
test <- get_data360(indicator_id = 944, country_iso3 = 'USA')
40085

library(bit64)
# Gross exports
exports <- get_data360(indicator_id = 2327, country_iso3 = c('USA', 'CHN', 'RUS'))
exports <- get_data360(indicator_id = 2327)

library(datasets)
glimpse(WorldPhones)

head(WorldPhones)
-----
  
  library(wbstats)
new_wb_cache <- wbcache()
wb_indicators <- new_wb_cache$indicators
lu_country <- new_wb_cache$countries %>% filter(regionID == "EAS")
