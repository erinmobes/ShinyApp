
# # Install necessary packages 
# install.packages(c('data360r', 'tidyverse', 'data.table', 'ggthemes', 'RColorBrewer', 'devtools', 'ggflags', 'ISOcodes', 'countrycode'))
# devtools::install_github("YTLogos/ggflags")

library(data360r) #database of indicators
library(tidyverse)
library(data.table) #melt
library(ggthemes)
library(RColorBrewer)
library(devtools)
library(ggflags)
library(ISOcodes)
library(countrycode)

# # ggflags sample 
# DF <- data.frame(country = c("UK", "France", "Germany"), 
#                  value=c(4, 3, 7))
# ggplot(DF, aes(x=country, country=country, y=value)) + geom_flag(size = 10)
# 
# data(lflags)
# set.seed(1234)
# d <- data.frame(x=rnorm(10), y=rnorm(10), 
#                 country=sample(c("ar","fr"), 10, TRUE), 
#                 stringsAsFactors = FALSE)
# ggplot(d, aes(x=x, y=y, country=country, size=x)) + 
#   geom_flag() + 
#   scale_country()

# # Exports Data

# All indicators available in Data360
ind_all <- get_metadata360(site='tc', metadata_type = 'indicators')

df_sum <- get_data360(indicator_id = c(
28609, # GDP 
515, # exports as a % of gdp
507 # imports as a % of gdp
), timeframes = 2014)

df_sum <- get_data360(indicator_id = 515, output_type = "long") # exports as a % of gdp
df_sum[,1:4, -1]
test <- df_sum %>% select(c(1:4,-1))
df_sum[,c(1:4)&c(-1)]
glimpse(df_sum)

blah <- df_sum[,-1]
glimpse(blah)

df_sum$value <- df_sum$`2014`
df_sum <- df_sum %>% select(`Country Name`, Indicator, value)
glimpse(df_sum2)
setDT(df_sum)
df_sum2 <- dcast(df_sum, `Country Name` ~ Indicator, value.var = "value")

# Indicators that are by partner
ind_partner <- get_metadata360(site='tc', metadata_type = 'indicators') %>%
  filter(byPartner == TRUE)
# Indicators of note: Exports = 2327, Imports = 2335

# Pull all data for an indiciator
df_export <- get_data360(indicator_id = 2353,  output_type = 'long')
df_import <- get_data360(indicator_id = 2360,  output_type = 'long')

df <- rbind(df_export, df_import)

# Create a country lookup between ISO3 and Country Name for future use
lu_country <- df %>%
  select(`Country ISO3`, `Country Name`) %>%
  distinct_() %>%
  plyr::rename(c("Country Name" = "Partner Name"))

# Add Country Name as "Partner" based on the ISO3
df <- df %>%
  left_join(lu_country, by = c("Partner" = "Country ISO3"))
glimpse(dat)
# Rename variable as year
df <- df %>%
  plyr::rename(c("Period" = "Year"))

# Filter to Competition Countries
dat <- df %>%
  filter(Partner %in% c('CHN', 'RUS', 'USA'))

# Convert factor year to integer year
dat$Year <- as.numeric(as.character(dat$Year))

dat$iso2 <- dat$Partner
dat$iso2 <- countrycode(dat$iso2, 'iso3c', 'iso2c')
dat$iso2 <- tolower(dat$iso2)

# Select the points 
first_last_mid_point <-  dat %>%
  filter(Year %in% c(max(Year), min(Year), round(mean(Year))))

df_sum

dat_last <- dat %>%
 filter(Year == max(Year))

glimpse(dat)
glimpse(df_sum)


df_sum2 <- dat_last %>% left_join(df_sum)

glimpse(df_sum2)

# # Line Plot with flag points
# plot <- ggplot(dat, aes(x=Year, y=value, country = iso2, group = Partner, stroke = "black")) + geom_line() + geom_flag(data = first_last_mid_point, size = 10, show.legend = T) + 
#   theme_classic() +
#   theme(legend.position = "top", panel.background = element_rect(fill = "lightgrey"), plot.background = element_rect(fill = "lightgrey"), legend.background = element_rect(fill = "lightgrey"))  + 
#   labs(title = paste0(country_long, ": Share of Gross Exports by Country"), y = "% of Gross Exports", country = "") + 
#   scale_country(labels=c("China", "Russia",  "United States")) 
# plot

# # Line plot with colored lines
# plot <- ggplot(dat, aes(x=Year, y=value, color = Partner, group = Partner, country = iso2)) + geom_line()  + 
#   scale_colour_manual(values=c(CHN="#7B3F00",RUS="orange3",USA="blue"))
# plot


