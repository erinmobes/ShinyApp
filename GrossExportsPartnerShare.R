
# # Install necessary packages 
 install.packages(c('data360r', 'tidyverse', 'data.table', 'ggthemes', 'RColorBrewer', 'devtools', 'ggflags', 'ISOcodes', 'countrycode'))
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

# ggflags sample 
DF <- data.frame(country = c("UK", "France", "Germany"), 
                 value=c(4, 3, 7))
ggplot(DF, aes(x=country, country=country, y=value)) + geom_flag(size = 10)

data(lflags)
set.seed(1234)
d <- data.frame(x=rnorm(10), y=rnorm(10), 
                country=sample(c("ar","fr"), 10, TRUE), 
                stringsAsFactors = FALSE)
ggplot(d, aes(x=x, y=y, country=country, size=x)) + 
  geom_flag() + 
  scale_country()

# # Exports Data

# All indicators available in Data360
ind_all <- get_metadata360(site='tc', metadata_type = 'indicators')

# Indicators that are by partner
ind_partner <- get_metadata360(site='tc', metadata_type = 'indicators') %>%
  filter(byPartner == TRUE)
# Indicators of note: Exports = 2327, Imports = 2335

# Pull all data for an indiciator
df <- get_data360(site = "tc", indicator_id = 2353)

# Create a country lookup between ISO3 and Country Name for future use
lu_country <- df %>%
  select(`Country ISO3`, `Country Name`) %>%
  distinct_() %>%
  plyr::rename(c("Country Name" = "Partner Name"))

# Add Country Name as "Partner" based on the ISO3
df <- df %>%
  left_join(lu_country, by = c("Partner" = "Country ISO3"))

# Transform from wide data to long data
df <- melt(df)

# Rename variable as year
df <- df %>%
  plyr::rename(c("variable" = "Year"))

# Filter to vietnam and Competition Countris
country <- "VNM"
dat <- df %>%
  filter(`Country ISO3` == country , Partner %in% c('CHN', 'RUS', 'USA'))
country_long <- dat$`Country Name`[1]

# Convert factor year to integer year
dat$Year <- as.numeric(as.character(dat$Year))

dat$iso2 <- dat$Partner

glimpse(dat)
dat$iso2 <- countrycode(dat$iso2, 'iso3c', 'iso2c')
dat$iso2 <- tolower(dat$iso2)

# Select the points 
first_last_mid_point <-  dat %>%
  filter(Year %in% c(max(Year), min(Year), round(mean(Year))))

# Line Plot with flag points
plot <- ggplot(dat, aes(x=Year, y=value, country = iso2, group = Partner, stroke = "black")) + geom_line() + geom_flag(data = first_last_mid_point, size = 10, show.legend = T) + 
  theme_classic() +
  theme(legend.position = "top", panel.background = element_rect(fill = "lightgrey"), plot.background = element_rect(fill = "lightgrey"), legend.background = element_rect(fill = "lightgrey"))  + 
  labs(title = paste0(country_long, ": Share of Gross Exports by Country"), y = "% of Gross Exports", country = "") + 
  scale_country(labels=c("China", "Russia",  "United States")) 
plot

# Line plot with colored lines
plot <- ggplot(dat, aes(x=Year, y=value, color = Partner, group = Partner, country = iso2)) + geom_line() + theme_() + 
  scale_colour_manual(values=c(CHN="#7B3F00",RUS="orange3",USA="blue"))
plot
