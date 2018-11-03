library(wbstats)
library(data.table)
library(googleVis)
library(shiny)
library(rsconnect)
library(shinydashboard)
library(data360r) #database of indicators
library(tidyverse)
library(data.table) #melt
library(ggthemes)
library(RColorBrewer)
library(devtools)
library(ggflags)
library(ISOcodes)
library(countrycode)

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

# Testing dropdown
dat <- dat %>%
  filter(`Country Name` %in% c('Australia', 'Vietnam', 'Thailand'))

# Line

ui = shinyUI(fluidPage(
  titlePanel("Exports Test"),
  mainPanel(
    navlistPanel(
      tabPanel("Share of Exports", h1("Exports Chart"),
               sidebarLayout(      
                 
                 # Define the sidebar with one input
                 sidebarPanel(
                   selectInput("country", "Country:", 
                               choices=dat$`Country Name`,
                               selected = ""),
                   hr(),
                   helpText("Gross Exports, percent to each Competition Country, World Bank Data")
                 ),
                 
                 # Create a spot for the barplot
                 mainPanel(
                   plotOutput("exportsPlot")  
                   
                 )))))))

server = shinyServer(function(input, output, session) {
  
  output$exportsPlot <- renderPlot({
    
    # Render a barplot
    ggplot(dat[dat$`Country Name` == input$country,], aes(x=Year, y=value, country = iso2, group = Partner, stroke = "black")) + 
      geom_line() + 
      geom_flag(data = first_last_mid_point[first_last_mid_point$`Country Name` == input$country, ], size = 10, show.legend = T) + 
      theme_classic() +
      theme(legend.position = "top", panel.background = element_rect(fill = "lightgrey"), plot.background = element_rect(fill = "lightgrey"), legend.background = element_rect(fill = "lightgrey"))  + 
      labs(title = paste0(input$country, ": Share of Gross Exports by Country"), y = "% of Gross Exports", country = "") + 
      scale_country(labels=c("China", "Russia",  "United States")) 

  })
})

shinyApp(ui = ui, server = server)

## Deploy app from work
# setwd('C:/Users/1274806318A/Desktop/App')
# rsconnect::deployApp('C:/Users/1274806318A/Desktop/App', appName = 'Competition')

## Deploy app from home
# rsconnect::deployApp(getwd(), appName = 'Competition')

## Run app locally
# shiny::runApp(list(ui = ui, server = server), port = 80, launch.browser = FALSE, interactive(), host = getOption("shiny.host", "0.0.0.0")) 
