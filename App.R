library(wbstats)
library(data.table)
library(googleVis)
library(shiny)
library(rsconnect)
library(shinydashboard)
library(foreign)
library(ggplot2)
library(plotly)

# blahhh
## bllaaahhh
# 
# # Download World Bank data and turn into data.table
# myDT <- data.table(
#   wb(indicator = c(
#     "SP.POP.TOTL", # Population
#     #   "SP.DYN.LE00.IN", # Life expectancy at birth, total (years)
#     "CC.EST", # Control of Corruption: Estimate
#     # Refugees
#     #   "SM.POP.REFG",	# Refugee population by country or territory of asylum
#     #   "SM.POP.REFG.OR",	# Refugee population by country or territory of origin
#     #  "6.0.GDP_current", # GDP Current US Dollars
#     #   "6.0.GDPpc_constant", #GDP per capita, PPP (constant 2011 international $
#     "NY.GDP.MKTP.KD", #GDP (constant 2010 US$)
#     "GB.XPD.DEFN.GDP.ZS", # Defense expenditure (% of GDP)
#     "MS.MIL.MPRT.KD",	# Arms imports (SIPRI trend indicator values)
#     "MS.MIL.XPND.ZS",	# Military expenditure (% of central government expenditure)
#     "MS.MIL.XPND.GD.ZS"	# Military expenditure (% of GDP)
#   )
#   , mrv = 60)
# )
# # Download country mappings
# countries <- data.table(wbcountries())
# # Set keys to join the data sets
# setkey(myDT, iso2c)
# setkey(countries, iso2c)
# # Add regions to the data set, but remove aggregates
# myDT <- countries[myDT][ ! region %in% "Aggregates"]
# # Reshape data into a wide format
# wDT <- reshape(
#   myDT[, list(
#     country, region, date, value, indicator)],
#   v.names = "value",
#   idvar=c("date", "country", "region"),
#   timevar="indicator", direction = "wide")
# # Turn date, here year, from character into integer
# wDT[, date := as.integer(date)]
# setnames(wDT, names(wDT),
#          c("Country", "Region",
#            "Year",  "Population",
#            "Trust in Gov","GDP",
#            "Def Exp % GDP","Arms Imports",
#            "Mil Exp % GDP", "Mil Exp % Gov Exp"
#          ))

source(file = file.path(getwd(), "GrossExportsPartnerShare.R"))

ui <- dashboardPage(
  dashboardHeader(title = "Basic Dashboard"),
  dashboardSidebar(
    menuItem("Menu1", icon = icon("dashboard")),
    menuItem("Menu2", icon = icon("dashboard"))
    ),
  dashboardBody(
    fluidRow(
      selectInput("dropdown", "Export Country of Comparison", choices = dat$`Country Name`, selected =""),
      box(
        plotlyOutput("graph1")
        ),
      box(
        plotlyOutput("graph2")
      )
    )
  )
)

server <- function (input, output, session){
  
  
  output$graph1 <- renderPlotly({
    print(
      ggplotly(
        ggplot(dat[dat$`Country Name`== input$dropdown,], aes(x=Year, y=value, group = `Partner Name`, color = `Partner Name`, width = 6)) + 
          geom_line() + 
          scale_color_manual(name = "Partners", values=c("#7B3F00", "#FF3300", "#0066CC")) +
          theme_classic() +
          theme(legend.position = "top", 
                legend.background = element_rect(fill = "lightgrey"))  + 
          labs(title = paste0(input$dropdown, ": Share of Gross Exports by Country"), y = "% of Gross Exports")
      )
    )
  }
  )
  
  output$graph2 <- renderPlotly({
    print(
      ggplotly(
        ggplot(dat[dat$`Country Name`== input$dropdown,], aes(x=Year, y=value, group = `Partner Name`, color = `Partner Name`, width = 6)) + 
          geom_line() + 
          scale_color_manual(name = "Partners", values=c("#7B3F00", "#FF3300", "#0066CC")) +
          theme_classic() +
          theme(legend.position = "top", 
                legend.background = element_rect(fill = "lightgrey"))  + 
          labs(title = paste0(input$dropdown, ": Share of Gross Exports by Country"), y = "% of Gross Exports")
      )
    )
  }
  )
}


# setwd('C:/Users/1274806318A/Desktop/App')

# rsconnect::deployApp('C:/Users/1274806318A/Desktop/App', appName = 'Competition')

# Deploy app from home
# rsconnect::deployApp(getwd(), appName = 'Competition')
shinyApp(ui = ui, server = server)

## Run app locally
# shiny::runApp(list(ui = ui, server = server), port = 80, launch.browser = FALSE, interactive(), host = getOption("shiny.host", "0.0.0.0")) 
