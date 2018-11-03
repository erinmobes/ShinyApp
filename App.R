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
      selectInput("dropdown", "Country of Comparison", choices = dat$`Country Name`, selected =""),
      box(
        plotlyOutput("exports")
        ),
      box(
        plotlyOutput("imports")
      )
    )
  )
)

server <- function (input, output, session){
  
  
  output$exports <- renderPlotly({
    print(
      ggplotly(
        ggplot(dat %>% filter(`Country Name`== input$dropdown, dat$Indicator=="Gross exports, partner shares"), aes(x=Year, y=Observation, group = `Partner Name`, color = `Partner Name`, width = 6)) + 
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
 #dat[dat$`Country Name`== input$dropdown&dat$Indicator=="Gross imports, partner shares",] 
  output$imports <- renderPlotly({
    print(
      ggplotly(
        ggplot(dat %>% filter(`Country Name`== input$dropdown, dat$Indicator=="Gross imports, partner shares"), aes(x=Year, y=Observation, group = `Partner Name`, color = `Partner Name`, width = 6)) + 
          geom_line() + 
          scale_color_manual(name = "Partners", values=c("#7B3F00", "orange3", "#0066CC")) +
          theme_classic() +
          theme(legend.position = "top", 
                legend.background = element_rect(fill = "lightgrey"))  + 
          labs(title = paste0(input$dropdown, ": Share of Gross Imports by Country"), y = "% of Gross Imports")
      )
    )
  }
  )
  
# 
#     output$graph1 <- renderPlotly(
#       plot_ly(dat[dat$`Country Name`== input$dropdown,], aes(x=Year, y=value, country = iso2, group = Partner, stroke = "black")) + 
#         geom_line() + geom_flag(data = first_last_mid_point[first_last_mid_point$`Country Name` == input$dropdown,], 
#         size = 10, show.legend = T) + 
#         theme_classic() +
#         theme(legend.position = "top", panel.background = element_rect(fill = "lightgrey"), plot.background = element_rect(fill = "lightgrey"), legend.background = element_rect(fill = "lightgrey"))  + 
#         labs(title = paste0(input$dropdown, ": Share of Gross Exports by Country"), y = "% of Gross Exports", country = "") + 
#         scale_country(labels=c("China", "Russia",  "United States"))
#               )
}


# 
# ui = shinyUI(fluidPage(
#   titlePanel("Analysis Test"),
#   mainPanel(
#     navlistPanel(
#       tabPanel("MotionChart",h1("Motion Chart"),tableOutput("motionchart2")),
#       tabPanel("Phones", h1("Phone Chart"),
#                sidebarLayout(      
#                  
#                  # Define the sidebar with one input
#                  sidebarPanel(
#                    selectInput("region", "Region:", 
#                                choices=colnames(WorldPhones)),
#                    hr(),
#                    helpText("Data from AT&T (1961) The World's Telephones.")
#                  ),
#                  
#                  # Create a spot for the barplot
#                  mainPanel(
#                    plotOutput("phonePlot")  
#                    
#                  )))))))
# 
# server = shinyServer(function(input, output, session) {
#   
#   output$motionchart2 <- renderGvis({
#     gvisMotionChart(wDT, idvar = "Country",
#                     timevar = "Year",
#                     xvar = "Mil Exp % Gov Exp",
#                     yvar = "Trust in Gov",
#                     sizevar = "GDP",
#                     colorvar = "Region")
#   })
# })

# setwd('C:/Users/1274806318A/Desktop/App')

# rsconnect::deployApp('C:/Users/1274806318A/Desktop/App', appName = 'Competition')

# Deploy app from home
# rsconnect::deployApp(getwd(), appName = 'Competition')
shinyApp(ui = ui, server = server)

## Run app locally
# shiny::runApp(list(ui = ui, server = server), port = 80, launch.browser = FALSE, interactive(), host = getOption("shiny.host", "0.0.0.0")) 
