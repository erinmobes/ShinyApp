library(wbstats)
library(data.table)
library(googleVis)
library(shiny)
library(rsconnect)
library(shinydashboard)
# blahhh
## bllaaahhh

# Download World Bank data and turn into data.table
myDT <- data.table(
  wb(indicator = c(
    "SP.POP.TOTL", # Population
    #   "SP.DYN.LE00.IN", # Life expectancy at birth, total (years)
    "CC.EST", # Control of Corruption: Estimate
    # Refugees
    #   "SM.POP.REFG",	# Refugee population by country or territory of asylum
    #   "SM.POP.REFG.OR",	# Refugee population by country or territory of origin
    #  "6.0.GDP_current", # GDP Current US Dollars
    #   "6.0.GDPpc_constant", #GDP per capita, PPP (constant 2011 international $
    "NY.GDP.MKTP.KD", #GDP (constant 2010 US$)
    "GB.XPD.DEFN.GDP.ZS", # Defense expenditure (% of GDP)
    "MS.MIL.MPRT.KD",	# Arms imports (SIPRI trend indicator values)
    "MS.MIL.XPND.ZS",	# Military expenditure (% of central government expenditure)
    "MS.MIL.XPND.GD.ZS"	# Military expenditure (% of GDP)
  )
  , mrv = 60)
)
# Download country mappings
countries <- data.table(wbcountries())
# Set keys to join the data sets
setkey(myDT, iso2c)
setkey(countries, iso2c)
# Add regions to the data set, but remove aggregates
myDT <- countries[myDT][ ! region %in% "Aggregates"]
# Reshape data into a wide format
wDT <- reshape(
  myDT[, list(
    country, region, date, value, indicator)],
  v.names = "value",
  idvar=c("date", "country", "region"),
  timevar="indicator", direction = "wide")
# Turn date, here year, from character into integer
wDT[, date := as.integer(date)]
setnames(wDT, names(wDT),
         c("Country", "Region",
           "Year",  "Population",
           "Trust in Gov","GDP",
           "Def Exp % GDP","Arms Imports",
           "Mil Exp % GDP", "Mil Exp % Gov Exp"
         ))

ui = shinyUI(fluidPage(
  titlePanel("Analysis Test"),
  mainPanel(
    navlistPanel(
      tabPanel("MotionChart",h1("Motion Chart"),tableOutput("motionchart2")),
      tabPanel("Phones", h1("Phone Chart"),
               sidebarLayout(      
                 
                 # Define the sidebar with one input
                 sidebarPanel(
                   selectInput("region", "Region:", 
                               choices=colnames(WorldPhones)),
                   hr(),
                   helpText("Data from AT&T (1961) The World's Telephones.")
                 ),
                 
                 # Create a spot for the barplot
                 mainPanel(
                   plotOutput("phonePlot")  
                   
                 )))))))

server = shinyServer(function(input, output, session) {
  
  output$motionchart2 <- renderGvis({
    gvisMotionChart(wDT, idvar = "Country",
                    timevar = "Year",
                    xvar = "Mil Exp % Gov Exp",
                    yvar = "Trust in Gov",
                    sizevar = "GDP",
                    colorvar = "Region")
  })
  
  output$phonePlot <- renderPlot({
    
    # Render a barplot
    barplot(WorldPhones[,input$region]*1000, 
            main=input$region,
            ylab="Number of Telephones",
            xlab="Year")
  })
  
})

barplot(WorldPhones[,'Oceania']*1000, 
        main='Oceania',
        ylab="Number of Telephones",
        xlab="Year")

# setwd('C:/Users/1274806318A/Desktop/App')

# rsconnect::deployApp('C:/Users/1274806318A/Desktop/App', appName = 'Competition')

# Deploy app from home
# rsconnect::deployApp(getwd(), appName = 'Competition')
shinyApp(ui = ui, server = server)

## Run app locally
# shiny::runApp(list(ui = ui, server = server), port = 80, launch.browser = FALSE, interactive(), host = getOption("shiny.host", "0.0.0.0")) 
