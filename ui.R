#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Climate Data"),
  # Include radio buttons  (Only one of these can be selected)
  # These radioButtons are used in the server as an input variable called
  # 'dataSource'
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("data","Select datasets:",choices = c("Vancouver Temperature","Vancouver Snow","Vancouver CO2", "Vancouver Rainfall")),
      conditionalPanel(condition="input.data=='Vancouver Temperature'",
      sliderInput("bins",  #This is the variable name, the value of which is determined by the slider.
                  "slide to view temperature in months:",
                  min = 1,
                  max = 12,
                  value = 1,
                  step=1),
      checkboxInput("check4","Fit a line",FALSE),
      selectInput("season","Select season", choices=c("Spring","Summer","Autumn","Winter")),
      checkboxInput("check","Fit a line",FALSE),
      checkboxInput("check3","show all season?",FALSE)),
      
      
      
      conditionalPanel(condition="input.data=='Vancouver CO2'",
                       sliderInput("bins3",
                                    "CO2 over the years",
                                    min=1979,
                                    max=2018,
                                    value=2018,
                                    step=1)),
      
      conditionalPanel(condition="input.data=='Vancouver Snow'",
                       checkboxInput("check50","View boxplot",FALSE)),
      
      conditionalPanel(condition="input.data=='Vancouver Rainfall'", 
      sliderInput("bins2",  #This is the variable name, the value of which is determined by the slider.
                                   "slide to view temperature in months:",
                                   min = 1,
                                   max = 12,
                                   value = 1,
                                   step=1),
     checkboxInput("check42","Fit a line",FALSE),
     selectInput("season2","Select season", choices=c("Spring","Summer","Autumn","Winter")),
     checkboxInput("check43","Fit a line",FALSE),
     checkboxInput("check44","show all season?",FALSE))
      
      
      ),
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Users",conditionalPanel(condition="input.data=='Vancouver Temperature'", 
                                                    h3("Slide the slider to view the temperature in different months over the years"),
                                                    plotOutput("distPlot"),
                                                    h3("Seasonal temperature in different year"),
                                                    plotOutput("seasonplot")),
                           conditionalPanel(condition="input.data=='Vancouver Rainfall'",
                                                    h3("Slide the slider to view the precipitation in different months over the years"),
                                                    plotOutput("distPlot3"),
                                                    h3("Seasonal precipitation"),
                                                    plotOutput("seasonplot2")),
                           
                           conditionalPanel(condition="input.data=='Vancouver Snow'",
                                            h3("Vancouver snowfall in the history"),
                                            plotOutput("snowplot"),
                                            h5("Note:"),
                                            helpText("The red dots are the outliers")),
  
                           conditionalPanel(condition="input.data=='Vancouver CO2'",
                                                    h3("Vancouver CO2 over the years"),
                                                    plotOutput("CO2plot")
                                                  
                                                    )),
                  
                  tabPanel("Findings",h5("From this plot, I find that in all four seasons, the average temperature is gradually increasing year after year. Each set of data is a representative to one season, and each trend line is upward sloping. Therefore, our climate is becoming warmer and warmer."),
                           plotOutput("distplot2"),h5("In this plot below, Co2 is increasing dramatically over time with an estimated slope of 1.772 which means that the average Co2 increases 1.772 ppm every year. This might be one of the primary reason that causes global warming. From the plot, we can see that Co2 is fluctuating in in a regular pattern. It seems like in some certain months, the Co2 decreases and increases in some other months. However, the overall pattern is increasing."),
                           plotOutput("Co2"),h5("From the plots below, it seems like the temperature has positive linear relationships with rainfall, snowfall, and Co2. Therefore, we can expect that the climate will become hotter, more snow, more rain, and more Co2 in the air in the future."),plotOutput("temp")),
                  tabPanel("About",source("about.R")$value()),
                  tabPanel("Reference",h3("Reference"),
                           h6(("Dlugokencky, E.J., K.W. Thoning, P.M. Lang, and P.P. Tans (2017), NOAA Greenhouse Gas Reference from Atmospheric Carbon Dioxide Dry Air Mole Fractions from the NOAA ESRL Carbon Cycle Cooperative Global Air Sampling Network. Data Path: ftp: //aftp.cmdl.noaa.gov/data/trace_gases/co2/flask/surface/")),
                           h6((("Vincent, L. A., X. L. Wang, E. J. Milewska, H. Wan, F. Yang, and V. Swail, 2012. A second generation of homogenized Canadian monthly surface air temperature for climate trend analysis, J. Geophys. Res., 117, D18110, doi:10.1029\\/2012JD017859."))),
                           h6("Wan, H., X. L. Wang, V. R. Swail, 2007: A quality assurance system for Canadian hourly pressure data. J. Appl. Meteor. Climatol., 46, 1804-1817. "),
                           h6("Wang, X.L, Y. Feng, L. A. Vincent, 2013. Observed changes in one-in-20 year extremes of Canadian surface air temperatures. Atmosphere-Ocean. Doi:10.1080\\/07055900.2013.818526."),
                           h6(("Mekis, É. and L.A. Vincent, 2011: An overview of the second generation adjusted daily precipitation dataset for trend analysis in Canada. Atmosphere-Ocean, 49(2), 163-177."))))
      
      
    )
    
  ))
)