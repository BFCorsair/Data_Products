library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Portfolio Returns 1928 - 2014"),
  sidebarPanel(
	helpText("Select Investment Allocations"),
  sliderInput('stocks', 'Stocks',value = 50, min = 0, max = 100, step = 0.5, post = '%'),
	sliderInput('bonds', 'Bonds',value = 50, min = 0, max = 100, step = 0.5, post = '%'),
  hr(),
  helpText("Select the start and end years"),
  sliderInput('dates',"Start and End Dates", min=1928, max = 2014, step = 1, sep = '', value = c(1928, 2014))
),
  mainPanel(
  	h3(textOutput('msg')),
    tableOutput("values"),
  	h3("Returns of Blended Portfolio"),
    plotOutput('Portfolio', height="400px"),
    tableOutput("metrics"),
    h3("Returns of 3 Main Investment Classes"),
    plotOutput('Indices', height="400px"),
    tableOutput("indices_metrics")

  )
))
