library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Portfolio Returns 1928 - 2014"),
  sidebarPanel(
	h3("Select Investment Allocations"),
    sliderInput('stocks', 'Stocks',value = 50, min = 0, max = 100, step = 0.5,),
  	sliderInput('bonds', 'Bonds',value = 50, min = 0, max = 100, step = 0.5,)
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
