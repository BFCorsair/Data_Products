library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Portfolio Returns 1928 - 2014"),
  sidebarPanel(
	h3("Select Investment Allocations"),
    sliderInput('stocks', 'Stocks',value = 50, min = 0, max = 100, step = 0.5,),
  	sliderInput('bonds', 'Bonds',value = 50, min = 0, max = 100, step = 0.5,)
),
  mainPanel(
  	h2(textOutput('msg')),
    tableOutput("values"),
    plotOutput('Portfolio', height="600px")
  )
))
