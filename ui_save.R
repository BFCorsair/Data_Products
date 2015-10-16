library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Example plot"),
  sidebarPanel(
    sliderInput('stocks', 'Stocks',value = 50, min = 0, max = 100, step = 0.5,),
  	sliderInput('bonds', 'Bonds',value = 50, min = 0, max = 100, step = 0.5,)
),
  mainPanel(
    tableOutput("values"),
    plotOutput('Portfolio')
  )
))
