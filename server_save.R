library(shiny)
library(gdata)
library(utils)
library(dplyr)
library(reshape2)
library(grid)
library(gridExtra)

# Retrieve the Excel file and load it into a data frame
download.file('http://www.stern.nyu.edu/~adamodar/pc/datasets/histretSP.xls', 'histretSP.xls', method='wget')
df <- readWorksheetFromFile('histretSP.xls', sheet=1, startRow = 18)
# Keep years 1928 - 2014, included
df <- df[1:87,]
df <- df[,1:4]
df <- mutate(df, Year = as.integer(df$Year))


shinyServer(function(input, output) {
  # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    stocks <- input$stocks
    bonds <- input$bonds
    bills <- 100.0 - (stocks + bonds)

    # Compose data frame
    data.frame(
      Name = c("Stocks %", 
               "Bonds %",
               "Bills %"),
      Value = as.character(c(input$stocks, 
                             input$bonds,
                             bills)), 
      stringsAsFactors=FALSE)
  })

  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })


  output$Portfolio <- renderPlot({
    df1 <- df
    colnames(df1) <- c('Year','S & P 500', '3-month T-Bills', '10-year Bonds')
    meltdf <- melt(df1, id='Year')
    colnames(meltdf) <- c('Year', "Investment", "Returns")
    q <- ggplot(data=meltdf, aes(x=Year, y=Returns, group=Investment, colour=Investment))
    q <- q + geom_line(aes())+geom_point()
    q


    # Compute & plot portfolio
    stocks <- input$stocks
    bonds <- input$bonds
    bills <- 1.0 - (stocks + bonds)
    df2 <- mutate(df, Portfolio = 0.01 * (stocks*df$S.P.500+bills*df$X3.month.T.Bill+bonds*df$X10.year.T..Bond))
    colnames(df2) <- c('Year','S & P 500', '3-month T-Bills', '10-year Bonds', 'Portfolio')
    g <- ggplot(df2, aes(x=Year, group=1))
    g <- g+ geom_line(aes(y=Portfolio),size=1.5,colour="black", alpha = 1.0)
    g

    grid.arrange(grobs=list(g, q), ncol=1, nrow=2)

  })
})

