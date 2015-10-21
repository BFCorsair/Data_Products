library(shiny)
library(gdata)
library(utils)
library(dplyr)
library(reshape2)
library(grid)
library(gridExtra)
library(ggplot2)

# ---- Helper Functions
# compute basic stats for an index or portfolio
compute_stats <- function(vct) {
  c( round(mean(vct),2), 
     round(sd(vct),2),
     round(min(vct),2),
     round(max(vct),2) )
}

# Function to remove the % character from the 3 columns
rmpct <- function(x) { gsub('%','',x)}
# ----

# Retrieve the Excel file and load it into a data frame
if (! file.exists('histretSP.xls')) {
  download.file('http://www.stern.nyu.edu/~adamodar/pc/datasets/histretSP.xls', 'histretSP.xls', method='wget')
} else {
  message("File already exists - skipping the download")
}
df <- read.xls('histretSP.xls', sheet=1, stringsAsFactors = FALSE)
# Skip the header rows, and only keep the first 4 columns: Year and 3 main indices
df <- df[16:nrow(df), 1:4]
# Remove the unused bottom rows
df <- df[1:87,]
colnames(df) <- c('Year','S.P.500', 'X3.month.T.Bills', 'X10.year.Bonds')

xx <- lapply(df[,2:4],rmpct)
yy <- lapply(xx, as.numeric)
zz <- data.frame(df$Year)
colnames(zz) <-'Year'
zz <- mutate(zz, Year=as.integer(as.character(zz$Year)))
dfx <- cbind(zz,yy)
# Rename columns
colnames(dfx) <- c('Year','S.P.500', 'X3.month.T.Bills', 'X10.year.Bonds')
# max_scale ensures that all plots are on the same scale
max_scale <- 10* (1+round(0.1*max(abs(dfx$S.P.500),0)))

# Default: 50% stocks & 50% bonds
default_stocks <- 50 # percent
default_bonds <- 50 # percent


shinyServer(function(input, output) {
  # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    stocks <- input$stocks
    bonds <- input$bonds
    # Note the {} to execute R code
    if (stocks+bonds > 100.0) {  # Must stay under 100%
      output$msg <- renderText("Combined allocation of stocks and bonds must be less than 100 percent")
      stocks <- default_stocks
      bonds <- default_bonds
      # Reference: http://shiny.rstudio.com/reference/shiny/latest/validate.html
      # validate(
      #   need(stocks+bonds <= 100.0, "Combined allocation of stocks and bonds must be less than 100%")
      # )
    } else { # All good
      output$msg <- renderText('New Portfolio')
    }
    bills <- 100.0 - (stocks + bonds)



    # Compose data frame
    data.frame(
      'Investment Type' = c("Stocks %", 
               "Bonds %",
               "Bills %"),
      Percentage = as.character(c(stocks, 
                             bonds,
                             bills)), 
      stringsAsFactors=FALSE)
  })

  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })

  
  output$Portfolio <- renderPlot({
    message('renderPlot')
    # Compute & plot portfolio
    df2 <- mutate(dfx, Portfolio = stocks*dfx$S.P.500+bills*dfx$X3.month.T.Bills+bonds*dfx$X10.year.Bonds)
    message(df2[1,])
    colnames(df2) <- c('Year','S & P 500', '3-month T-Bills', '10-year Bonds', 'Portfolio')
    g <- ggplot(df2, aes(x=Year, group=1))
    g <- g +  geom_line(aes(y=Portfolio),size=1.5,colour="black", alpha = 1.0)
    # g <- g + scale_color_continuous()
    g <- g + ylim(-max_scale, max_scale) + ggtitle("Returns for Blended Portfolio")
    g
  })    
  output$Indices <- renderPlot({
    # Plot historical data
    df1 <- dfx
    colnames(df1) <- c('Year','S & P 500', '3-month T-Bills', '10-year Bonds')
    meltdf <- melt(df1, id='Year')
    colnames(meltdf) <- c('Year', "Investment_Class", "Returns")
    q <- ggplot(data=meltdf, aes(x=Year, y=Returns, group=Investment_Class, colour=Investment_Class, height=0.1))
    q <- q + geom_line(aes())+geom_point()
    q <- q + ylim(-max_scale, max_scale)
    q <- q + theme(legend.position = "bottom")
    q <- q  + ggtitle("Returns for 3 Main Investment Classes")
    q
  })


   # Compute Portofolio Metrics
  output$indices_metrics <- renderTable({
    data.frame(
      'Portofolio Metrics' = c("Average Return", 
               "Standard Deviation",
               "Lowest Yearly Return",
               "Highest Yeaarly Return"),
      'S & P 500' = as.character(compute_stats(df1[,2])), 
      '3-month T-Bills' = as.character(compute_stats(df1[,3])), 
      '10-year Bonds' = as.character(compute_stats(df1[,4])), 
      stringsAsFactors=FALSE)  
    })



  # Compute Portofolio Metrics
  output$metrics <- renderTable({
    data.frame(
      'Portofolio Metrics' = c("Average Return", 
               "Standard Deviation",
               "Lowest Yearly Return",
               "Highest Yeaarly Return"),
      Percentage = as.character(compute_stats(df2$Portfolio)),
      stringsAsFactors=FALSE)  
    })
})

