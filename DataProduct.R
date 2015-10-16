library(gdata)
library(utils)
library(dplyr)
library(reshape2)
library(grid)
library(gridExtra)

# Retrieve the Excel file and load it into a data frame
if (! file.exists('histretSP.xls')) {
	download.file('http://www.stern.nyu.edu/~adamodar/pc/datasets/histretSP.xls', 'histretSP.xls', method='wget')
} else {
	message("File already exists - skipping the download")
}
df <- readWorksheetFromFile('histretSP.xls', sheet=1, startRow = 18)
# Keep years 1928 - 2014, included
df <- df[1:87,]
df <- df[,1:4]
df <- mutate(df, Year = as.integer(df2$Year))
max_scale <- 0.1* (1+ round(10*max(abs(df$S.P.500),0)))
# Plot historical data
df1 <- df
colnames(df1) <- c('Year','S & P 500', '3-month T-Bills', '10-year Bonds')
meltdf <- melt(df1, id='Year')
colnames(meltdf) <- c('Year', "Investment_Class", "Returns")
q <- ggplot(data=meltdf, aes(x=Year, y=Returns, group=Investment_Class, colour=Investment_Class, height=0.1))
q <- q + geom_line(aes())+geom_point()
q <- q + ylim(-max_scale, max_scale)
q <- q + theme(legend.position = "bottom")
q <- q	+ ggtitle("Returns for 3 Main Investment Classes")
q

# Compute & plot portfolio
stocks <- 0.5
bonds <- 0.3
bills <- 1.0 - (stocks + bonds)
df2 <- mutate(df, Portfolio = stocks*df$S.P.500+bills*df$X3.month.T.Bill+bonds*df$X10.year.T..Bond)
colnames(df2) <- c('Year','S & P 500', '3-month T-Bills', '10-year Bonds', 'Portfolio')
g <- ggplot(df2, aes(x=Year, group=1))
g <- g +  geom_line(aes(y=Portfolio),size=1.5,colour="black", alpha = 1.0)
# g <- g + scale_color_continuous()
g <- g + ylim(-max_scale, max_scale) + ggtitle("Returns for Blended Portfolio")

grid.arrange(grobs=list(g, q), ncol=1, nrow=2, heights=c(1, .75))


# alpha_val = 0.5
# line_type = 'solid'
# size_val = 1
# # Plot the 3 rates over the years
# g <- ggplot(df2, aes(x=Year, group=1))
# g <- g+ geom_line(aes(y=S.P.500),size=size_val,colour="blue", linetype=line_type, alpha = alpha_val)
# g <- g+ geom_line(aes(y=X3.month.T.Bill),size=size_val,colour="red", linetype=line_type, alpha = alpha_val)
# g <- g+ geom_line(aes(y=X10.year.T..Bond),size=size_val,colour="chartreuse4", linetype=line_type, alpha = alpha_val)
# g <- g+ geom_line(aes(y=Folio),size=1.5,colour="black", alpha = 1.0)
# g <- g+scale_x_continuous(limits=c(1928,2014), breaks = c(1928, 1950, 1975, 2000, 2014), labels=c('Stocks', "Bills", 'Bonds', 'Portfolio'))
# g

# df4 <- df2
# colnames(df4) <- c('Year','S & P 500', '3-month T-Bills', '10-year Bonds', 'Portfolio')
# meltdf <- melt(df4, id='Year')
# colnames(meltdf) <- c('Year', "Investment", "Returns")
# q <- ggplot(data=meltdf, aes(x=Year, y=Returns, group=Investment, colour=Investment))
# q <- q + geom_line(aes())+geom_point()
# q


# Unsuccessful Attempt at making the size of Portfolio bigger
# mm <- mutate(meltdf, size=ifelse(meltdf$Investment == 'Folio',1.5,1))
# q <- ggplot(data=mm, aes(x=Year, y=Returns, group=Investment, colour=Investment,size=size))
# q <- q + geom_line(aes())+geom_point()
# q