library(gdata)
library(utils)
library(dplyr)
library(reshape2)
library(grid)
library(gridExtra)
library(ggplot2)

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

# Function to remove the % character from the 3 columns
rmpct <- function(x) { gsub('%','',x)}
xx <- lapply(df[,2:4],rmpct)
yy <- lapply(xx, as.numeric)
zz <- data.frame(df$Year)
colnames(zz) <-'Year'
zz <- mutate(zz, Year=as.integer(as.character(zz$Year)))
dfx <- cbind(zz,yy)
# Rename columns
colnames(dfx) <- c('Year','S.P.500', 'X3.month.T.Bills', 'X10.year.Bonds')


max_scale <- 10* (1+round(0.1*max(abs(dfx$S.P.500),0)))
# Plot historical data
df1 <- dfx
colnames(df1) <- c('Year','S & P 500', '3-month T-Bills', '10-year Bonds')
meltdf <- melt(df1, id='Year')
colnames(meltdf) <- c('Year', "Investment_Class", "Returns")
q <- ggplot(data=meltdf, aes(x=Year, y=Returns, group=Investment_Class, colour=Investment_Class, height=0.1))
q <- q + geom_line(aes())+geom_point()
q <- q + ylim(-max_scale, max_scale)
q <- q + theme(legend.position = "bottom")
q <- q	+ ggtitle("Returns for 3 Main Investment Classes")
q

