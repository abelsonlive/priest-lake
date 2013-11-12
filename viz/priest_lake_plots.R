setwd('~/Dropbox/code/priest-lake')
library(lubridate)
library(ggplot2)
library(plyr)
library(scales)
rm(list=ls())
flow <- read.csv('data/flow-d3.csv', as.is=T)
height <- read.csv('data/height-d3.csv', as.is=T)
d <- join(height, flow, by="Date", type="full")


# parse dates
d$Date <- as.Date(d$Date)
d$Year <- year(d$Date)
d$Month <- month(d$Date, label=T)
d$YearMonth <- paste(d$Year, d$Month, sep="-")

ggplot(subset(d, Year>1927), aes(y=Height, x=Flow, color=Year)) + 
  geom_point(alpha=0.1) +
  scale_color_continuous(low="blue", high="red") +
  facet_grid(. ~ Month, margins=T) + 
  theme_grey() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

d$Year
d[d$Year==year,]
# aggregate
plyfx_yrmo <- function(x) {
  data.frame(avg_month=mean(x$height[!is.na(x$height)]))
}
plyfx_yr <- function(x) {
  data.frame(avg_year=mean(x$height[!is.na(x$height)]))
}
avg_month <- ddply(d, 'year_month', plyfx_yrmo)
avg_year <- ddply(d, 'year', plyfx_yr)

d <- join(avg_month, d, by="year_month", type="right")
d <- join(avg_year, d, by="year", type="right")


ggplot(d, aes(x=datetime, y=height)) + geom_line(color="steelblue", alpha=0.5) +
  geom_line(aes(y=avg_year), color="tomato", size=1.5) + 
  ylab('Gage Height') + 
  xlab('Date') + 
  scale_x_date(
    labels = date_format("%Y"),
    breaks=date_breaks(width="5 years")
  ) +
  labs(title="Priest Lake Gage Height by Day, 1928 - 2013") + 
  theme_minimal()
  
