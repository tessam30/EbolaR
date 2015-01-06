# Create a simple html time-series widget using Ebola data

remove(list = ls()) # Clear existing workspace
setwd("C:/Users/tessam/Documents/GitHub/EbolaR")

d <- read.csv("Ebola.csv", header = TRUE)

# load libraries for htmlwidgets
library("htmlwidgets")
library("dygraphs")
library("zoo")
library("lubridate")

# View head of datatable
head(d)

ed <- subset(d, select = c(lib_deaths, gui_deaths, sl_deaths))
names(ed) <- c("Liberia", "Guinea", "Sierra Leone")
date <- mdy(d$date)


# Set time series in data
eb.ts <- zoo(ed, order.by=date)
plot(eb.ts)
dygraph(eb.ts)


dygraph(eb.ts, main = "West Africa Ebola Deaths") %>%
  dyAxis("y", label = "Deaths", drawGrid = FALSE) %>%
  dyOptions(axisLineWidth = 1.5)  %>%
  dyLegend(width = 400) %>%
  dyRangeSelector()

  