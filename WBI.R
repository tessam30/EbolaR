# ---- Download WDI package and install

# --- Install World Development Indicators API if not already installed
# install.packages("WDI")

# --- Clear the workspace
remove(list = ls())

# --- Load libraries & set working directory
libs <- c ("ggplot2", "dplyr", "RColorBrewer", "grid", "WDI", 
           "directlabels", "gridExtra")

# --- Load required libraries
lapply(libs, require, character.only=T)

# --- Search WDI database for urban population variables
WDIsearch('urban population')

# --- Extract urban population variable into new data frame
# --- Select extra = TRUE to extract extra variables and use tbl_df for dplyr
d <- tbl_df(WDI(country = "all", indicator = "SP.URB.TOTL.IN.ZS",
         start = 1960, end = 2010, extra = TRUE, cache = NULL))

# Remove "(developing only)" from country name
d$country <- gsub("(developing only)", "", d$country, fixed = TRUE) 

# --- Browse the names to get a sense of what is in the data
head(d)
tail(d)
unique(d$iso3c)

# --- Looking for EAP, ECA, LAC, MENA, SA, SSA using dplyr and ISO3 codes
#     see http://data.worldbank.org/developers/api-overview?print&book_recurse
fd <- filter(d, iso3c == "EAP" | iso3c == "ECA" | iso3c == "LAC" |
          iso3c == "MNA" | iso3c == "SSA" | iso3c == "SAS") %>%
          arrange(country, year) 


# --- Create a unique id for each region
fd$id <- c(as.factor(fd$country))

# --- Lab RGB colors in case of customization
redL   	<- c("#B71234")
dredL 	<- c("#822443")
dgrayL 	<- c("#565A5C")
lblueL 	<- c("#7090B7")
dblueL 	<- c("#003359")
lgrayL	<- c("#CECFCB")


# --- First basic plot of data using paths and points
pf <- ggplot(fd, aes(y = SP.URB.TOTL.IN.ZS, x = year, colour = country)) + #define basic plot
        geom_path(size = 0.25) + geom_point(size = 2.5) + #customize plot type
        geom_hline(yintercept = 50, linetype="dotted", size = 1, alpha = .10) #add in horizontal lize

# --- Set legend status
legstat <- c("none")

# --- Customize plot 
pp <- pf + theme(legend.position = legstat, legend.title=element_blank(), 
          panel.border = element_blank(), legend.key = element_blank(), 
          legend.text = element_text(size = 14), #Customize legend
          plot.title = element_text(hjust = 0, size = 17, face = "bold"), # Adjust plot title
          panel.background = element_rect(fill = "white"), # Make background white 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #remove grid    
          axis.text.y = element_text(hjust = -0.5, size = 14, colour = dgrayL), #soften axis text
          axis.text.x = element_text(hjust = .5, size = 14, colour = dgrayL),
          axis.ticks.y = element_blank(), # remove y-axis ticks
          #axis.ticks.x=element_blank(), # remove x-axis ticks
          #plot.margin = unit(c(1,1,1,1), "cm"),
          plot.title = element_text(lineheight = 1 ), # 
          panel.grid.major = element_blank(), # remove facet formatting
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 13, colour = dgrayL, face = "bold"), # format facet panel text
          panel.border = element_rect(colour = "black"),
          panel.margin = unit(2, "lines")) + # Move plot title up
          scale_x_continuous(breaks = seq(1960, 2010, 10), expand = c(0.05,0.05)) + #customize x-axis
          scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) + # customize y-axis
          labs(x = "", y = "Urban population (% of total)\n", # label y-axis and create title
          title = "East Asia has experienced a rapid increase in the percent of the population living in urban areas in the last 30 years.", size = 13) +
          facet_wrap(~ country, nrow = 1) + scale_colour_brewer(palette="Set2") # apply faceting and color palette
print(pp)

# Add a footnote to the graph using the gridExtra package
g <- arrangeGrob(pp, sub = textGrob("Source: World Bank World Development Indicators"
                , x = 0, hjust = -0.25, vjust=-0.25, 
                gp = gpar(fontface = "italic", fontsize = 12, col = dgrayL)))


# --- Create variable reflecting working directory
wd <- c("C:/Users/t/Documents/GitHub/EbolaR")
setwd(wd)

# Save the plot
ggsave(g, filename = paste("Urbanization.png"), width=21, height=10, dpi = 300)



############
# Dygraphs #
############

# --- Load new libraries for interactive graphics
library("reshape2")
library("zoo")
library("htmlwidgets")
library("dygraphs")

# --- Select and reshape data and try interactive plot
names(fd)
fds <- select(fd, country, SP.URB.TOTL.IN.ZS, year, id)

# --- Select and reshape the data to make interactive plots using the dygraphs package
fds_reshape <- dcast(fds, year ~ country, value.var = "SP.URB.TOTL.IN.ZS" )

# --- Add time variable to dataset
fds_wide <- as.data.frame(fds_reshape %>%
  mutate(year = as.Date(sprintf("%d-01-01", year))))

# --- Declare time variable
date <- as.Date(fds_wide$year)

# --- Create subset of data containing only country values
urb <- subset(fds_wide, select = -c(year) )

# Rename variables for Legend
colnames(urb) <- c("E. Asia", "Europe & C. Asia", "LAC", "MENA", "S. Asia", "Sub-Saharan Africa")

# Set data as time-series and make basic plots
urb.ts <- zoo(urb, order.by=date)
plot(urb.ts)
dygraph(urb.ts)

# Customize the dygraph 
dg <- dygraph(urb.ts, main = "East Asia has experienced rapid urbanization in the past 30 years") %>%
  dyAxis("y", label = "Urban population (% of total)", drawGrid = FALSE) %>%
  dyAxis("x", pixelsPerLabel = 50, drawGrid = FALSE) %>%
  dyOptions(axisLineWidth = 0.01, colors = RColorBrewer::brewer.pal(6, "Set2"))  %>%
  dyLegend(width = 800) %>%
  dyRangeSelector() %>%
  dyHighlight(highlightCircleSize = 3.5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE)

setwd(wd)
saveWidget(dg, "Urbanization.html", selfcontained = TRUE)

# --- Investigate use of steamgraphs (Not really appropriate for this type of data)
library("streamgraph")
fd %>%
  streamgraph("country", "SP.URB.TOTL.IN.ZS", "year", offset = "zero", interpolate="linear")
    sg_legend(show = TRUE, label = "Country:")


