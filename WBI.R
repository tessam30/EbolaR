# ---- Download WDI package and install

# --- Install World Development Indicators API if not already installed
install.packages("WDI")

# --- Clear the workspace
remove(list = ls())

# --- Load libraries & set working directory
libs <- c ("ggplot2", "dplyr", "RColorBrewer", "grid", "WDI", 
           "zoo", "lubridate", "directlabels")

# --- Load required libraries
lapply(libs, require, character.only=T)

# --- Search WDI database for urban population variables
WDIsearch('urban population')

# --- Extract urban population variable into new data frame
# Select extra = TRUE to extract extra variables and use tbl_df for dplyr
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


# Define Global Development Lab Colors for potential use
# Lab RGB colors
redL   	<- c("#B71234")
dredL 	<- c("#822443")
dgrayL 	<- c("#565A5C")
lblueL 	<- c("#7090B7")
dblueL 	<- c("#003359")
lgrayL	<- c("#CECFCB")

# --- Make first basic spaghetti plot of data
p <- ggplot(fd, aes(y = SP.URB.TOTL.IN.ZS, x = year, colour = country)) + 
        geom_path(alpha = 0.1) + geom_point(size = 3.5)

# geom_point(aes(size = (SP.URB.TOTL.IN.ZS/10)))

# --- Set legend status
legstat <- c("top")

# --- Customize plot 
pp <- p + theme(legend.position = legstat, legend.title=element_blank(), 
          panel.border = element_blank(), legend.key = element_blank(), 
          legend.text = element_text(size = 14), #Customize legend
          plot.title = element_text(hjust = 0, size = 16, face = "bold"), # Adjust plot title
          panel.background = element_rect(fill = "white"), # Make background white 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #remove grid    
          axis.ticks.y = element_blank(), #Remove axis
          axis.text.y = element_text(hjust = -0.5, size = 14, colour = dgrayL), #soften axis text
          axis.text.x = element_text(hjust = .5, size = 14, colour = dgrayL),
          #axis.ticks.x=element_blank(), # remove x-axis ticks
          #plot.margin = unit(c(1,1,1,1), "cm"),
          plot.title = element_text(lineheight = 0 )) + # Move plot title up
          scale_x_continuous(breaks = seq(1960, 2010, 10), expand = c(0.02,0.02)) + #customize x-axis
          scale_y_continuous(breaks = seq(10, 80, 10)) + # customize y-axis
          labs(x = "", y = "Urban population (% of total)\n", 
          title = " East Asia has experienced a rapid increase in the percent of the population living in urban areas in the last 30 years.", size = 13) +
          scale_colour_brewer(palette="Set2") + facet_wrap(~ country)
print(pp)

# --- Make first basic spaghetti plot of data
pf <- ggplot(fd, aes(y = SP.URB.TOTL.IN.ZS, x = year, colour = country)) + 
  geom_path(alpha = 0.1) + geom_point(size = 3.5) + facet_wrap( ~country)









# --- Interactive plots
library(metricsgraphics)

fdts <- fd 

# mutate(year = as.Date(sprintf("%d-01-01", year)))
# as.Date(fdts$year)

# Rename key variable of interest
names(fdts)[names(fdts) == 'SP.URB.TOTL.IN.ZS'] <- "Urban.Population"

mjs_plot(fdts, x = year, y = Urban.Population) %>%
    mjs_point(color_accessor = id, color_type = "number",
              size_accessor = Urban.Population) %>%
    mjs_labs(x = "year", y = "Urban population (% of total)") %>%
    mjs_axis_x(xax_format = "plain") %>%
    mjs_add_legend(legend=c("Country")) %>%
    mjs_line()
    

# --- Check out streamgraphs of the data
library(streamgraph)

fd %>%
  filter(grepl("^(EAP|ECA|LAC|MNA|SSA|SSA)$", iso3c)) %>%
  group_by(year, iso3c) %>%
  streamgraph("country", value = "Urban.Population", "year", 
              offset = "wiggle", interpolate="linear") %>%
  sg_axis_x(20) %>%
  sg_fill_brewer("Set2") %>%
  sg_legend(show=TRUE, label="Region: ")














