# ---- Download WDI package and install

# --- Install World Development Indicators API if not already installed
install.packages("WDI")

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

# --- Lab RGB colors
redL   	<- c("#B71234")
dredL 	<- c("#822443")
dgrayL 	<- c("#565A5C")
lblueL 	<- c("#7090B7")
dblueL 	<- c("#003359")
lgrayL	<- c("#CECFCB")


# --- Make first basic spaghetti plot of data
pf <- ggplot(fd, aes(y = SP.URB.TOTL.IN.ZS, x = year, colour = country)) + #define basic plot
        geom_path(size = 0.25) + geom_point(size = 2.5) + #customize plot type
        geom_hline(yintercept = 50, linetype="dotted", size = 1, alpha = .10) #add in horizontal lize

# --- Set legend status
legstat <- c("none")


plot.title = "East Asia has experienced a rapid increase in the percent of the population living in urban areas in the last 30 years"
plot.subtitle
ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 

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

# Add a footnote to the graph using the gridExtra library
g <- arrangeGrob(pp, sub = textGrob("Source: World Bank World Development Indicators"
                , x = 0, hjust = -0.25, vjust=-0.25, 
                gp = gpar(fontface = "italic", fontsize = 12, col = dgrayL)))


setwd("C:/Users/t/Documents/GitHub/EbolaR")
# Save the plot
ggsave(g, filename = paste("Urbanization", ".svg"), width=21, height=8)
