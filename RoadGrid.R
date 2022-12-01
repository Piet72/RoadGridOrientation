#----run this line if you haven't installed any of these packages yet
install.packages(c('sf', 'geosphere', 'osmdata', 'tidyverse', 'cowplot'))

#----load up the necessary packages
library(sf)
library(geosphere)
library(osmdata)
library(tidyverse)
library(cowplot)


#----Fill out this section with your info before running the rest of the code----

#Pick a city to plot
city <- 'Portland, OR'

#list out the road designations you want to plot
plottype <- c('Avenue', 'Drive', 'Boulevard', 'Road',  'Street', 'Terrace', 'Place',  'Court', 'Way')

#Pick colors to plot each designation with 
plotcolors <- c('#59C8E5', '#0A7ABF', '#2E968C', '#4CB580',  '#FED032','#FF9223', '#FE9EA5',  '#FE4D64', '#8374BE')


#---get the road network from OSM----

# get bounding box for a given place name
bb <- getbb(city)

# build an overpass query
qry <- opq(bbox = bb, timeout = 300) %>%
  # add the ley feature 'highway' to the overpass query
  add_osm_feature(key = 'highway') %>%
  # get overpass query as an osmdata object in sf format
  # sf (simple features format: https://en.wikipedia.org/wiki/Simple_Features
  # how sf in R are organized: https://r-spatial.github.io/sf/articles/sf1.html#how-simple-features-in-r-are-organized
  # sf vignette: https://cran.r-project.org/web/packages/sf/sf.pdf
  osmdata_sf()

# getting a Simple Features collection of linestrings
roads <- qry$osm_lines %>% select(geometry, name)


#----find road bearings----

# initialize the dataframe that contains geo data
bearings <- data.frame(b = numeric(), l = numeric(), name = character())

for (i in 1:(length(roads$geometry))){
  #geometry contains linestring like:
  # LINESTRING (-122.7928 45.52251, -122.7927 45.52237, -122.7927 45.52214, -122.7926 45.52201, -122.7925 45.52189, -122.7922 45.52141, -122.7919 45.52085, -122.7918 45.52074, -122.7916 45.52046, -122.7916 45.52038, -122.7915 45.52024, -122.7914 45.52006, -122.7914 45.51997, -122.7914 45.51985, -122.7914 45.51974)
  # length of linestring above is 30, somehow it counts every single value although the linestring is composed of <lon,lat> tuples
  k <- length(roads$geometry[[i]])/2
  # get first and last pair of <lon,lat> coordinates
  lon1 <- roads$geometry[[i]][1,1]
  lat1 <- roads$geometry[[i]][1,2]
  lon2 <- roads$geometry[[i]][k,1]
  lat2 <- roads$geometry[[i]][k,2]
  
  bearings <- bind_rows(
    bearings, 
    data.frame(
      # get direction in degrees for each linestring
      b = geosphere::bearing(c(lon1, lat1),c(lon2, lat2)),
      # shortest distance between first and last points of linestring
      l = geosphere::distHaversine(c(lon1, lat1),c(lon2, lat2)),
      # keep the name of the linestring, if any
      name = roads$name[i]
    )
  )
}


#----get ready to plot----

#get the inverse of each bearing (e.g. if a road goes 90deg, it also goes at 270deg)
plot <- bind_rows(bearings,
                  data.frame(b = bearings$b[bearings$b<=180]+180, l = bearings$l[bearings$b<=180], name = bearings$name[bearings$b<=180])) %>%
  bind_rows(
    data.frame(b = bearings$b[bearings$b>180]-180, l = bearings$l[bearings$b>180], name = bearings$name[bearings$b>180]))

#shift any bearings <0 back to positive numbers
plot$b[plot$b< 0] <- plot$b[plot$b< 0]+360

#round each value to the closest 10 degrees to eliminate edge effects.
#then shift all values by -5 so that the plotted bars will be centered on their respective tick marks
plot$b <- (round(plot$b/10,0)*10) %% 360 - 5

#----plot----
plotgrid <- NULL

for (i in 1:length(plottype)) {
  #subset all roads to just the current road designation
  plotme <- subset(plot, grepl(paste0(" ", plottype[i], "$"), name))
  color <- plotcolors[i]
  
  #first get the basic plot ready
  basicplot <- ggplot(plotme, aes(x = b, weight = l)) +
    geom_histogram(binwidth = 10,  boundary = 5, size = .01, closed = 'left',
                   fill = color, color = 'black', aes(y=..count../sum(..count..))) +
    scale_x_continuous(breaks=c(0, 90, 180, 270), limits = c(-5, 355), labels = c("N", "E", "S", "W")) +
    coord_polar(start  = -pi/36) +
    xlab(NULL)+ylab(NULL) +  theme_bw() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  #then extract the data to make the labels
  plotdata <- ggplot_build(basicplot)$data[[1]]
  
  #and put it into its slot
  plotgrid[[i]] <- basicplot +
    geom_hline(yintercept = seq(.25, .75, by = .25)*max(plotdata$y), alpha = .2, size = .1) +
    geom_hline(yintercept = max(plotdata$y)) + 
    geom_vline(xintercept = seq(0, 360, by = 45), alpha = .2, size = .1) +
    geom_text(size =1, color = 'grey', x = 22.5, y = as.double(max(plotdata$y)*.25+max(plotdata$y)*.07), label = round(max(plotdata$y)*.25,2)) +
    geom_text(size =1, color = 'grey', x = 22.5, y = as.double(max(plotdata$y)*.5+max(plotdata$y)*.07), label = round(max(plotdata$y)*.5,2)) +
    geom_text(size =1, color = 'grey', x = 22.5, y = as.double(max(plotdata$y)*.75+max(plotdata$y)*.07), label = round(max(plotdata$y)*.75,2)) +
    geom_text(size =1, color = 'grey', x = 22.5, y = as.double(max(plotdata$y)+max(plotdata$y)*.07), label = round(max(plotdata$y),2)) 
  
}

#finally, plot em all together
#of course, if you have more or less than 9 road designations to plot, you'll need to adjust this section to match
plot_grid(plotgrid[[1]], plotgrid[[2]], plotgrid[[3]], 
          plotgrid[[4]], plotgrid[[5]], plotgrid[[6]], 
          plotgrid[[7]], plotgrid[[8]], plotgrid[[9]],   ncol = 9)

#and save
ggsave(paste0(city, ".png"),  width = 24, height = 4, units = "in", dpi = 300)



