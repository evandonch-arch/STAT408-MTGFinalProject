# this script is drawn partly from https://r-spatial.org/r/2018/10/25/ggplot2-sf.html


# - what ggspatial can do, and using it to add annotations like scale bar and north arrow
# - using geom_text and annotate to add country names
# - more theme options: grid lines, background color

# Good practice: load your packages at the beginning of the script, 
# and/or use a comment at the top of the script to tell users what packages  
# they need to have installed on their computer

# packages you'll need to install if you haven't already:
# rnaturalearth, rnaturalearthdata, rnaturalearthhires, mapview, ggplot2, sf, ggspatial
# note that some of these you don't need to load (using library), just make sure they're installed

# you can use install.packages() for all the packages except rnaturalearthhires, 
# which uses another package called pak to install it (this is common)
# install.packages("pak")
# pak::pkg_install("ropensci/rnaturalearthhires")




# first, let's explore the data -----------------------------------------------

# rnaturalearth is an R package to access and work with a public domain map dataset called Natural Earth
# more about rnaturalearth: https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html

library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

# what package is the ne_countries function from?
plot(ne_countries(type = "countries", scale = "small"))
plot(ne_countries(country = "united kingdom", type = "countries"))
plot(ne_countries(country = "united kingdom", type = "map_units")) # requires having the rnaturalearthdata package
plot(ne_countries(country = "united kingdom", scale = "large")) # requires having the rnaturalearthhires package

# nicer plots with mapview
# recall: what's the difference between mapview() and mapview::mapview()?
mapview::mapview(ne_countries(country = "united kingdom", scale = "large")$geometry)
mapview::mapview(ne_countries(country = "ireland", scale = "small")$geometry)
mapview::mapview(ne_countries(country = "ireland", scale = "medium")$geometry)
mapview::mapview(ne_countries(country = "ireland", scale = "large")$geometry)

# you can also look at the coastlines
plot(ne_coastline())



# now, let's start making some maps -------------------------------------------

library(ggplot2)
library(sf) # see slides for references about sf
library(ggspatial) # more about ggspatial: https://paleolimbot.github.io/ggspatial/articles/ggspatial.html

theme_set(theme_bw()) # sets the theme for plots in this script

# ne_countries() can return sp or sf class objects, and we want sf
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
st_geometry(world) # what kind of feature is world? why?

# let's look at the geometry column, the sfc
world_geom = st_geometry(world)
world_geom = world$geometry # same as above

world_geom
world_geom[[1]] # shows you the first simple feature geometry
class(world_geom[[1]])
str(world_geom[[1]])

# take a look at the world object in your environment: how many rows and columns? what kinds of variables are in it?
# note the value of the << < > >> arrows when the dataset has >50 columns

# a first plot
ggplot(data = world) +
  geom_sf() # this function is in which package?

ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# coord_sf lets you specify the coordinate reference system (crs)
ggplot(data = world) +
  geom_sf() +
  # what package is coord_sf from?
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035)) # what package is st_crs from?

# coord_sf also lets you set the spatial extent of the map
# try copying this code block fresh from the tutorial and adjusting the style
ggplot(data = world) +
  geom_sf() +
  coord_sf(
    xlim = c(-102.15, -74.12), 
    ylim = c(7.65, 33.97), 
    expand = FALSE
  )

# what are three ways you can figure out what expand = FALSE does?
# - 
# - 
# - 

# now we start to use ggspatial for the annotation_scale and annotation_north_arrow functions
# - adds a scale and a north arrow to the plot
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) + # what do the arguments do?
  annotation_north_arrow( # what do the arguments do? how would you know which ones to use and why/how?
    location = "bl", which_north = "true",
    pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))

# st_centroid computes the centroid of each multipolygon
world_points = st_centroid(world)
st_geometry(world)
st_geometry(world_points)
class(world)
class(world_points)

world_points = st_centroid(world$geometry)
st_geometry(world$geometry) # how does this compare to st_geometry(world)?
st_geometry(world_points)
class(world$geometry) # how does this compare to st_geometry(world)?
class(world_points)

# now let's extract just the X Y coordinates of the centroids
world_points = world$geometry |>
  st_centroid() |>
  st_coordinates()

# st_centroid and st_coordinates are in what package(s)?

# cbind binds columns by stitching together datasets with the same number of rows 
#   (rbind binds rows by stacking datasets with the same columns)
# here, we combine world and world_points; where do the columns of the old 
#   world_points end up in the new world_points?
world_points <- cbind(world, world_points)
st_geometry(world_points)
class(world_points)

# use geom_text and annotate to add country names
# - what packages are these functions from?
# - note: the help documentation for annotate has some great examples!
ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points, aes(x=X, y=Y, label=name),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico",
           fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

# you can use ggsave to save whatever last map/figure you made to a file (png, pdf, etc.)
ggsave("map.pdf") # save as a pdf
ggsave("map_web.png", width = 6, height = 6, dpi = "screen") # save as a png

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))
