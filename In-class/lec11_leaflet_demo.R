# working with the leaflet R package
# based on code from these examples: https://r-graph-gallery.com/map.html

# Note: if you do not already installed the leaflet package, first install it with:
# install.packages("leaflet")

# Get started ------------------

library(leaflet)
library(rnaturalearth)
library(sf)

# Initialize the leaflet map with the leaflet() function
m <- leaflet()
# Then we add default OpenStreetMap map tiles
m <- addTiles(m)
# Take a look at the map
m

# Same as above, but using the %>% operator
m <- leaflet() %>% 
  addTiles()
m

# Same as above, but without assignment
leaflet() %>% 
  addTiles()

# Same as above, but zoom to a specific region
# Longitude (lng) ranges from -180 (West) to 180 (East)
# Latitude (lat) ranges from -90 (South) and 90 (North)
# Zoom is controlled using the zoom argument
leaflet() %>% 
  addTiles() %>% 
  setView( lng = 166.45, lat = -22.25, zoom = 8 )

# Plot some data ------------------

# let's return to that countries dataset we looked at earlier in the course
world <- ne_countries(scale = "medium", returnclass = "sf")
st_geometry(world) # take a look

# what does this code do? how can you step through it line by line?
world_points = world |>
  st_geometry() |>
  st_centroid() |>
  st_coordinates()

world_points = cbind(world, world_points)

# check that the coordinate reference system (crs) for world_points is WGS 84, which is what leaflet expects
# https://rstudio.github.io/leaflet/articles/projections.html
st_crs(world_points) # it is!

## Three different ways to add circles to the map: --------------

### 1. Show a circle at each position --------------
leaflet(data = world_points) %>%
  addTiles() %>%
  addCircleMarkers(~X, ~Y , popup = ~as.character(name))



### 2. Show a custom circle at each position, fixed size with zoom --------------
# Size defined in Pixel. Size does not change when you zoom
# Notice that here I don't assign the map to an object, I just display it; that 
# choice is up to you and depends on your purposes
leaflet(data = world_points) %>%
  addTiles() %>%
  addCircleMarkers(~X, ~Y, radius =~ log(pop_est + 1e-16)) 

# we can add color based on the estimated population size
leaflet(data = world_points) %>%
  addTiles() %>%
  addCircleMarkers(
    ~X, ~Y, 
    radius =~ log(pop_est + 1e-16), 
    color =~ ifelse(
      world_points$pop_est > mean(world_points$pop_est) , 
      "red", 
      "orange"
    )
  ) 



### 3. Show a custom circle at each position, size rescales with zoom ------------
# Size in meters --> change when you zoom.
leaflet(data = world_points) %>%
  addTiles() %>%
  addCircles(~X, ~Y, radius=~pop_est/10^3) 

# we can add color based on estimated population size and also a pop-up bubble 
# that displays the country name when you click on a circle
leaflet(data = world_points) %>%
  addTiles() %>%
  addCircles(
    ~X, ~Y, 
    radius=~pop_est/10^3, 
    color=~ifelse(
      world_points$pop_est > mean(world_points$pop_est) , 
      "red", 
      "orange"
    ),
    popup = ~as.character(name)
  ) 


## More features ------------------

### Add a rectangle to the map ---------------
leaflet(data = world_points) %>%
  addTiles() %>%
  addCircles(
    ~X, ~Y, 
    radius=~pop_est/10^3, 
    color=~ifelse(
      world_points$pop_est > mean(world_points$pop_est) , 
      "red", 
      "orange"
    ),
    popup = ~as.character(name)
  )  %>%  
  addRectangles(
    lng1=-110, lat1=50,
    lng2=-50, lat2=10,
    fillColor = "transparent"
  )

### Change the background ---------------

# Background 1: NASA
leaflet(data = world_points) %>% 
  addTiles() %>% 
  setView(lng = 0, lat = 0, zoom = 1) %>%
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
  addCircles(
    ~X, ~Y, 
    radius=~pop_est/10^3, 
    color=~ifelse(
      world_points$pop_est > mean(world_points$pop_est) , 
      "red", 
      "orange"
    ),
    popup = ~as.character(name)
  )

# Background 2: World Imagery
leaflet(data = world_points) %>% 
  addTiles() %>% 
  setView(lng = 0, lat = 0, zoom = 1) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircles(
    ~X, ~Y, 
    radius=~pop_est/10^3, 
    color=~ifelse(
      world_points$pop_est > mean(world_points$pop_est) , 
      "red", 
      "orange"
    ),
    popup = ~as.character(name)
  )
