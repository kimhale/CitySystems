# Convert between geojson and sp spatial objects in R

combatcommands.df <- combatcommands.df %>%
            mutate(long = if_else((long <= -180), -1*(180+long), long)) %>%
            mutate(long = if_else((ABBREV == "USEUCOM" & long > 180), 180, long)) %>%
            mutate(long = if_else((group == "4.1"), long-180, long)) %>%
            filter(long <= 180 & long >=-180)
  

basemap <- ggplot() +
  geom_polygon(dat = wrld.df, aes(x = long, y = lat, group = group, fill = CONTINENT), color = "grey10") +
  #geom_point(dat = filter(worldcities, pop >= 200000), aes(x = lng, y = lat)) +
  theme_void() +
  coord_equal()

basemap


combatcommands <- readOGR("GeographicCombatantCommands.json", "OGRGeoJSON")
combatcommands@data$id <-  rownames(combatcommands@data)
combatcommands.points <-  fortify(combatcommands, region="id")
combatcommands.df <-  plyr::join(combatcommands.points, combatcommands@data, by="id")
worldcities <- read.csv("simplemaps-worldcities-basic.csv", stringsAsFactors = FALSE)

remove(combatcommands.points, combatcommands)


coordinates(worldcities) <- ~lng+lat
proj4string(wrld) 
proj4string(worldcities) <- proj4string(wrld) 

worldcities@data <- cbind(worldcities@data, over(worldcities, wrld[,"CONTINENT"]))
worldcities@data$id <-  rownames(worldcities@data)
worldcities.points <-  broom(worldcities, region="id")
worldcities.df <-  plyr::join(worldcities.points, worldcities@data, by="id")


