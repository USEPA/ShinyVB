map_click <- function(input, map_clicks, bo) {
  
  if (!all(c("lat","lng") %in% names(map_clicks$points))) {
    map_clicks$points <- data.frame(lat = numeric(0), lng = numeric(0))
  }
  
  shore_icon <- awesomeIcons(
    icon = "ios-close",
    iconColor = "black",
    library = "ion",
    markerColor = "cadetblue"
  )
  
  water_icon <- awesomeIcons(
    icon = "ios-close",
    iconColor = "black",
    library = "ion",
    markerColor = "lightblue"
  )
  
  click <- input
  lat <- round(click$lat, 5)
  lng <- round(click$lng, 5)
  
  # Append this click
  map_clicks$points <- rbind(map_clicks$points, data.frame(lat = lat, lng = lng))
  n <- nrow(map_clicks$points)
  
  if (n == 1) {
    
    leafletProxy("map") %>%
      addAwesomeMarkers(
        lat = map_clicks$points$lat[1],
        lng = map_clicks$points$lng[1],
        icon = shore_icon,
        group = "shoreline"
      )
    
  } else if (n == 2) {
    
    leafletProxy("map") %>%
      addAwesomeMarkers(
        lat = map_clicks$points$lat[2],
        lng = map_clicks$points$lng[2],
        icon = shore_icon,
        group = "shoreline"
      )
    
    shoreline <- data.frame(
      lon = c(map_clicks$points$lng[1], map_clicks$points$lng[2]),
      lat = c(map_clicks$points$lat[1], map_clicks$points$lat[2])
    )
    
    leafletProxy("map") %>%
      addPolylines(
        lng = shoreline$lon, lat = shoreline$lat,
        color = "darkgreen", opacity = 1.0, weight = 3, group = "shoreline"
      )
    
  } else if (n == 3) {
    
    leafletProxy("map") %>%
      addAwesomeMarkers(
        lat = map_clicks$points$lat[3],
        lng = map_clicks$points$lng[3],
        icon = water_icon,
        group = "shoreline"
      )
    
    # Rectangle: A--B is shoreline, A2--B2 is parallel through W
    A <- c(lng = map_clicks$points$lng[1], lat = map_clicks$points$lat[1])
    B <- c(lng = map_clicks$points$lng[2], lat = map_clicks$points$lat[2])
    W <- c(lng = map_clicks$points$lng[3], lat = map_clicks$points$lat[3])
    
    v   <- c(B["lng"] - A["lng"], B["lat"] - A["lat"])
    len <- sqrt(sum(v^2))
    if (len > 0) {
      u <- v / len
      nrm <- c(-u[2], u[1])
      
      d <- (W["lng"] - A["lng"]) * nrm[1] + (W["lat"] - A["lat"]) * nrm[2]
      A2 <- c(A["lng"] + d * nrm[1], A["lat"] + d * nrm[2])
      B2 <- c(B["lng"] + d * nrm[1], B["lat"] + d * nrm[2])
      
      leafletProxy("map") %>% addPolygons(
        lng = c(A["lng"], B["lng"], B2[1], A2[1], A["lng"]),
        lat = c(A["lat"], B["lat"], B2[2], A2[2], A["lat"]),
        fillColor  = "#4da3ff",
        fillOpacity = 0.3,
        color      = "#4da3ff",
        weight     = 2,
        group      = "shoreline",
        layerId    = "water_rect"
      )
    }
    
    # Beach orientation
    if (map_clicks$points$lat[1] < map_clicks$points$lat[2]) {
      pointA <- map_clicks$points[1, ]
      pointB <- map_clicks$points[2, ]
    } else if (map_clicks$points$lat[1] > map_clicks$points$lat[2]) {
      pointA <- map_clicks$points[2, ]
      pointB <- map_clicks$points[1, ]
    } else {
      pointA <- map_clicks$points[1, ]
      pointB <- map_clicks$points[2, ]
    }
    
    deltaY <- pointB$lat - pointA$lat
    deltaX <- pointB$lng - pointA$lng
    
    angle <- atan2(deltaY, deltaX)
    beach_orient <- (180 * angle) / pi
    
    if (deltaY == 0) {
      quad <- 5
      slope <- 1e30
      
      if (map_clicks$points$lat[3] > map_clicks$points$lat[2]) {
        beach_orient <- 270
      } else {
        beach_orient <- 90
      }
      
    } else if (deltaX == 0) {
      slope <- 0
      quad <- 6
      
      if (map_clicks$points$lng[3] > map_clicks$points$lng[2]) {
        beach_orient <- 0
      } else {
        beach_orient <- 180
      }
      
    } else if (deltaX != 0 && deltaY != 0) {
      
      slope <- (pointA$lat - pointB$lat) / (pointA$lng - pointB$lng)
      b <- pointA$lat - slope * pointA$lng
      
      ix <- (slope * (map_clicks$points$lat[3] - b) + map_clicks$points$lng[3]) / (slope^2 + 1)
      iy <- (slope^2 * map_clicks$points$lat[3] + slope * map_clicks$points$lng[3] + b) / (slope^2 + 1)
      
      if (ix > map_clicks$points$lng[3] && iy < map_clicks$points$lat[3]) {
        quad <- 3
      } else if (ix < map_clicks$points$lng[3] && iy < map_clicks$points$lat[3]) {
        quad <- 4
      } else if (ix < map_clicks$points$lng[3] && iy > map_clicks$points$lat[3]) {
        quad <- 1
      } else if (ix > map_clicks$points$lng[3] && iy > map_clicks$points$lat[3]) {
        quad <- 2
      }
    }
    
    if (slope > 0 && slope < 1e30) {
      if (quad == 1) {
        beach_orient <- 90 - beach_orient
      } else if (quad == 3) {
        beach_orient <- 270 - beach_orient
      }
    } else if (slope < 0) {
      if (quad == 2) {
        beach_orient <- 270 - beach_orient
      } else if (quad == 4) {
        beach_orient <- 450 - beach_orient
      }
    }
    
    bo(round(beach_orient, 0))
    updateNumericInput(inputId = "beach_angle", value = round(bo(), 0))
    
  } else {
    # 4th click: reset and start a new sequence with this click as the first
    leafletProxy("map") %>% clearGroup("shoreline")
    
    map_clicks$points <- data.frame(lat = lat, lng = lng)
    
    leafletProxy("map") %>%
      addAwesomeMarkers(
        lat = lat, lng = lng,
        icon = shore_icon, group = "shoreline"
      )
  }
}