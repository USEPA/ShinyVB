map_click = function(input,map_clicks,bo) {
  
  shore_icons = awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "cadetblue")
  
  water_icon = awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "lightblue")
  
  click = input
  lat = round(click$lat,digits=5)
  lng = round(click$lng,digits=5)
  i=nrow(map_clicks$points)+1
  map_clicks$points[i,1] = lat
  map_clicks$points[i,2] = lng
  
  if (nrow(map_clicks$points) == 1) {
    
    leafletProxy("map") %>% addAwesomeMarkers(lat = lat, lng = lng, icon = shore_icons, group = "shoreline")

  } else if (nrow(map_clicks$points) == 2) {
    
    leafletProxy("map") %>% addAwesomeMarkers(lat = lat, lng = lng, icon = shore_icons, group = "shoreline")
      
      shoreline = data.frame(
        lon = c(map_clicks$points[1,2],map_clicks$points[2,2]),       
        lat =c(map_clicks$points[1,1],map_clicks$points[2,1])
      )
      
      leafletProxy("map") %>% addPolylines(lng=shoreline$lon, lat=shoreline$lat,color="darkgreen",opacity=1.0,weight= 3,group="shoreline")
    
  } else if (nrow(map_clicks$points) == 3) {
    
    leafletProxy("map") %>% addAwesomeMarkers(lat = lat, lng = lng, icon = water_icon, group = "shoreline")
    
    #Calculate Beach Orientation
    
    #Order points from left to right
    
    if (map_clicks$points[1,1] < map_clicks$points[2,1]) {
      pointA = map_clicks$points[1,]
      pointB = map_clicks$points[2,]
    } else if (map_clicks$points[1,1] > map_clicks$points[2,1]) {
      pointA = map_clicks$points[2,]
      pointB = map_clicks$points[1,]
    } else {
      pointA = map_clicks$points[1,]
      pointB = map_clicks$points[2,]
    }
    
    #Using latitude for y-axis and longitude for x-axis because we want 0 degrees to be a north-south orientation!
    
    deltaY = pointB[1,1] - pointA[1,1]
    deltaX = pointB[1,2] - pointA[1,2]
    
    angle = atan2(deltaY,deltaX)
    beach_orient = (180*angle)/pi
    
    if (deltaY == 0) {
      quad = 5
      slope = 1 * 10^30
      
      if (map_clicks$points[3,1] > map_clicks$points[2,1]) {
        beach_orient = 270
      } else {
        beach_orient = 90
      }
    } else if (deltaX == 0) {
      slope = 0
      quad = 6
      
      if (map_clicks$points[3,2] > map_clicks$points[2,2]) {
        beach_orient = 0
      } else {
        beach_orient = 180
      }
    } else if ((deltaX & deltaY != 0)) {
      
      slope = (map_clicks$points[1,1] - map_clicks$points[2,1]) / (map_clicks$points[1,2] - map_clicks$points[2,2])
      b = pointA[1,1] - slope*pointA[1,2]
      
      intersection_x = (slope*(map_clicks$points[3,1]-b)+map_clicks$points[3,2])/(slope^2+1)
      intersection_y = (slope^2*map_clicks$points[3,1]+slope*map_clicks$points[3,2] + b)/(slope^2+1)
      
      if (intersection_x > map_clicks$points[3,2] & intersection_y < map_clicks$points[3,1]) {
        quad = 3
      } else if (intersection_x < map_clicks$points[3,2] & intersection_y < map_clicks$points[3,1]) {
        quad = 4
      } else if (intersection_x < map_clicks$points[3,2] & intersection_y > map_clicks$points[3,1]) {
        quad = 1
      } else if (intersection_x > map_clicks$points[3,2] & intersection_y > map_clicks$points[3,1]) {
        quad = 2
      }
    }
    
    if (slope > 0 & slope < 1*10^30) {
      
      if (quad == 1) {
        beach_orient = 90 - beach_orient
      } else if (quad == 3) {
        beach_orient = 270 - beach_orient
      }
      
    } else if (slope < 0) {
      
      if (quad == 2) {
        beach_orient = 270 - beach_orient 
      } else if (quad == 4) {
        beach_orient = 450 - beach_orient
      }
    }
    
    bo(round(beach_orient,digits=1))
    
    # leafletProxy("map") %>% addPopups(lat=lat,lng=lng,paste("Beach Orientation:",round(beach_orient,digits=1)))
    
  } else {
    
    leafletProxy("map") %>% clearGroup("shoreline")
    
    map_clicks$points = data.frame()
    lat = round(click$lat,digits=5)
    lng = round(click$lng,digits=5)
    map_clicks$points[1,1] = lat
    map_clicks$points[1,2] = lng
    
    leafletProxy("map") %>% addAwesomeMarkers(lat = lat, lng = lng, icon = shore_icons, group = "shoreline")
  }
}