map_click = function(input,rv,bo) {

    click = input
    lat = round(click$lat,digits=5)
    lng = round(click$lng,digits=5)
    i=nrow(rv$points)+1
    rv$points[i,1] = lat
    rv$points[i,2] = lng
    
    if (nrow(rv$points) < 3) {
      
      leafletProxy("map") %>% addMarkers(lat = lat, lng = lng)
      
      if (nrow(rv$points) == 2) {
        
        shoreline = data.frame(
          lon = c(rv$points[1,2],rv$points[2,2]),       
          lat =c(rv$points[1,1],rv$points[2,1])
        )
        
        leafletProxy("map") %>% addPolylines(lng=shoreline$lon, lat=shoreline$lat,color="darkgreen",opacity=1.0,weight= 3,layerId="shoreline")
        
      }
      
    } else if (nrow(rv$points) == 3) {
      
      #Calculate Beach Orientation
      
      #Order points from left to right
      
      if (rv$points[1,1] < rv$points[2,1]) {
        pointA = rv$points[1,]
        pointB = rv$points[2,]
      } else if (rv$points[1,1] > rv$points[2,1]) {
        pointA = rv$points[2,]
        pointB = rv$points[1,]
      } else {
        pointA = rv$points[1,]
        pointB = rv$points[2,]
      }
      
      #Using latitude for y-axis and longitude for x-axis because we want 0 degrees to be a north-south orientation!
      
      deltaY = pointB[1,1] - pointA[1,1]
      deltaX = pointB[1,2] - pointA[1,2]
      
      angle = atan2(deltaY,deltaX)
      beach_orient = (180*angle)/pi
      
      if (deltaY == 0) {
        quad = 5
        slope = 1 * 10^30
        
        if (rv$points[3,1] > rv$points[2,1]) {
          beach_orient = 270
        } else {
          beach_orient = 90
        }
      } else if (deltaX == 0) {
        slope = 0
        quad = 6
        
        if (rv$points[3,2] > rv$points[2,2]) {
          beach_orient = 0
        } else {
          beach_orient = 180
        }
      } else if ((deltaX & deltaY != 0)) {
        
        slope = (rv$points[1,1] - rv$points[2,1]) / (rv$points[1,2] - rv$points[2,2])
        b = pointA[1,1] - slope*pointA[1,2]
        
        intersection_x = (slope*(rv$points[3,1]-b)+rv$points[3,2])/(slope^2+1)
        intersection_y = (slope^2*rv$points[3,1]+slope*rv$points[3,2] + b)/(slope^2+1)
        
        if (intersection_x > rv$points[3,2] & intersection_y < rv$points[3,1]) {
          quad = 3
        } else if (intersection_x < rv$points[3,2] & intersection_y < rv$points[3,1]) {
          quad = 4
        } else if (intersection_x < rv$points[3,2] & intersection_y > rv$points[3,1]) {
          quad = 1
        } else if (intersection_x > rv$points[3,2] & intersection_y > rv$points[3,1]) {
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
      
      leafletProxy("map") %>% addPopups(lat=lat,lng=lng,paste("Beach Orientation:",round(beach_orient,digits=1)))
      
    } else {
      
      leafletProxy("map") %>% clearMarkers()
      leafletProxy("map") %>% clearShapes()
      rv$points = data.frame()
    }
}