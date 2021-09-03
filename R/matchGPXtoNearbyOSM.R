
#' Point to Line Conversion in List
#'
#' Convert a list of observations of Points to a list of observations with lines between points.
#' The conversion adds the parameter speed.
#'
#' @param p points
#' @return The points of \code{p} merged to linestrings starting with \code{p[i-1]} and ending with \code{p[i]}.
#' @examples
#' points2line_trajectory(c(st_point(c(0,1)), st_point(c(1,2)), st_point(c(4,1)), st_point(c(4,2))))
points2line_trajectory = function(p) {
  c = sf::st_coordinates(p)
  i = seq(nrow(p) - 2)
  l = purrr::map(i, ~ sf::st_linestring(c[.x:(.x + 1), ]))
  speed = purrr::map_dbl(i, function(x) {
    geosphere::distHaversine(c[x, ], c[(x + 1), ]) /

      as.numeric(p$time[x + 1] - p$time[x])
  }
  )
  lfc = sf::st_sfc(l)
  a = seq(length(lfc)) + 1 # sequence to subset
  p_data = cbind(sf::st_set_geometry(p[a, ], NULL), speed)
  sf::st_sf(p_data, geometry = lfc)
}




#' Choose points on OSM feature which correspond to point on tracks.
#'
#' Calculates the closest distance between a feature (e.g. OSM lines) and a feature list (points of tracks).
#' Depending on the distance's threshold, either the closest point on the OSM linestring is chosen or the point of the track is taken if no OSM feature is close enough.
#' The function returns a track matched to the OSM features. The track is returned in crs EPSG:4326.
#'
#' @param track List of trajectory
#' @param osm_features List of OpenStreetMap features
#' @param threshold Distance threshold in meters.s
#' @return Closest distance between \code{x} and \code{y}.
#' @examples
#' track = readGPXFile("./inst/extdata/track_run.gpx")
#' bbox <- st_bbox(track)
#' osm_features = readOSMFiles(bbox, c("tertiary, secondary, sidewalk, footway"))
#' osm_track = bringFeatureToOSM(track, osm_features, 10)
#' @export
bringFeatureToOSM = function(track, osm_features, threshold){
  osm_feature = sf::st_union(c(sf::st_union(osm_features$osm_lines), sf::st_union(osm_features$osm_multilines)))
  nearestPoints = sf::st_nearest_points(track, osm_feature)

  startingPoints = sf::st_transform(sf::st_line_sample(sf::st_transform(nearestPoints, 3857), sample = 0),4326)
  endingPoints = sf::st_transform(sf::st_line_sample(sf::st_transform(nearestPoints, 3857), sample = 1),4326)

  distance = list()
  for(i in 1:length(startingPoints)){
   distance = c(distance, sf::st_distance(startingPoints[i], endingPoints[i]))
  }

  smoothedTrack = startingPoints[1]
  for (i in 1: length(distance)){
    if(distance[i] < threshold){
      smoothedTrack = c(smoothedTrack, startingPoints[i])
    } else {
      smoothedTrack = c(smoothedTrack, endingPoints[i])
    }
  }
  smoothedTrack = smoothedTrack[-1]
  track_smoothed = track
  track_smoothed$geometry = sf::st_cast(sf::st_transform(smoothedTrack, 4326), "POINT")
  return(track_smoothed)
}



#Visualize results

#' A trajectory gets analyzed with respect to movement statistics.
#'
#' The main purpose is an evaluation of trajectories tracked during sports.
#'
#' @param track List of trajectories.
#' @return Evaluation plots of track
#' @examples
#' \dontrun{
#'  track = readGPXFile("./inst/extdata/track_run.gpx")
#'  evaluateTrack(track)
#' }
#'
#' @export
evaluateTrack = function(track){
  plot(track$geometry)
  summary(track$time)
  plot(track, max.plot = 26)
  plot(track$time, 1:nrow(track))

  track_line = points2line_trajectory(track)
  plot(track_line["speed"], lwd = track_line$speed)

  m <- leaflet::addPolylines(leaflet::addTiles(leaflet::leaflet(track_line)))
  m
  return(true)
}

