#' Returns content of a GPX file
#'
#' source: https://geocompr.github.io/geocompkg/articles/gps-tracks.html?tag=makemoney0821-20
#'
#' @param fp Path to GPX file.
#' @return content of GPX file.
#' @examples
#' \dontrun{
#'  track = readGPXFile("./inst/extdata/track_run.gpx")
#' }
#' @export
readGPXFile = function(fp) {
  sf::st_layers(fp)
  track = sf::st_read(fp, layer ="track_points")
  return(track)
}



#' Returns all OSM features within a bounding box
#'
#' @param bbox bounding box. Must be ordered like this: left, bottom, right top
#' @return list of requested features cropped by the bounding box.
#' @examples
#' \dontrun{
#'  track = readGPXFile("./inst/extdata/track_run.gpx")
#'  bbox <- st_bbox(track)
#'  osm_features = readOSMFiles(bbox)
#' }
#' @export
readOSMFiles = function(bbox){
  osm_features <-  osmdata::osmdata_sf(osmdata::add_osm_feature(osmdata::opq(bbox), "secondary", ""), quiet = FALSE)

  names(bbox) <- c("left", "bottom","right", "top")
  osm_features_cropped = osm_features
  osm_features_cropped$bbox = bbox
  osm_features_cropped$osm_lines = sf::st_crop(osm_features$osm_lines, bbox)
  osm_features_cropped$osm_points = sf::st_crop(osm_features$osm_points, bbox)
  osm_features_cropped$osm_multilines = sf::st_crop(osm_features$osm_multilines, bbox)
  osm_features_cropped$osm_polygons = sf::st_crop(osm_features$osm_polygons, bbox)
  osm_features_cropped$osm_multipolygons = sf::st_crop(osm_features$osm_multipolygons, bbox)
  return(osm_features_cropped)
}


#' Delete Outliers in Tracks
#'
#' This function selects points that are too far away from its next neighboring points.
#'
#' The distance can be determined by the threshold.
#' Whenever the distance to one direct neighboring point is less than the threshold, the point will be kept.
#' It has the main purpose to identify outliers of GPS measurements when the receiver misslocated itself
#'
#' @param track List of trajectory.
#' @param threshold Distance threshold. Unit based on crs of track of Points.
#' @return Returns all entries of the \code{trackOfPoints} that are close enough.
#' @examples
#' \dontrun{
#' track = readGPXFile("./inst/extdata/track_run.gpx")
#' filteredTrack = filterForClosePoints(track, 2)
#' }
#' @export
filterForClosePoints = function(track, threshold){
  validPoints = data.frame(track[1,])
  thresholdUnits = units::set_units(threshold, 'm')
  for(x in 2:(length(track$geometry)-1)){
    distMatrix = sf::st_distance(c(track$geometry[x-1], track$geometry[x], track$geometry[x+1]))
    if(distMatrix[1,2] < thresholdUnits|| distMatrix[2,3] < thresholdUnits){
      validPoints <- rbind(
        validPoints,
        stats::setNames(data.frame(track[x,]), colnames(validPoints))
      )
    }
  }
  validPoints <- rbind(
    validPoints,
    stats::setNames(data.frame(track[length(track$geometry),]), colnames(validPoints))
  )
  return(validPoints)
}
