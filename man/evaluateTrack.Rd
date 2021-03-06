% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matchGPXtoNearbyOSM.R
\name{evaluateTrack}
\alias{evaluateTrack}
\title{A GPX trajectory gets analyzed with respect to movement statistics.}
\usage{
evaluateTrack(track)
}
\arguments{
\item{track}{List of trajectory}
}
\value{
Evaluation plots of track
}
\description{
The main purpose is an evaluation of trajectories tracked during sports.
}
\examples{
\dontrun{
 track = readGPXFile("./inst/extdata/track_run.gpx")
 evaluateTrack(track)
}

}
