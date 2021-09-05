test_that("filter works", {
  gpx_features_samsung = readGPXFile("../inst/extdata/track_samsung_watch.gpx")
  expect_equal(length(filterForClosePoints(gpx_features_samsung, 2)), 2)
})
