test_that("filter works", {

  gpx_features_samsung = readGPXFile("track_run.gpx")
  expect_equal(length(filterForClosePoints(gpx_features_samsung, 2)), 27)
})
