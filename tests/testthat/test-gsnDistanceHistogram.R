test_that("gsnDistanceHistogram works", {
  # Load data files:
  load_test_data()

  # Run gsnDistanceHistogram
  testthat::expect_no_error( p <- gsnDistanceHistogram( object = STLF.GSN ) )
  test_out <- tempfile()
  suppressMessages( testthat::expect_no_error( ggplot2::ggsave( filename = test_out, plot = p, device = "png" ) ) )

  testthat::expect_no_error( p <- gsnDistanceHistogram( object = STLF.GSN, stat = "count" ) )
  suppressMessages( testthat::expect_no_error( ggplot2::ggsave( filename = test_out, plot = p, device = "png" ) ) )

  testthat::expect_no_error( p <- gsnDistanceHistogram( object = STLF.GSN, stat = "density" ) )
  suppressMessages( testthat::expect_no_error( ggplot2::ggsave( filename = test_out, plot = p, device = "png" ) ) )
})
