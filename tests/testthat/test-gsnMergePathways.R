test_that("gsnMergePathways works", {
  # Load test data:
  load_test_data()

  STLF.GSN.subnets <- gsnMergePathways( STLF.GSN )

  testthat::expect_true( object = all( !is.na( STLF.GSN.subnets$subnet ) ) )

  testthat::expect_true( object = all( !is.na( STLF.GSN.subnets$subnetRank ) ) )

  testthat::expect_true( object = class( STLF.GSN.subnets$subnetRank ) %in% c('integer', 'numeric') )
})
