test_that("gsnMergePathways works", {
  # Load data files:
  testdata_path <- file.path( testthat::test_path(), "testdata" )
  rdafiles <- list.files( path = testdata_path, pattern = "\\.Rda$", full.names = TRUE )
  for( .f in rdafiles ){ load( .f ) }

  STLF.GSN.subnets <- gsnMergePathways( STLF.GSN )

  testthat::expect_true( object = all( !is.na( STLF.GSN.subnets$subnet ) ) )

  testthat::expect_true( object = all( !is.na( STLF.GSN.subnets$subnetRank ) ) )

  testthat::expect_true( object = class( STLF.GSN.subnets$subnetRank ) %in% c('integer', 'numeric') )

})
