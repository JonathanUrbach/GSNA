test_that("gsIntersectCounts works", {
  # Load data files:
  testdata_path <- file.path( testthat::test_path(), "testdata" )
  rdafiles <- list.files( path = testdata_path, pattern = "\\.Rda$", full.names = TRUE )
  for( .f in rdafiles ){ load( .f ) }
  testthat::expect_no_error( .out <- gsIntersectCounts( SIM_UP_GENES[1:90], gs2 = SIM_UP_GENES[60:139], 139 ) )
  testthat::expect_equal( object = .out, c(a=0, b=59, c=49, d=31) )
  testthat::expect_equal( object = names(.out), c("a", "b", "c", "d") )

  testthat::expect_no_error( .out2 <- gsIntersectCounts( SIM_UP_GENES[1:60], gs2 = SIM_UP_GENES[61:139], 140 ) )
  testthat::expect_equal( object = .out2, c(a=1, b=60, c=79, d=0) )
})
