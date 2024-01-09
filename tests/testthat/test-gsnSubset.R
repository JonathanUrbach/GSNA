test_that("gsnSubset works", {
  # Load test data:
  load_test_data()

  # Subset for cluster 1
  testthat::expect_no_error( c1.stlf.GSN <- gsnSubset( object = STLF.GSN, subnet_names = 1 ) )
  testthat::expect_contains( object = class(c1.stlf.GSN), expected = "GSNData" )
  testthat::expect_true( object = all( c1.stlf.GSN$distances$stlf$vertex_subnets$subnet == 1 ) )

  testthat::expect_no_error( GS0001_2.GSN <- gsnSubset( object = STLF.GSN, vertex_names = c("GS0004", "GS0005" ) ) )
  testthat::expect_setequal( object = GS0001_2.GSN$distances$stlf$vertex_subnets$vertex, expected = c("GS0004", "GS0005" ) )

})
