test_that("calculate_stlf_vector works", {
  # Load test data:
  load_test_data()

  genePresenceAbsence <- STLF.GSN$genePresenceAbsence
  stlf.v <- calculate_stlf_vector( mat = genePresenceAbsence )
  combinations_expect <- sum( 1:(ncol(genePresenceAbsence)-1) )
  testthat::expect_equal( object = length( stlf.v ), expected = combinations_expect )

  testthat::expect_equal( object = class(stlf.v), expected = "numeric" )
})
