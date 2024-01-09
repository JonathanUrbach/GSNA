test_that("distMat2UnitNormRank works", {
  # Load test data
  load_test_data()

  # distMat2UnitNormRank
  .stlf.unr.mat <- distMat2UnitNormRank( STLF.GSN$distances$stlf$matrix)
  testthat::expect_true( attr( x = .stlf.unr.mat, which = "lower_is_closer", exact = TRUE ) )

  .jaccard.unr.mat <- distMat2UnitNormRank( JACCARD.GSN$distances$jaccard$matrix)
  testthat::expect_true( attr( x = .jaccard.unr.mat, which = "lower_is_closer", exact = TRUE ) )

  # negative
  testthat::expect_warning( .neg_stlf <- GSNA::negative( STLF.GSN$distances$stlf$matrix ) )
  testthat::expect_contains( object = class(.neg_stlf), expected = c("matrix", "array") )
  testthat::expect_equal( object = dim( .neg_stlf ), expected = dim(STLF.GSN$distances$stlf$matrix) )
  testthat::expect_equal( object = attr( x = .neg_stlf, which = "lower_is_closer" ), expected = FALSE )
  testthat::expect_equal( object = attr( x = .neg_stlf, which = "distance" ), expected = "negative_stlf" )

  testthat::expect_no_warning( .neg_jaccard <- GSNA::negative( JACCARD.GSN$distances$jaccard$matrix ) )
  testthat::expect_equal( object = attr( x = .neg_jaccard, which = "lower_is_closer" ), expected = TRUE )
  testthat::expect_equal( object = attr( x = .neg_jaccard, which = "distance" ), expected = "negative_jaccard" )

  # complement
  testthat::expect_warning( .comp_stlf <- GSNA::complement( STLF.GSN$distances$stlf$matrix ) )
  testthat::expect_contains( object = class(.comp_stlf), expected = c("matrix", "array") )
  testthat::expect_equal( object = dim( .comp_stlf ), expected = dim(STLF.GSN$distances$stlf$matrix) )
  testthat::expect_equal( object = attr( x = .comp_stlf, which = "lower_is_closer" ), expected = FALSE )
  testthat::expect_equal( object = attr( x = .comp_stlf, which = "distance" ), expected = "complement_stlf" )

  testthat::expect_no_warning( .comp_jaccard <- GSNA::complement( JACCARD.GSN$distances$jaccard$matrix ) )
  testthat::expect_equal( object = attr( x = .comp_jaccard, which = "lower_is_closer" ), expected = TRUE )
  testthat::expect_equal( object = attr( x = .comp_jaccard, which = "distance" ), expected = "complement_jaccard" )

  testthat::expect_warning( GSNA::complement( c( 0, 0.5, 0.9, 1, 1.1) ) )

})
