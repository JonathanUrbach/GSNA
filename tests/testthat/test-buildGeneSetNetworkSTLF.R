test_that("buildGeneSetNetworkSTLF works", {
  # Load test data:
  load_test_data()

  .stlf.GSN <- buildGeneSetNetworkSTLF( ref.background = BACKGROUND_SET, geneSetCollection = GSC )

  # Does the buildGeneSetNetworkSTLF return a GSNData object?
  testthat::expect_s3_class( object = .stlf.GSN, class = "GSNData" )

  # Are the distances equal to the test values?
  testthat::expect_equal( object = .stlf.GSN$distances$stlf$matrix, expected = STLF.GSN$distances$stlf$matrix )

  # Is distance type correctly identified
  .expect <- "stlf"
  testthat::expect_equal( object = .stlf.GSN$default_distance, expected = .expect )
  testthat::expect_contains( object = names(.stlf.GSN$distances), expected = .expect )
})
