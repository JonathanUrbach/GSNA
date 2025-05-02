test_that("buildGeneSetNetworkKappa works", {
  # Load test data:
  load_test_data()

  .Kappa.GSN <- buildGeneSetNetworkKappa( ref.background = BACKGROUND_SET, geneSetCollection = GSC )

  # Does the buildGeneSetNetworkKappa return a GSNData object?
  testthat::expect_s3_class( object = .Kappa.GSN, class = "GSNData" )

  # Are the distances equal to the test values?
  testthat::expect_equal( object = .Kappa.GSN$distances$stlf$matrix, expected = .Kappa.GSN$distances$stlf$matrix )

  # Is distance type correctly identified
  .expect <- "kappa"
  testthat::expect_equal( object = .Kappa.GSN$default_distance, expected = .expect )
  testthat::expect_contains( object = names(.Kappa.GSN$distances), expected = .expect )
})
