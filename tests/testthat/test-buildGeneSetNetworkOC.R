test_that("buildGeneSetNetworkOC works", {
  # Load data files:
  testdata_path <- file.path( testthat::test_path(), "testdata" )
  rdafiles <- list.files( path = testdata_path, pattern = "\\.Rda$", full.names = TRUE )
  for( .f in rdafiles ){ load( .f ) }

  .oc.GSN <- buildGeneSetNetworkOC( ref.background = BACKGROUND_SET, geneSetCollection = GSC )

  # Does the buildGeneSetNetworkOC return a GSNData object?
  testthat::expect_s3_class( object = .oc.GSN, class = "GSNData" )

  # Are the distances equal to the test values?
  testthat::expect_equal( object = .oc.GSN$distances$stlf$matrix, expected = OC.GSN$distances$stlf$matrix )

  # Is distance type correctly identified
  .expect <- "oc"
  testthat::expect_equal( object = .oc.GSN$default_distance, expected = .expect )
  testthat::expect_contains( object = names(.oc.GSN$distances), expected = .expect )
})
