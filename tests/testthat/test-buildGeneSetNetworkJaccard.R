test_that("buildGeneSetNetworkJaccard works", {
  # Load data files:
  testdata_path <- file.path( testthat::test_path(), "testdata" )
  rdafiles <- list.files( path = testdata_path, pattern = "\\.Rda$", full.names = TRUE )
  for( .f in rdafiles ){ load( .f ) }

  .jaccard.GSN <- buildGeneSetNetworkJaccard( ref.background = BACKGROUND_SET, geneSetCollection = GSC )

  # Does the buildGeneSetNetworkJaccard return a GSNData object?
  testthat::expect_s3_class( object = .jaccard.GSN, class = "GSNData" )

  # Are the distances equal to the test values?
  testthat::expect_equal( object = .jaccard.GSN$distances$stlf$matrix, expected = JACCARD.GSN$distances$stlf$matrix )

  # Is distance type correctly identified
  .expect <- "jaccard"
  testthat::expect_equal( object = .jaccard.GSN$default_distance, expected = .expect )
  testthat::expect_contains( object = names(.jaccard.GSN$distances), expected = .expect )
})
