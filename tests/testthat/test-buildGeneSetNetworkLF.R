test_that("buildGeneSetNetworkLF works", {
  # Load data files:
  testdata_path <- file.path( testthat::test_path(), "testdata" )
  rdafiles <- list.files( path = testdata_path, pattern = "\\.Rda$", full.names = TRUE )
  for( .f in rdafiles ){ load( .f ) }

  .oc.GSN <- buildGeneSetNetworkLF( ref.background = BACKGROUND_SET, geneSetCollection = GSC )

  # Does the buildGeneSetNetworkLF return a GSNData object?
  testthat::expect_s3_class( object = .oc.GSN, class = "GSNData" )

  # Is distance type correctly identified
  .expect <- "lf"
  testthat::expect_equal( object = .oc.GSN$default_distance, expected = .expect )
  testthat::expect_contains( object = names(.oc.GSN$distances), expected = .expect )
})
