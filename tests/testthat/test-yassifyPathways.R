test_that("yassifyPathways works", {
  # Load data files:
  testdata_path <- file.path( testthat::test_path(), "testdata" )
  rdafiles <- list.files( path = testdata_path, pattern = "\\.Rda$", full.names = TRUE )
  for( .f in rdafiles ){ load( .f ) }


  URL_STEM <- "http://imaginary.fakeorg/?GS="
  ID_TO_URL.v <- structure( paste0( URL_STEM, PW.ORA$ID ), names = PW.ORA$ID )

  yassify.out <- yassifyPathways( pathways = STLF.subnetSummary,
                                  url_map_list = list(Seed.ID = ID_TO_URL.v ),
                                  url_map_by_words_list = list(IDs = ID_TO_URL.v )
  )

  testthat::expect_s3_class( object = yassify.out, "htmlwidget" )
  testthat::expect_s3_class( object = yassify.out, "datatables" )

  # Check that Seed.ID and IDs columns got links:
  link.regex <- "^<a\\shref=\"http://imaginary\\.fakeorg/\\?GS=GS\\d+\"\\starget=\"_blank\">GS\\d+</a>"
  testthat::expect_match( yassify.out$x$data$Seed.ID,
                          regexp = link.regex )
  testthat::expect_match( yassify.out$x$data$IDs,
                          regexp = link.regex )
} )
