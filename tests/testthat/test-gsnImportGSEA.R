test_that("gsnImportGSEA works", {
  # Load data files:
  load_test_data()

  # Copy STLF.GSN and delete pathways data.
  STLF.GSN.fg <- STLF.GSN
  STLF.GSN.fg$pathways <- NULL
  # Create fake gsea data
  PW.fake_gsea <- fake_GSEA_data( ora_data = PW.ORA, .gsc = GSC )

  # Does it fail when no pathways data is given?
  testthat::expect_error( gsnImportGSEA( object = STLF.GSN.fg, pathways_data = NULL ) )

  # See if it imports DAVID data
  STLF.GSN.fg <- gsnImportGSEA( object = STLF.GSN.fg, pathways_data = PW.fake_gsea )

  # Test STLF.GSN.fg object:
  testthat::expect_contains( object = names(STLF.GSN.fg), expected = "pathways" )
  testthat::expect_false( object = is.null( STLF.GSN.fg$pathways ) )
  testthat::expect_equal( object = STLF.GSN.fg$pathways$type, expected = "gsea" )

  testthat::expect_contains( object = STLF.GSN.fg$pathways$data, expected = PW.fake_gsea )

  testthat::expect_equal( object = STLF.GSN.fg$pathways$id_col, expected = "NAME" )
  testthat::expect_equal( object = STLF.GSN.fg$pathways$stat_col, expected = "NOM p-val" )
  testthat::expect_equal( object = STLF.GSN.fg$pathways$sig_order, expected = "loToHi" )
  testthat::expect_equal( object = STLF.GSN.fg$pathways$n_col, expected = "SIZE" )
})
