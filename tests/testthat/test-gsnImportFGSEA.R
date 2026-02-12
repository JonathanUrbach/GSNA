test_that("gsnImportFGSEA works", {
  # Load data files:
  load_test_data()

  testthat::expect_true( object = exists( "PW.ORA" ) )

  # Copy STLF.GSN and delete pathways data.
  STLF.GSN.fg <- STLF.GSN
  STLF.GSN.fg$pathways <- NULL

  # Create fake FGSEA data
  PW.fake_fgsea <- fake_FGSEA_data( ora_data = PW.ORA, .gsc = GSC )

  # Does it fail when no pathways data is given?
  testthat::expect_error( gsnImportFGSEA( object = STLF.GSN.fg, pathways_data = NULL ) )

  # See if it imports FGSEA data
  STLF.GSN.fg <- gsnImportFGSEA( object = STLF.GSN.fg, pathways_data = PW.fake_fgsea )

  # Test STLF.GSN.fg object:
  testthat::expect_contains( object = names(STLF.GSN.fg), expected = "pathways" )
  testthat::expect_false( object = is.null( STLF.GSN.fg$pathways ) )
  testthat::expect_equal( object = STLF.GSN.fg$pathways$type, expected = "fgsea" )

  testthat::expect_contains( object = STLF.GSN.fg$pathways$data, expected = PW.fake_fgsea )

  testthat::expect_equal( object = STLF.GSN.fg$pathways$id_col, expected = "pathway" )
  testthat::expect_equal( object = STLF.GSN.fg$pathways$stat_col, expected = "pval" )
  testthat::expect_equal( object = STLF.GSN.fg$pathways$sig_order, expected = "loToHi" )
  testthat::expect_equal( object = STLF.GSN.fg$pathways$n_col, expected = "size" )
})
