test_that("gsnImportGSNORA works", {
  # Load data files:
  load_test_data()

  # Copy STLF.GSN and delete pathways data.
  STLF.GSN.ora <- STLF.GSN
  STLF.GSN.ora$pathways <- NULL

  # Does it fail when no pathways data is given?
  testthat::expect_error( gsnImportGSNORA( object = STLF.GSN.ora, pathways_data = NULL ) )

  # See if it imports GSNORA data
  STLF.GSN.ora <- gsnImportGSNORA( object = STLF.GSN.ora, pathways_data = PW.ORA )

  # Test STLF.GSN.ora object:
  testthat::expect_contains( object = names(STLF.GSN.ora), expected = "pathways" )
  testthat::expect_false( object = is.null( STLF.GSN.ora$pathways ) )
  testthat::expect_equal( object = STLF.GSN.ora$pathways$type, expected = "gsnora" )

  testthat::expect_contains( object = STLF.GSN.ora$pathways$data, expected = PW.ORA )

  testthat::expect_equal( object = STLF.GSN.ora$pathways$id_col, expected = "ID" )
  testthat::expect_equal( object = STLF.GSN.ora$pathways$stat_col, expected = "adj.P.1S" )
  testthat::expect_equal( object = STLF.GSN.ora$pathways$sig_order, expected = "loToHi" )
  testthat::expect_equal( object = STLF.GSN.ora$pathways$n_col, expected = "N" )

  STLF.GSN.ora$pathways <- NULL
  .pw.ora <- PW.ORA
  .pw.ora$log10P.1S <- log10( .pw.ora$P.1S )
  # Import with invalid stat_col:
  testthat::expect_error( gsnImportGSNORA( object = STLF.GSN.ora,
                                           pathways_data = .pw.ora,
                                           id_col = "ID",
                                           stat_col = "invalid" ) )
  # Do it correctly:
  STLF.GSN.ora <- gsnImportGSNORA( object = STLF.GSN.ora,
                                   pathways_data = .pw.ora,
                                   id_col = "ID",
                                   stat_col = "log10P.1S",
                                   sig_order = "hiToLo",
                                   n_col = "N" )


})
