test_that("gsnImportGenericPathways works", {
  load_test_data()

  # Copy STLF.GSN and delete pathways data.
  STLF.GSN.fd <- STLF.GSN
  STLF.GSN.fd$pathways <- NULL
  # Create fake david data
  PW.fake_david <- fake_david_chart( ora_data = PW.ORA, .gsc = GSC )

  # See if it can do a generic import of DAVID data
  suppressMessages(
    testthat::expect_message( object = STLF.GSN.fd <- gsnImportGenericPathways( object = STLF.GSN.fd, pathways_data = PW.fake_david ),
                              regexp =  "id_col\\s=\\sTerm.+stat_col\\s=\\sBonferroni.+sig_order\\s=\\sloToHi" )
    )

  # Test STLF.GSN.fd object:
  testthat::expect_contains( object = names(STLF.GSN.fd), expected = "pathways" )
  testthat::expect_false( object = is.null( STLF.GSN.fd$pathways ) )
  testthat::expect_equal( object = STLF.GSN.fd$pathways$type, expected = "generic" )

  # Import again, but specify type = "david"
  suppressMessages(
    testthat::expect_message(
      object = ( STLF.GSN.fd <- gsnImportGenericPathways( object = STLF.GSN.fd,
                                                          pathways_data = PW.fake_david,
                                                          type = "david" ) ),
      regexp =  "id_col\\s=\\sTerm.+stat_col\\s=\\sBonferroni.+sig_order\\s=\\sloToHi"
                            )
    )

  testthat::expect_equal( object = STLF.GSN.fd$pathways$type, expected = "david" )

  testthat::expect_contains( object = colnames(STLF.GSN.fd$pathways$data), expected = colnames(PW.fake_david) )

  testthat::expect_equal( object = STLF.GSN.fd$pathways$id_col, expected = "Term" )
  testthat::expect_in( object = STLF.GSN.fd$pathways$stat_col,
                       expected = c("Bonferroni", "FDR", "Benjamini", "PValue") )
  testthat::expect_equal( object = STLF.GSN.fd$pathways$sig_order, expected = "loToHi" )
  testthat::expect_equal( object = STLF.GSN.fd$pathways$n_col, expected = "Count" )

  # Now, import gsnORA data:
  STLF.GSN.fd$pathways <- NULL
  STLF.GSN.fd <- suppressMessages( gsnImportGenericPathways( object = STLF.GSN.fd, pathways_data = PW.ORA ) )

  # Test STLF.GSN.fd object:
  testthat::expect_contains( object = names(STLF.GSN.fd), expected = "pathways" )
  testthat::expect_false( object = is.null( STLF.GSN.fd$pathways ) )
  testthat::expect_equal( object = STLF.GSN.fd$pathways$type, expected = "generic" )

  testthat::expect_contains( object = colnames(STLF.GSN.fd$pathways$data), expected = colnames(PW.ORA) )

  testthat::expect_equal( object = STLF.GSN.fd$pathways$id_col, expected = "ID" )
  testthat::expect_in( object = STLF.GSN.fd$pathways$stat_col, expected = c("P.1S") )
  testthat::expect_equal( object = STLF.GSN.fd$pathways$sig_order, expected = "loToHi" )
  testthat::expect_equal( object = STLF.GSN.fd$pathways$n_col, expected = "N" )


  # This next test is to address a bug that happens when a stat_col is specified as an argument,
  # and none of the columns in the data set are automatically recognized as statistical columns.
  # This issue was outlined here: https://github.com/JonathanUrbach/GSNA/issues/7
  STLF.GSN.fake_generic <- STLF.GSN.fd
  STLF.GSN.fake_generic$pathways <- NULL
  # We're renaming the columns to be non-recognizable by the code that automatically identifies stat_cols:
  PW.fake_generic <- dplyr::rename( .data = PW.fake_david,
                                    stat1 = PValue,
                                    stat2 = Bonferroni,
                                    stat3 = Benjamini,
                                    stat4 = FDR )

  # We expect a message and no error:
  testthat::expect_message( testthat::expect_no_error( STLF.GSN.fake_generic <- gsnImportGenericPathways( object = STLF.GSN.fake_generic,
                                                     pathways_data = PW.fake_generic,
                                                     type = "fake_generic",
                                                     stat_col = "stat1", sig_order = "loToHi",
                                                     stat_col_2 = "Fold Enrichment", sig_order_2 = "hiToLo"
  ) ), regexp = "id_col = Term" )

  testthat::expect_equal( object = STLF.GSN.fake_generic$pathways$stat_col, expected = "stat1" )
  testthat::expect_equal( object = STLF.GSN.fake_generic$pathways$sig_order, expected = "loToHi" )
  testthat::expect_equal( object = STLF.GSN.fake_generic$pathways$stat_col_2, expected = "Fold Enrichment" )
  testthat::expect_equal( object = STLF.GSN.fake_generic$pathways$sig_order_2, expected = "hiToLo" )


})

