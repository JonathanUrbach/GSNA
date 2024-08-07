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
})

