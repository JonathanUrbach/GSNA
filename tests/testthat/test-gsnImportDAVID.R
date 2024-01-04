test_that("gsnImportDAVID works", {
  # Load data files:
  load_test_data()

  # Copy STLF.GSN and delete pathways data.
  STLF.GSN.fd <- STLF.GSN
  STLF.GSN.fd$pathways <- NULL
  # Create fake david data
  PW.fake_david <- fake_david_chart( ora_data = PW.ORA, .gsc = GSC )

  # See if it imports DAVID data
  STLF.GSN.fd <- gsnImportDAVID( object = STLF.GSN.fd, pathways_data = PW.fake_david )

  # Test STLF.GSN.fd object:
  testthat::expect_contains( object = names(STLF.GSN.fd), expected = "pathways" )
  testthat::expect_false( object = is.null( STLF.GSN.fd$pathways ) )
  testthat::expect_equal( object = STLF.GSN.fd$pathways$type, expected = "david" )

  testthat::expect_equal( object = STLF.GSN.fd$pathways$data, expected = PW.fake_david )

  testthat::expect_equal( object = STLF.GSN.fd$pathways$id_col, expected = "Term" )
  testthat::expect_equal( object = STLF.GSN.fd$pathways$stat_col, expected = "FDR" )
  testthat::expect_equal( object = STLF.GSN.fd$pathways$sig_order, expected = "loToHi" )
  testthat::expect_equal( object = STLF.GSN.fd$pathways$n_col, expected = "Count" )

  # Create some error states: See if it imports DAVID data with bad id_col, stat_col, sig_order
  testthat::expect_error( gsnImportDAVID( object = STLF.GSN.fd, pathways_data = PW.fake_david, id_col = "INVALID" ) )
  testthat::expect_error( gsnImportDAVID( object = STLF.GSN.fd, pathways_data = PW.fake_david, stat_col = "INVALID" ) )
  testthat::expect_error( gsnImportDAVID( object = STLF.GSN.fd, pathways_data = PW.fake_david, sig_order = "INVALID" ) )

  # Try again, but use GSNORA data:
  STLF.GSN.fd$pathways <- NULL
  # This should produce a warning and an error
  testthat::expect_warning(
    testthat::expect_error(STLF.GSN.fd <- gsnImportDAVID( object = STLF.GSN.fd, pathways_data = PW.ORA ) )
  )

  testthat::expect_error( object = suppressWarnings( gsnImportDAVID( object = STLF.GSN.fd, pathways_data = PW.ORA, id_col = "ID" ) ) )
  testthat::expect_error( object = suppressWarnings( gsnImportDAVID( object = STLF.GSN.fd, pathways_data = PW.ORA, id_col = "ID", stat_col = "adj.P.2S" ) ) )
  testthat::expect_no_error( STLF.GSN.fd <- suppressWarnings( gsnImportDAVID( object = STLF.GSN.fd, pathways_data = PW.ORA, id_col = "ID", stat_col = "adj.P.2S", sig_order = 'loToHi' ) ) )
  STLF.GSN.fd$pathways$id_col <- "Term"

  # Try with read in from DAVID Functional Annotation Cluster Data Files:
  fake_david_chart.file <- tempfile()
  write_fake_david_cluster( fake_david_chart = PW.fake_david, outfile = fake_david_chart.file )
  STLF.GSN.fd$pathways <- NULL
  testthat::expect_no_error( gsnImportDAVID( object = STLF.GSN.fd, filename = fake_david_chart.file ) )
  write_fake_david_chart( fake_david_chart = PW.fake_david, outfile = fake_david_chart.file )
  STLF.GSN.fd$pathways <- NULL
  testthat::expect_no_error( gsnImportDAVID( object = STLF.GSN.fd, filename = fake_david_chart.file ) )
})
