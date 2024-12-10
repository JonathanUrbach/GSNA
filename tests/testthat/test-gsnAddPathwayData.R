# NOTE: This is a near-duplicate of test-gsnAddPathwaysData.R. This runs the same tests on gsnAddPathwayData()
# which will be deprecated in a future GSNA version.
test_that("gsnAddPathwayData works", {
    load_test_data()

    # Copy STLF.GSN and delete pathways data.
    STLF.GSN.fd <- STLF.GSN
    STLF.GSN.fd$pathways <- NULL
    # Create fake david data
    PW.fake_david <- fake_david_chart( ora_data = PW.ORA, .gsc = GSC )

    # Import again, but specify type = "david"
    suppressWarnings(
      testthat::expect_warning(
        suppressMessages(
          testthat::expect_message(
            object = STLF.GSN.fd <- gsnAddPathwayData( object = STLF.GSN.fd, pathways_data = PW.fake_david ),
            regexp = "Using\\sDAVID\\simport."
          )
        ),
        regexp = "is included to support old code"
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
    suppressWarnings(
      testthat::expect_warning(
        suppressMessages(
          testthat::expect_message(
            object = STLF.GSN.fd <- gsnAddPathwayData( object = STLF.GSN.fd, pathways_data = PW.ORA ),
            regexp = "Using\\sGSN-ORA\\simport\\."
          )
        ),
        regexp = "is included to support old code"
      )
    )

    # Test STLF.GSN.fd object:
    testthat::expect_contains( object = names(STLF.GSN.fd), expected = "pathways" )
    testthat::expect_false( object = is.null( STLF.GSN.fd$pathways ) )
    testthat::expect_equal( object = STLF.GSN.fd$pathways$type, expected = "gsnora" )
    testthat::expect_contains( object = colnames(STLF.GSN.fd$pathways$data), expected = colnames(PW.ORA) )
    testthat::expect_equal( object = STLF.GSN.fd$pathways$id_col, expected = "ID" )
    testthat::expect_in( object = STLF.GSN.fd$pathways$stat_col, expected = c("adj.P.1S", "P.1S") )
    testthat::expect_equal( object = STLF.GSN.fd$pathways$sig_order, expected = "loToHi" )
    testthat::expect_equal( object = STLF.GSN.fd$pathways$n_col, expected = "N" )

    # Test gsnORA import with specified stat_col, stat_col_2, sig_order, sig_order_2, n_col
    suppressMessages(
      testthat::expect_message(
        suppressWarnings(
          testthat::expect_warning(
            object = STLF.GSN.fd_2 <- gsnAddPathwayData( object = STLF.GSN.fd,
                                                         pathways_data = PW.ORA,
                                                         stat_col = "P.2S",
                                                         sig_order = "loToHi",
                                                         stat_col_2 = "Enrichment",
                                                         sig_order_2 = "hiToLo",
                                                         n_col = "d" ),
            regexp = "is included to support old code"
          )

        ),
        regexp = "Using\\sGSN-ORA\\simport\\."
      )
    )
    testthat::expect_equal( object = STLF.GSN.fd_2$pathways$stat_col, expected = "P.2S" )
    testthat::expect_equal( object = STLF.GSN.fd_2$pathways$sig_order, expected = "loToHi" )
    testthat::expect_equal( object = STLF.GSN.fd_2$pathways$stat_col_2, expected = "Enrichment" )
    testthat::expect_equal( object = STLF.GSN.fd_2$pathways$sig_order_2, expected = "hiToLo" )
    testthat::expect_equal( object = STLF.GSN.fd_2$pathways$n_col, expected = "d" )
})
