test_that("snParedVsRawDistancePlot works", {
  # Start with test data:
  load_test_data()

  # This is some setup that needs to be done before running the tests.
  .jaccard.GSN <- gsnPareNetGenericHierarchic( object = JACCARD.GSN )
  # gsnAddPathwaysData checks gene presence absense matrix for the names of the gene sets, so we need to
  # add it back from STLF.GSN
  .jaccard.GSN$genePresenceAbsence <- STLF.GSN$genePresenceAbsence
  .jaccard.GSN <- suppressMessages( gsnAddPathwaysData( object = .jaccard.GSN, pathways_data = PW.ORA ) )

  # Now we get to the test:
  {
    .outfile <- tempfile()
    png( .outfile )
    testthat::expect_no_error(  gsnParedVsRawDistancePlot( .jaccard.GSN ) )
    invisible(dev.off())
    testthat::expect_true(  object = file.size(.outfile) > 0 )

    .jaccard.GSN$default_distance <- NULL
    png( .outfile )
    testthat::expect_error(  gsnParedVsRawDistancePlot( .jaccard.GSN ) )
    invisible(dev.off())

    .jaccard.GSN$default_distance <- "jaccard"

    png( .outfile )
    testthat::expect_error(  gsnParedVsRawDistancePlot( JACCARD.GSN ) )
    invisible(dev.off())
  }
})
