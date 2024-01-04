test_that("gsnAssignSubnets works", {
  # Start with test data:
  load_test_data()

  # This is some setup that needs to be done before running the tests.
  .jaccard.GSN <- gsnPareNetGenericHierarchic( object = JACCARD.GSN )
  # gsnAddPathwaysData checks gene presence absense matrix for the names of the gene sets, so we need to
  # add it back from STLF.GSN
  .jaccard.GSN$genePresenceAbsence <- STLF.GSN$genePresenceAbsence
  .jaccard.GSN <- suppressMessages( gsnAddPathwaysData( object = .jaccard.GSN, pathways_data = PW.ORA ) )

  # Now we get to the test:
  testthat::expect_no_error( .jaccard.GSN <- gsnAssignSubnets( .jaccard.GSN ) )
  testthat::expect_contains( object = colnames(.jaccard.GSN$distances$jaccard$vertex_subnets),
                             expected = c( "vertex", "subnet" ) )

  # Lets try it with nearest neighbor paring
  .jaccard.GSN <- gsnPareNetGenericToNearestNNeighbors( object = JACCARD.GSN )
  # gsnAddPathwaysData checks gene presence absense matrix for the names of the gene sets, so we need to
  # add it back from STLF.GSN
  .jaccard.GSN$genePresenceAbsence <- STLF.GSN$genePresenceAbsence
  .jaccard.GSN <- suppressMessages( gsnAddPathwaysData( object = .jaccard.GSN, pathways_data = PW.ORA ) )

  # Now we get to the test:
  testthat::expect_no_error( .jaccard.GSN <- gsnAssignSubnets( .jaccard.GSN ) )
  testthat::expect_contains( object = colnames(.jaccard.GSN$distances$jaccard$vertex_subnets),
                             expected = c( "vertex", "subnet" ) )

  # Try with STLF.GSN data:
  .stlf.GSN <- STLF.GSN
  # Now we get to the test:
  testthat::expect_no_error( .stlf.GSN <- gsnAssignSubnets( .stlf.GSN ) )
  testthat::expect_contains( object = colnames(.stlf.GSN$distances$stlf$vertex_subnets),
                             expected = c( "vertex", "subnet" ) )

})
