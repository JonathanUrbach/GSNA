test_that( "gsnPathways works", {
  load( file.path( testthat::test_path(), "testdata", "STLF.GSN.Rda" ) )
  .pathways <- gsnPathways( STLF.GSN )
  expect_equal( object = .pathways, STLF.GSN$pathways$data )

  # Retrieval gsnPathways()
  .STLF.GSN.cp <- STLF.GSN
  .pathways.ss <- gsnPathways( object = .STLF.GSN.cp, "ID", "Title", "N", "Enrichment", "P.1S", "adj.P.1S" )
  expect_equal( object = .pathways.ss,
                expected = .STLF.GSN.cp$pathways$data[,c("ID", "Title", "N", "Enrichment", "P.1S", "adj.P.1S")] )

  # Assignments `gsnPathways<-`()
  expect_no_error( gsnPathways( .STLF.GSN.cp, "TEST" ) <- c("A", "B", "C", "D", "E" ) )
  expect_equal( gsnPathways( .STLF.GSN.cp, "TEST" ), expected = c("A", "B", "C", "D", "E" ) )
  expect_error( gsnPathways( .STLF.GSN.cp, "TEST2" ) <- c("A", "B", "C", "D", "E", "F" ) )

  # Make sure factors work. (They didn't in version 0.1.6.5)
  expect_no_error( gsnPathways( .STLF.GSN.cp, "TEST_FACTOR" ) <- factor( c("A", "B", "C", "D", "E" ) ) )

  # Assign a data frame to multiple columns.
  .assign <- data.frame( TEST3 = c("A", "B", "C", "D", "E" ), TEST4 = c(1,2,3,4,5) )
  expect_no_error( gsnPathways( .STLF.GSN.cp, "TEST3", "TEST4" ) <- .assign )

  expect_no_error( .pw <- gsnPathways( .STLF.GSN.cp, "TEST3", "TEST4" ) )
  rownames( .pw ) <- NULL # We're going to ignore the rownames.
  expect_equal( object = .pw, expected = .assign )
} )
