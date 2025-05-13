test_that("gsnMergePathways works", {
  # Load test data:
  load_test_data()

  STLF.GSN.subnets <- gsnMergePathways( STLF.GSN )

  testthat::expect_true( object = all( !is.na( STLF.GSN.subnets$subnet ) ) )

  testthat::expect_true( object = all( !is.na( STLF.GSN.subnets$subnetRank ) ) )

  testthat::expect_true( object = class( STLF.GSN.subnets$subnetRank ) %in% c('integer', 'numeric') )

  # Test with id_reassign = FALSE
  STLF.GSN.cp <- STLF.GSN
  STLF.GSN.cp$pathways$id_col <- "ACCESSION"
  STLF.GSN.cp$pathways$data <- within( STLF.GSN.cp$pathways$data, { ACCESSION <- ID; ID <- NULL } )
  testthat::expect_equal( object =  colnames( gsnMergePathways( STLF.GSN.cp,id_reassign = TRUE ) ),
                          c("subnet", "subnetRank", "ID", "Title",
                            "a", "b", "c",  "d", "N",
                            "Enrichment", "P.1S", "adj.P.1S", "P.2S", "adj.P.2S" )
                          )
  testthat::expect_equal( object =  colnames( gsnMergePathways( STLF.GSN.cp,id_reassign = FALSE ) ),
                          c("subnet", "subnetRank", "ACCESSION", "Title",
                            "a", "b", "c",  "d", "N",
                            "Enrichment", "P.1S", "adj.P.1S", "P.2S", "adj.P.2S" )
  )
})
