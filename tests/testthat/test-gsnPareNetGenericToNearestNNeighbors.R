test_that("gsnPareNetGenericToNearestNNeighbors works", {
  # Load test data:
  load_test_data()

  #"matrix" "optimal_extreme" "vertices"
  .jaccard.GSN <- gsnPareNetGenericToNearestNNeighbors( JACCARD.GSN )

  # Check for new fields:
  paring_fields <- c( "paring_call_arguments",  "pared", "pared_optimal_extreme",
                      "edges", "vertices", "orphanVertices" )
  testthat::expect_contains(object = names(.jaccard.GSN$distances$jaccard),
                            expected = paring_fields )
  testthat::expect_s3_class( object = .jaccard.GSN$distances$jaccard$edges, class = "data.frame" )
  testthat::expect_contains( object = class(.jaccard.GSN$distances$jaccard$matrix), expected = c("matrix", "array" ) )
  testthat::expect_vector( object = .jaccard.GSN$distances$jaccard$vertices )
  # Check that there are more NAs in the pared matrix:
  nas.mat <- sum(is.na(as.vector( x = .jaccard.GSN$distances$jaccard$matrix )))
  nas.pared <- sum(is.na(as.vector( x = .jaccard.GSN$distances$jaccard$pared )))
  testthat::expect_gt( nas.pared, nas.mat )
} )
