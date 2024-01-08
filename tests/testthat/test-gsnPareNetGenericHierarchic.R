test_that("gsnPareNetGenericHierarchic works", {
  # Load test data:
  load_test_data()

  #"matrix" "optimal_extreme" "vertices"
  .jaccard.GSN <- gsnPareNetGenericHierarchic( JACCARD.GSN )

  # Check for new fields:
  paring_fields <- c( "hclust", "paring_call_arguments",  "pared", "pared_optimal_extreme",
                      "clusters", "edges", "orphanVertices" )
  testthat::expect_contains(object = names(.jaccard.GSN$distances$jaccard),
                            expected = paring_fields )
  testthat::expect_s3_class( object = .jaccard.GSN$distances$jaccard$hclust, class = "hclust" )

} )
