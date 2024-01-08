test_that("gsnHierarchicalDendrogram works", {
  # Load test data:
  load_test_data()

  gsnHierarchicalDendrogram( object = STLF.GSN,
                             render.plot = FALSE,
                             width = 6,
                             show.leaves = TRUE,
                             geometry = "horizontal" ) -> hd1.out

  testthat::expect_s3_class( object = hd1.out, class = "dendrogram" )

  GSNA_plot_params <- attr( x = hd1.out, which = "GSNA_plot_params" )
  testthat::expect_equal( object = class(GSNA_plot_params), expected = "list" )

  testthat::expect_true( object = GSNA_plot_params$width >=
                           sum( na.rm = TRUE,
                                c( GSNA_plot_params$tree_x_size.in,
                                   GSNA_plot_params$legend_x_size.in,
                                   GSNA_plot_params$left_margin.in,
                                   GSNA_plot_params$right_margin.in ) # right_margin.in may be null
                           ) )

  # Test creation of horizontal dendrograms
  test.f <- tempfile()
  gsnHierarchicalDendrogram( object = STLF.GSN,
                             render.plot = TRUE,
                             width = 10,
                             show.leaves = TRUE,
                             geometry = "horizontal",
                             out_format = "svg",
                             filename = test.f ) -> hd2.out

  testthat::expect_true( file.exists( test.f ) )
  testthat::expect_gt( object = file.size( test.f ), expected = 0)
  if( file.exists( test.f ) ) invisible( file.remove(test.f) )

  # Test creation of circular dendrograms
  test.f2 <- tempfile()
  gsnHierarchicalDendrogram( object = STLF.GSN,
                             render.plot = TRUE,
                             width = 10,
                             show.leaves = TRUE,
                             geometry = "circular",
                             out_format = "svg",
                             filename = test.f2 ) -> hd3.out

  testthat::expect_true( file.exists( test.f2 ) )
  testthat::expect_gt( object = file.size( test.f2 ), expected = 0)
  if( file.exists( test.f2 ) ) invisible( file.remove(test.f2) )
})

