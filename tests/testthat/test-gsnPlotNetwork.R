test_that("gsnPlotNetwork works", {
  # Load data files:
  testdata_path <- file.path( testthat::test_path(), "testdata" )
  rdafiles <- list.files( path = testdata_path, pattern = "\\.Rda$", full.names = TRUE )
  for( .f in rdafiles ){ load( .f ) }

  # 1 channel network plot
  temp1.f <- tempfile()

  # Currently, this emits warnings about the the node size legend not having sufficient space to
  # plot. I'm planning on fixing this, but for the purposes of the test, I'm suppressing the
  # warnings.
  suppressWarnings( gsnPlotNetwork( object = STLF.GSN,
                  filename = temp1.f,
                  width = 10,
                  height = 10,
                  out_format = "svg" ) )-> pn1.out

  testthat::expect_true( object = file.exists( temp1.f ) )
  testthat::expect_true( object = file.size( temp1.f ) > 0 )
  if( file.exists( temp1.f ) ) invisible( file.remove( temp1.f ) )

  testthat::expect_s3_class( object = pn1.out, class = "igraph" )

  GSNA_plot_params <- attr( x = pn1.out, which = "GSNA_plot_params" )

  testthat::expect_equal( object = class(GSNA_plot_params), expected = "list" )

  # 2 channel network plot
  temp2.f <- tempfile()

  # Currently, this emits warnings about the the node size legend not having sufficient space to
  # plot. I'm planning on fixing this, but for the purposes of the test, I'm suppressing the
  # warnings.
  suppressWarnings( gsnPlotNetwork( object = STLF.GSN,
                  filename = temp2.f,
                  width = 10,
                  height = 10,
                  stat_col_2 = 'adj.P.2S',
                  sig_order_2 = 'loToHi',
                  out_format = "svg" ) ) -> pn2.out

  testthat::expect_true( object = file.exists( temp2.f ) )
  testthat::expect_true( object = file.size( temp2.f ) > 0 )
  if( file.exists( temp2.f ) ) invisible( file.remove( temp2.f ) )

  testthat::expect_s3_class( object = pn2.out, class = "igraph" )

  GSNA_plot_params <- attr( x = pn2.out, which = "GSNA_plot_params" )

  testthat::expect_equal( object = class(GSNA_plot_params), expected = "list" )
})
