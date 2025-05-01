test_that( "gsnSubnetsDotPlot works", {
  load( file.path( testthat::test_path(), "testdata", "STLF.GSN.Rda" ) )

  .sdp1.gg <- gsnSubnetsDotPlot( STLF.GSN, id_col = "ID", stat_col = "adj.P.1S", color_col = "Enrichment", x_transform = "log10", interactive = FALSE )
  expect_s3_class( .sdp1.gg, class = "ggplot" )

  .sdp1.pl <- gsnSubnetsDotPlot( STLF.GSN, id_col = "ID", stat_col = "adj.P.1S", color_col = "Enrichment", x_transform = "log10", interactive = TRUE )
  expect_s3_class( .sdp1.pl, class = 'plotly' )
  expect_s3_class( .sdp1.pl, class = 'htmlwidget' )
} )
