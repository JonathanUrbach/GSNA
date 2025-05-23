test_that( "gsnSubnetsDotPlot works", {
  load( file.path( testthat::test_path(), "testdata", "STLF.GSN.Rda" ) )

  .sdp1.gg <- gsnSubnetsDotPlot( STLF.GSN, id_col = "ID", stat_col = "adj.P.1S", color_col = "Enrichment", x_transform = "log10", interactive = FALSE )
  expect_s3_class( .sdp1.gg, class = "ggplot" )

  .sdp1.pl <- gsnSubnetsDotPlot( STLF.GSN, id_col = "ID", stat_col = "adj.P.1S", color_col = "Enrichment", x_transform = "log10", interactive = TRUE )
  expect_s3_class( .sdp1.pl, class = 'plotly' )
  expect_s3_class( .sdp1.pl, class = 'htmlwidget' )

  # Test subsetting. The following tests depend on the ability to look inside the returned ggplot object to get the data:
  # Bare expression:
  .filt.e10.gg <- gsnSubnetsDotPlot( STLF.GSN,
                                     id_col = "ID",
                                     stat_col = "adj.P.1S",
                                     color_col = "Enrichment",
                                     x_transform = "log10",
                                     interactive = FALSE,
                                     subset = Enrichment < 10
  )
  expect_true( object = all( .filt.e10.gg$data$Enrichment < 10 ) )

  # Expression:
  .filt.N_gt_10.expr.gg <- gsnSubnetsDotPlot( STLF.GSN,
                                              id_col = "ID",
                                              stat_col = "adj.P.1S",
                                              color_col = "Enrichment",
                                              x_transform = "log10",
                                              interactive = FALSE,
                                              subset = expression( N > 100 ) )
  expect_true( object = all( .filt.N_gt_10.expr.gg$data$N > 100 ) )

  # Function returning boolean:
  .filt.b_gt_10.fn.gg <- gsnSubnetsDotPlot( STLF.GSN,
                                            id_col = "ID",
                                            stat_col = "adj.P.1S",
                                            color_col = "Enrichment",
                                            x_transform = "log10",
                                            interactive = FALSE,
                                            subset = function(){ b > 10 } )
  expect_true( object = all( .filt.b_gt_10.fn.gg $data$b > 10 ) )

  # Ordering of results:
  # Warning for non-unique value of function for each group_by
  expect_warning( .sort.neg_b.fn.gg <- gsnSubnetsDotPlot( STLF.GSN,
                                                            id_col = "ID",
                                                            stat_col = "adj.P.1S",
                                                            color_col = "Enrichment",
                                                            x_transform = "log10",
                                                            interactive = FALSE,
                                                            order_by = function(){ -b } ),
                  regexp = "The order_by function returns more than one unique value"
  )


  # Ordering with proper function that returns single values
  expect_no_warning( .sort.mean_Enrichment.fun2.gg <- gsnSubnetsDotPlot( STLF.GSN,
                                                            id_col = "ID",
                                                            stat_col = "adj.P.1S",
                                                            color_col = "Enrichment",
                                                            x_transform = "log10",
                                                            interactive = FALSE,
                                                            order_by = function() mean(Enrichment) )
  )

  {
    .sort.mean_Enrichment.fun2.order_expect <-  with( .sort.mean_Enrichment.fun2.gg$data,
                                              aggregate( x = list(Enrichment = Enrichment),
                                                         by = list(subnet = subnet),
                                                         FUN = mean ) )
    .sort.mean_Enrichment.fun2.order_expect <- as.character( .sort.mean_Enrichment.fun2.order_expect[order(.sort.mean_Enrichment.fun2.order_expect$Enrichment),'subnet'] )
  }

  expect_equal( object = levels( .sort.mean_Enrichment.fun2.gg$data$subnet ), expected = .sort.mean_Enrichment.fun2.order_expect )


  # Ordering with proper function that returns single values
  expect_no_warning( .sort.min_P.1S.expr.gg <- gsnSubnetsDotPlot( STLF.GSN,
                                                                 id_col = "ID",
                                                                 stat_col = "adj.P.1S",
                                                                 color_col = "Enrichment",
                                                                 x_transform = "log10",
                                                                 interactive = FALSE,
                                                                 order_by = -min(P.1S) )
  )

  {
    .sort.min_P.1S.expr.order_expect <-  with( .sort.min_P.1S.expr.gg$data,
                                              aggregate( x = list(P.1S = P.1S),
                                                         by = list(subnet = subnet),
                                                         FUN = function(x){-min(x)} ) )
    .sort.min_P.1S.expr.order_expect <- as.character( .sort.min_P.1S.expr.order_expect[order(.sort.min_P.1S.expr.order_expect$P.1S),'subnet'] )
  }

  expect_equal( object = levels( .sort.min_P.1S.expr.gg$data$subnet ), expected = .sort.min_P.1S.expr.order_expect )

  # Categorical colors in plot.
  STLF.GSN.cp2 <- STLF.GSN
  .Test.vals <- factor( c("Fancy", "Cheap", "Fancy", "Cheap", NA ) )
  gsnPathways( STLF.GSN.cp2, "Test" ) <- .Test.vals
  STLF.GSN.cp2.pw <- gsnPathways( STLF.GSN.cp2 )
  rownames( STLF.GSN.cp2.pw ) <- STLF.GSN.cp2.pw$ID

  expect_no_error( .categorical.gg <- gsnSubnetsDotPlot( STLF.GSN.cp2,
                                                         id_col = "ID",
                                                         stat_col = "adj.P.1S",
                                                         color_col = "Test",
                                                         x_transform = "log10",
                                                         interactive = FALSE )
  )
  # Checking if the fill attribute is the "Test" column.
  expect_equal( object = rlang::eval_tidy( .categorical.gg$mapping$fill, data = .categorical.gg$data ),
                expected = STLF.GSN.cp2.pw[.categorical.gg$data$ID, "Test"] )

} )
