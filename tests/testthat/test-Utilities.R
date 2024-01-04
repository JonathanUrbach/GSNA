test_that("Utilities works", {
  # Load data
  load_test_data()
  # Create a GSNData object with multiple distance metrics:
  MULTI.GSN <- STLF.GSN
  MULTI.GSN$distances$jaccard <- JACCARD.GSN$distances$jaccard
  MULTI.GSN$distances$oc <- OC.GSN$distances$oc

  # test gsn_default_distance
  testthat::expect_equal( object = gsn_default_distance( MULTI.GSN ), "stlf" )
  # test gsn_default_distance<-
  gsn_default_distance( MULTI.GSN ) <- "jaccard"
  testthat::expect_equal( object = gsn_default_distance( MULTI.GSN ), "jaccard" )
  # test gsn_default_distance<- with invalid distance.
  testthat::expect_error( object = gsn_default_distance( MULTI.GSN ) <- "invalid" )

  # test gsn_distances
  testthat::expect_contains( object = gsn_distances( MULTI.GSN ), expected = c("stlf", "jaccard", "oc") )

  # test pw_id_col
  testthat::expect_equal( object = pw_id_col( MULTI.GSN ), expected = "ID" )
  # Invalid assignment
  testthat::expect_error( object = pw_id_col( MULTI.GSN ) <- "invalid" )
  # Now do proper assignment:
  MULTI.GSN$pathways$id_col <- NULL
  pw_id_col( MULTI.GSN ) <- "ID"
  testthat::expect_equal( object = MULTI.GSN$pathways$id_col, expected = "ID" )



  # test pw_stat_col
  testthat::expect_in( object = pw_stat_col( MULTI.GSN ),
                       expected = c("P.1S", "adj.P.1S", "P.2S", "adj.P.2S") )
  # Invalid assignment
  testthat::expect_error( object = pw_stat_col( MULTI.GSN ) <- "invalid" )
  # Now do proper assignment:
  MULTI.GSN$pathways$stat_col <- NULL
  pw_stat_col( MULTI.GSN ) <- "adj.P.2S"
  testthat::expect_equal( object = MULTI.GSN$pathways$stat_col, expected = "adj.P.2S" )


  # test pw_stat_col_2
  # Invalid assignment
  testthat::expect_error( object = pw_stat_col_2( MULTI.GSN ) <- "invalid" )
  # Now do proper assignment:
  MULTI.GSN$pathways$stat_col_2 <- NULL
  pw_stat_col_2( MULTI.GSN ) <- "adj.P.1S"
  testthat::expect_equal( object = MULTI.GSN$pathways$stat_col_2, expected = "adj.P.1S" )
  testthat::expect_equal( object = pw_stat_col_2( MULTI.GSN ), expected = "adj.P.1S" )


  # test pw_sig_order
  testthat::expect_equal( object = pw_sig_order( MULTI.GSN ), expected = "loToHi" )
  # Invalid assignment
  testthat::expect_error( object = pw_sig_order( MULTI.GSN ) <- "invalid" )
  # Now do proper assignment:
  MULTI.GSN$pathways$sig_order <- NULL
  pw_sig_order( MULTI.GSN ) <- "hiToLo"
  testthat::expect_equal( object = MULTI.GSN$pathways$sig_order, expected = "hiToLo" )
  pw_sig_order( MULTI.GSN ) <- "loToHi"
  testthat::expect_equal( object = MULTI.GSN$pathways$sig_order, expected = "loToHi" )


  # test pw_sig_order_2
  testthat::expect_equal( object = pw_sig_order_2( MULTI.GSN ), expected = NULL )
  # Invalid assignment
  testthat::expect_error( object = pw_sig_order_2( MULTI.GSN ) <- "invalid" )
  # Now do proper assignment:
  pw_sig_order_2( MULTI.GSN ) <- "loToHi"
  testthat::expect_equal( object = pw_sig_order_2( MULTI.GSN ), expected = "loToHi" )
  testthat::expect_equal( object = MULTI.GSN$pathways$sig_order_2, expected = "loToHi" )


  # test pw_n_col
  MULTI.GSN$pathways$n_col <- "N"
  testthat::expect_equal( object = pw_n_col( MULTI.GSN ), expected = "N" )
  # Invalid assignment
  testthat::expect_error( object = pw_n_col( MULTI.GSN ) <- "invalid" )
  # Now do proper assignment:
  MULTI.GSN$pathways$n_col <- NULL
  pw_n_col( MULTI.GSN ) <- "N"
  testthat::expect_equal( object = MULTI.GSN$pathways$n_col, expected = "N" )


  # test pw_type
  MULTI.GSN$pathways$type <- "gsnora"
  testthat::expect_equal( object = pw_type( MULTI.GSN ), expected = "gsnora" )
  # Assignment:
  MULTI.GSN$pathways$type <- NULL
  pw_type( MULTI.GSN ) <- "gsnora"
  testthat::expect_equal( object = MULTI.GSN$pathways$type, expected = "gsnora" )

  # nzLog10
  in.nzLog10 <- c(0, 1E-70, 1E-65 )
  testthat::expect_warning( out.nzLog10 <- nzLog10( in.nzLog10 ) )
  testthat::expect_equal( object = 10**out.nzLog10, in.nzLog10 + in.nzLog10[2]/2 )
  testthat::expect_error( nzLog10( -1 ) )

  # nzLog10.old # This may be removed soon.
  testthat::expect_warning( out.nzLog10.old <- nzLog10( in.nzLog10 ) )
  testthat::expect_equal( object = 10**out.nzLog10.old, in.nzLog10 + in.nzLog10[2]/2 )
  testthat::expect_error( nzLog10.old( -1 ) )

  # antisplit
  GSC.df <- antiSplit(.l = GSC, col.names = c("ID", "GENE") )
  testthat::expect_equal( object = nrow( GSC.df ), expected = length( unlist( GSC ) ) )
  testthat::expect_equal( object = colnames( GSC.df ), expected = c("ID", "GENE") )
  testthat::expect_setequal( object = unique( GSC.df$ID ), expected = names(GSC) )
  testthat::expect_setequal( object = unique( GSC.df$GENE ), expected = unique(unlist(GSC)) )

  # pick_MappedGeneSymbol
  mgs.from <- c("NHSL1///XXXX///YYYY", "Zzzz///Plag1///AAAA", "BBBB///CCCC///SLU7")
  mgs.to <- unique( unlist( GSC ) )
  mgs.out <- pick_MappedGeneSymbol( .from = mgs.from, .to = mgs.to )
  testthat::expect_equal( mgs.out, c("NHSL1", "PLAG1", "SLU7") )

  # read_gmt
  .gsc <- read_gmt( file = file.path( testthat::test_path(), "testdata", "GSC.gmt" ) )
  testthat::expect_equal( object = .gsc, expected = GSC )

  # write_gmt
  write_gmt.file <- tempfile()
  # gsc List missing names
  .gsc.bad <- structure( GSC, names = NULL )
  testthat::expect_error( object = write_gmt( gsc = .gsc.bad, filename = write_gmt.file ) )
  # gsc not a list of character vectors
  .gsc.bad <- c("Bad", 'data')
  testthat::expect_error( object = write_gmt( gsc = .gsc.bad, filename = write_gmt.file ) )
  # gsc an empty list
  .gsc.bad <- list()
  testthat::expect_error( object = write_gmt( gsc = .gsc.bad, filename = write_gmt.file ) )
  # How about good data?
  testthat::expect_no_error( object = write_gmt( gsc = GSC, filename = write_gmt.file ) )
  testthat::expect_true( file.size( write_gmt.file ) > 0 )

  # tmod2gsc
  # Using vignette sample data for this test.
  Bai_gsc.gsc <- tmod2gsc( Bai_gsc.tmod )
  # is Bai_gsc.gsc a named list of character vectors?
  testthat::expect_type( object = Bai_gsc.gsc, "list" )
  testthat::expect_type( object = Bai_gsc.gsc[[1]], "character" )
  testthat::expect_false( object = is.null( names( Bai_gsc.gsc ) ) )

  # intV2Color
  testthat::expect_equal( object = intV2Color( rgb_v = c(0, 255, 255) ), expected = "#00FFFF" )
  testthat::expect_equal( object = intV2Color( rgb_v = c(255, 0, 0) ), expected = "#FF0000" )
  testthat::expect_equal( object = intV2Color( rgb_v = c(0, 0, 0) ), expected = "#000000" )
  testthat::expect_equal( object = intV2Color( rgb_v = c(255, 255, 255) ), expected = "#FFFFFF" )
  testthat::expect_error( object = intV2Color( rgb_v = c(255, 255, 255, 255) ) )
  testthat::expect_error( object = intV2Color( rgb_v = c("255", "255", "255", "255") ) )
  testthat::expect_error( object = intV2Color( rgb_v = c(255, NA, 255) ) )
  testthat::expect_error( object = intV2Color( rgb_v = c(255, 999, 255) ) )

  # color2IntV
  testthat::expect_equal( object = color2IntV("red"), expected = c(255,0,0) )
  testthat::expect_equal( object = color2IntV("#FF0000"), expected = c(255,0,0) )
})
