test_that("gsnImportCERNO works", {
  # Start with test data:
  load_test_data()

  # Setup Run CERNO
  fake_gene_list <- unique( c( unlist( GSC ), BACKGROUND_SET ) )
  GSC.tmod <- gsc2tmod( GSC )
  fake.cerno <- tmod::tmodCERNOtest( l = fake_gene_list, qval = 1.1, mset = GSC.tmod )

  .stlf.GSN <- STLF.GSN
  .stlf.GSN$pathways <- NULL
  testthat::expect_no_error( .stlf.GSN <- gsnImportCERNO( .stlf.GSN, pathways = fake.cerno ) )
  testthat::expect_equal( object = .stlf.GSN$pathways$type, expected = "cerno" )

  .stlf.GSN$pathways <- NULL
  testthat::expect_no_error( .stlf.GSN <- gsnImportCERNO( .stlf.GSN, pathways = fake.cerno, id_col = "ID", stat_col = "P.Value", sig_order = "loToHi" ) )
  testthat::expect_equal( object = .stlf.GSN$pathways$id_col, expected = "ID" )
  testthat::expect_equal( object = .stlf.GSN$pathways$stat_col, expected = "P.Value" )
  testthat::expect_equal( object = .stlf.GSN$pathways$sig_order, expected = "loToHi" )
  testthat::expect_error( .stlf.GSN <- gsnImportCERNO( .stlf.GSN, pathways = fake.cerno, id_col = "INVALID" ) )
  testthat::expect_error( .stlf.GSN <- gsnImportCERNO( .stlf.GSN, pathways = fake.cerno, stat_col = "INVALID" ) )
  testthat::expect_error( .stlf.GSN <- gsnImportCERNO( .stlf.GSN, pathways = fake.cerno, sig_order = "INVALID" ) )
})
