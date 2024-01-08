test_that("gsnORAtest works", {
  # Load data files:
  testdata_path <- file.path( testthat::test_path(), "testdata" )
  rdafiles <- list.files( path = testdata_path, pattern = "\\.Rda$", full.names = TRUE )
  for( .f in rdafiles ){ load( .f ) }

  # Test gsnORAtest full results:
  .pw.ora <- gsnORAtest( l = SIM_UP_GENES, bg = BACKGROUND_SET, geneSetCollection = GSC, full = TRUE )
  # Does the gsnORAtest_cpp return a data.frame?
  testthat::expect_s3_class( object = .pw.ora, class = "data.frame" )

  testthat::expect_contains( object = colnames( .pw.ora ),
                             expected = c("ID", "a", "b", "c", "d", "N", "Enrichment",
                                          "P.1S", "P.2S", "adj.P.1S", "adj.P.2S" ) )

  # Test that reference values are equal to calculated values:
  testthat::expect_equal( object = .pw.ora$P.1S, expected = PW.ORA$P.1S )
  testthat::expect_equal( object = .pw.ora$P.2S, expected = PW.ORA$P.2S )
  testthat::expect_equal( object = .pw.ora$adj.P.1S, expected = PW.ORA$adj.P.1S )
  testthat::expect_equal( object = .pw.ora$adj.P.2S, expected = PW.ORA$adj.P.2S )
  testthat::expect_equal( object = .pw.ora$N, expected = PW.ORA$N )
  testthat::expect_equal( object = .pw.ora$Enrichment, expected = PW.ORA$Enrichment )
  testthat::expect_equal( object = .pw.ora$a, expected = PW.ORA$a )
  testthat::expect_equal( object = .pw.ora$b, expected = PW.ORA$b )
  testthat::expect_equal( object = .pw.ora$c, expected = PW.ORA$c )
  testthat::expect_equal( object = .pw.ora$d, expected = PW.ORA$d )

  # Test with other parameters, full = FALSE
  testthat::expect_no_error( gsnORAtest( l = SIM_UP_GENES, bg = BACKGROUND_SET, geneSetCollection = GSC, full = FALSE ))
  testthat::expect_error( gsnORAtest( l = c(1,2,3,4,5,6,7,8), bg = BACKGROUND_SET, geneSetCollection = GSC, full = FALSE ))
  testthat::expect_error( gsnORAtest( l = SIM_UP_GENES, bg = c(1,2,3,4,5,6,7,8), geneSetCollection = GSC, full = FALSE ))

  # Try with tmod object
  testthat::expect_no_error( .pw.tmod <- gsnORAtest( l = SIM_UP_GENES, bg = BACKGROUND_SET, geneSetCollection = GSC.tmod, full = FALSE ))
  testthat::expect_false( object = is.null( .pw.tmod$Title ) )
  # With tmod object, Title should be "character"
  testthat::expect_equal( object = class( .pw.tmod$Title ), expected = "character" )

  # Try with tmodGS object
  testthat::expect_no_error(  .pw.tmodGS <- gsnORAtest( l = SIM_UP_GENES, bg = BACKGROUND_SET, geneSetCollection = GSC.tmodGS, full = FALSE ))
  testthat::expect_false( object = is.null( .pw.tmodGS$Title ) )
  testthat::expect_equal( object = class( .pw.tmodGS$Title ), expected = "character" )


})
