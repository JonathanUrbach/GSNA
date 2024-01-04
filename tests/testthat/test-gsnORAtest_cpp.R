test_that("gsnORAtest_cpp works", {
  # Load data files:
  testdata_path <- file.path( testthat::test_path(), "testdata" )
  rdafiles <- list.files( path = testdata_path, pattern = "\\.Rda$", full.names = TRUE )
  for( .f in rdafiles ){ load( .f ) }

  .pw.ora <- gsnORAtest_cpp( l = SIM_UP_GENES, bg = BACKGROUND_SET, geneSetCollection = GSC )

  # Does the gsnORAtest_cpp return a data.frame?
  testthat::expect_s3_class( object = .pw.ora, class = "data.frame" )

  testthat::expect_contains( object = colnames( .pw.ora ),
                             expected = c("ID", "a", "b", "c", "d", "N", "Enrichment", "P.1S", "P.2S" ) )

  # Compare against PW.ORA, reference data generated on a seemingly working installation.
  # Reorder data in PW.ORA
  rownames(PW.ORA) <- PW.ORA$ID
  PW.ORA <- PW.ORA[.pw.ora$ID,]

  # Test that reference values are equal to calculated values:
  testthat::expect_equal( object = .pw.ora$P.1S, expected = PW.ORA$P.1S )
  testthat::expect_equal( object = .pw.ora$P.2S, expected = PW.ORA$P.2S )
  testthat::expect_equal( object = .pw.ora$a, expected = PW.ORA$a )
  testthat::expect_equal( object = .pw.ora$b, expected = PW.ORA$b )
  testthat::expect_equal( object = .pw.ora$c, expected = PW.ORA$c )
  testthat::expect_equal( object = .pw.ora$d, expected = PW.ORA$d )
})
