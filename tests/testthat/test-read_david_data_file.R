test_that("read_david_data_file works", {
  load_test_data()

  # Use PW.ORA data to make fake david data
  PW.fake_david_chart <- fake_david_chart( ora_data = PW.ORA, .gsc = GSC )
  temp_file.chart <- tempfile()
  temp_file.cluster <- tempfile()
  write_fake_david_chart( fake_david_chart = PW.fake_david_chart, outfile = temp_file.chart )
  write_fake_david_cluster( fake_david_chart = PW.fake_david_chart, outfile = temp_file.cluster )

  # David Functional Annotation Chart Parsing
  test_chart_flat <- read_david_data_file( file = temp_file.chart, output = "flat" )

  # expected fields:
  .expected_fields <- c( "Category", "Term", "Count", "%", "PValue", "Genes",
                         "List Total", "Pop Hits", "Pop Total", "Fold Enrichment",
                         "Bonferroni", "Benjamini", "FDR" )

  # Does the read-in table have the same number of rows as the saved table?
  testthat::expect_equal( object = nrow( test_chart_flat ), expected = nrow( PW.fake_david_chart ) )
  testthat::expect_contains( object = colnames( test_chart_flat ), expected = .expected_fields )

  # output = "hierarchic" should fail.
  testthat::expect_error(object = read_david_data_file( file = temp_file.chart, output = "hierarchic" ) )

  # David Functional Annotation Cluster Parsing
  test_cluster_flat <- read_david_data_file( file = temp_file.cluster, output = "flat" )
  # Does the read-in table have the same number of rows as the saved table?
  testthat::expect_equal( object = nrow( test_cluster_flat ), expected = nrow( PW.fake_david_chart ) )
  # Does the column contain .expected_fields + "Cluster (ES)"?
  testthat::expect_contains( object = colnames( test_cluster_flat ),
                             expected = c(.expected_fields, "Cluster (ES)" ) )
})
