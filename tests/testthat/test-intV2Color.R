test_that("intV2Color_works", {
  .input1 <- c(255, 0, 128)
  .expect1 <- "#FF0080"
  .value1 <- intV2Color( .input1 )
  # Is return value type character?
  testthat::expect_type( .value1, "character" )
  # ... and length 1?
  testthat::expect_equal( length( .value1 ), 1 )
  # ... and did we get the correct answer?
  testthat::expect_equal( .value1 , .expect1 )

  # What about with bad data?
  testthat::expect_error( object =  intV2Color( c(255, 0, NA) ), regexp = "Missing data." )
  testthat::expect_error( object =  intV2Color( NULL ), regexp = "Incorrect data type" )
  testthat::expect_error( object =  intV2Color( c("255","0", "80" ) ), regexp = "Incorrect data type" )
  testthat::expect_error( object =  intV2Color( c(255,0, 80, 24 ) ), regexp = "Incorrect data length" )
})
