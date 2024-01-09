test_that("multiplication works", {
  load_test_data()
  # Test the GSNData Constructor
  test.GSNData <- GSNData()

  # Test the print.GSNData method
  {
    test.printfile <- tempfile()
    sink( test.printfile )
    print.GSNData(test.GSNData)
    sink( NULL )
    # Look for expected data in output:
    data.print <- readLines( test.printfile )
    testthat::expect_equal( object = length(data.print), expected = 1 )
    testthat::expect_true( object = stringr::str_detect( pattern = "GSNData object version:", data.print[[1]] ) )
  }
  {
    # Test the print.GSNData method with STLF.GSN
    test.printfile <- tempfile()
    sink( test.printfile )
    print.GSNData(STLF.GSN)
    sink( NULL )
    # Look for expected data in output:
    data.print <- readLines( test.printfile )
    testthat::expect_true( object = stringr::str_detect( pattern = "GSNData object version:", data.print[[1]] ) )
    testthat::expect_true( object = any(stringr::str_detect( pattern = "Contains the following distance\\(s\\)\\:", data.print ) ) )
    testthat::expect_true( object = any(stringr::str_detect( pattern = "Contains\\spathways\\sdata\\sof\\stype:\\s.+", data.print ) ) )
  }

})
