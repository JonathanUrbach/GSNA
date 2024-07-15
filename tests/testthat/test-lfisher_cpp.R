test_that("lfisher_cpp works", {
  # Load data files:
  testdata_path <- file.path( testthat::test_path(), "testdata" )
  rdafiles <- list.files( path = testdata_path, pattern = "\\.Rda$", full.names = TRUE )
  for( .f in rdafiles ){ load( .f ) }

  # Overrepresentation ("greater"):
  .data.mat <- matrix( c(20, 0, 0,  20), nrow = 2, ncol = 2 )
  .out.fisher <- stats::fisher.test( .data.mat, alternative = "greater" )
  .out.lfisher_cpp <- lfisher_cpp( .data.mat[1], .data.mat[2], .data.mat[3], .data.mat[4], alternative = 1 )
  testthat::expect_equal( object = .out.lfisher_cpp, expected = log(.out.fisher$p.value ) )

  # Overrepresentation ("less"):
  .data.mat2 <- matrix( c(0, 20, 20, 0), nrow = 2, ncol = 2 )
  .out.fisher2 <- stats::fisher.test( .data.mat2, alternative = "less" )
  .out.lfisher_cpp2 <- lfisher_cpp( .data.mat2[1], .data.mat2[2], .data.mat2[3], .data.mat2[4], alternative = 2 )
  testthat::expect_equal( object = .out.lfisher_cpp2, expected = log(.out.fisher2$p.value ) )

  # Two sided
  .out.fisher3 <- stats::fisher.test( .data.mat, alternative = "two.sided" )
  .out.lfisher_cpp3 <- lfisher_cpp( .data.mat[1], .data.mat[2], .data.mat[3], .data.mat[4], alternative = 3 )
  testthat::expect_equal( object = .out.lfisher_cpp3, expected = log(.out.fisher3$p.value ) )

  # Partial
  .lf.partial <- function( a,b,c,d ){
    log(factorial(a+b)) + log(factorial(c+d)) + log(factorial(a+c)) + log(factorial(b+d)) -
    log(factorial(a)) - log(factorial(b)) - log(factorial(c)) - log(factorial(d)) - log(factorial(a+b+c+d))
  }
  .out.lf.partial <- .lf.partial( .data.mat[1], .data.mat[2], .data.mat[3], .data.mat[4] )
  .out.lfisher_cpp4 <- lfisher_cpp( .data.mat[1], .data.mat[2], .data.mat[3], .data.mat[4], alternative = 4 )
  testthat::expect_equal( object = .out.lfisher_cpp4, .out.lf.partial )
})
