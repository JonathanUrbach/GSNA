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

  # A bug has been discovered that causes incorrect summation of partial p-values, affecting single upper tail
  # and double-tail log Fisher p-values at the very least. (See https://github.com/JonathanUrbach/GSNA/issues/17)
  # Since different platforms produce the error for slightly different values of a, we're going to test a range of
  # numbers for a from 50000 => 51000
  #
  # We're using a tolerance that is somewhat more than the default value of testthat_tolerance() (1.490116e-08).
  #
  # This also takes a long time to run using fisher.test(), so only running 500 iterations.

  {
    .tolerance = 1e-7
    stlf.st <- numeric()
    stlf.st.stats <- numeric()
    stlf.lt <- numeric()
    stlf.lt.stats <- numeric()
    stlf.dt <- numeric()
    stlf.dt.stats <- numeric()
    stlf.p <- numeric()
    stlf.p.stats <- numeric()
    .base <- 50000
    .iterations <- 500
    .count <- 0

    for( a in .base:(.base+.iterations) ){
      .count <- .count + 1
      stlf.st[.count] <- GSNA::lfisher_cpp( a = a , b = 7, c = 10, d = 0, alternative = 1 )
      stlf.st.stats[.count] <- log(stats::fisher.test( x = matrix( data = c(a = a , b = 7, c = 10, d = 0), nrow = 2, ncol = 2 ), alternative = "greater" )$p.value)

      stlf.lt[.count] <- GSNA::lfisher_cpp( a = 0 , b = 7, c = 10, d = a, alternative = 2 )
      stlf.lt.stats[.count] <- log(stats::fisher.test( x = matrix( data = c(a = a , b = 7, c = 10, d = 0), nrow = 2, ncol = 2 ), alternative = "less" )$p.value)

      stlf.dt[.count] <- GSNA::lfisher_cpp( a = a , b = 7, c = 10, d = 0, alternative = 3 )
      stlf.dt.stats[.count] <- log(stats::fisher.test( x = matrix( data = c(a = a , b = 7, c = 10, d = 0), nrow = 2, ncol = 2 ), alternative = "two.sided" )$p.value)

    }

    testthat::expect_equal( object = stlf.st, expected = stlf.st.stats, tolerance = .tolerance )
    if( (.errors.st <- sum( abs( stlf.st - stlf.st.stats ) > .tolerance ) ) > 0 ){
      message("\nIn lfisher_cpp() single upper-tail test, ", .errors.st, " values exceeded error tolerance.\n")
    }

    testthat::expect_equal( object = stlf.lt, expected = stlf.lt.stats, tolerance = .tolerance )
    if( (.errors.lt <- sum( abs( stlf.lt - stlf.lt.stats ) > .tolerance ) ) > 0 ){
      message("\nIn lfisher_cpp() single lower-tail test, ", .errors.lt, " values exceeded error tolerance.\n")
    }

    testthat::expect_equal( object = stlf.dt, expected = stlf.dt.stats, tolerance = .tolerance )
    if( (.errors.dt <- sum( abs( stlf.dt - stlf.dt.stats ) > .tolerance ) ) > 0 ){
      message("\nIn lfisher_cpp() two-tail test, ", .errors.dt, " values exceeded error tolerance.\n")
    }
  }


})
