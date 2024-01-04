test_that("lse works", {
  # Log sum of exponents:
  A <- 1E-40
  B <- 3E-41
  LSE.logAlogB <- lse( log(A), log(B) )
  testthat::expect_equal( object = LSE.logAlogB, expected = log( A + B ) )

  # Very small numbers:
  # 10E-400 doesn't work in R, so there's no way to add such small numbers except as logs.
  c <- -400
  LSE.2c <- lse( c, c )
  # If we take the exponent of the difference, we should get 2, since we doubled a
  testthat::expect_equal( object = exp(LSE.2c - c), expected = 2 )

  # Works for large numbers
  d <- 800
  LSE.2d <- lse( d, d )
  # If we take the exponent of the difference, we should get 2, since we doubled a
  testthat::expect_equal( object = exp(LSE.2d - d), expected = 2 )

  # Works for midrannge numbers
  E <- 5
  LSE.2logE <- lse( E, E )
  # If we take the exponent of the difference, we should get 2, since we doubled a
  testthat::expect_equal( object = exp(LSE.2logE - E), expected = 2 )
})
