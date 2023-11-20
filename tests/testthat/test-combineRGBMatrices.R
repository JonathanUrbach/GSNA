test_that("combineRGBMatrices works", {
  c1.mat <- t( matrix( nrow = 3, c( 255, 127, 0,
                                    255, 127, 0,
                                    255, 0,   0,
                                    255, 0,   0,
                                    0,   255, 0

  ) ) )

  c2.mat <-  t( matrix( nrow = 3, c( 0,   128, 255,
                                     0,     0, 255,
                                     0,     0, 255,
                                     0,   128, 255,
                                     127,   0, 127
  ) ) )

  c12.additive.expect <- t( matrix( nrow = 3, c( 255,  255,  255,
                                                 255,  127,  255,
                                                 255,    0,  255,
                                                 255,  128,  255,
                                                 127,  255,  127 ) ) )

  c12.scaled_geomean.expect <- t( matrix( nrow = 3, c( 0, 127,  0,
                                                       0,   0,  0,
                                                       0,   0,  0,
                                                       0,   0,  0,
                                                       0,   0,  0 ) ) )

  c12.euclidean.expect <- t( matrix( nrow = 3, c(  255,  180,  255,
                                                   255,  127,  255,
                                                   255,    0,  255,
                                                   255,  128,  255,
                                                   127,  255,  127 ) ) )

  c12.negative_euclidean.expect <- t( matrix( nrow = 3, c(  0,   75,    0,
                                                            0,    0,    0,
                                                            0,    0,    0,
                                                            0,    0,    0,
                                                            0,    0,    0) ) )

  c12.mean.expect <- t( matrix( nrow = 3, c(  128, 128, 128,
                                              128,  64, 128,
                                              128,   0, 128,
                                              128,  64, 128,
                                               64, 128,  64 ) ) )

  expect_equal( object = combineRGBMatrices( c1.mat = c1.mat, c2.mat = c2.mat, combine_method = "mean" ),
                expected = c12.mean.expect )

  expect_equal( object = combineRGBMatrices( c1.mat = c1.mat, c2.mat = c2.mat, combine_method = "scaled_geomean" ),
                expected = c12.scaled_geomean.expect )

  expect_equal( object = combineRGBMatrices( c1.mat = c1.mat, c2.mat = c2.mat, combine_method = "euclidean" ),
                expected = c12.euclidean.expect )

  expect_equal( object = combineRGBMatrices( c1.mat = c1.mat, c2.mat = c2.mat, combine_method = "negative_euclidean" ),
                expected = c12.negative_euclidean.expect )

  expect_equal( object = combineRGBMatrices( c1.mat = c1.mat, c2.mat = c2.mat, combine_method = "additive" ),
                expected = c12.additive.expect )

})
