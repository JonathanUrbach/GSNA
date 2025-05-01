test_that( 'myColorF works', {
  .expect <- c("#FFFFF0", "#DAFFAB", "#B5FF66", "#91FF22", "#B5916D", "#DA48B6",  "#FF00FF")
  .out <- GSNA:::myColorF(numbers = c(10, 9, 8, 7, 6, 5, 4 ), n = 7, colors = c("magenta", "chartreuse", "ivory"))
  expect_equal( object = .out, expected = .expect )
} )
