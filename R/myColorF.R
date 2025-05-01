

# This function is used to assign a set of colors to a set of supplied numerical values. It's only used in
# one place in gsnPlotNetwork to assign edge colors, and I might remove it eventually.
#
#' @importFrom grDevices colorRampPalette
myColorF<-function(numbers, n=100, colors =  c("white","yellow","red")){
  numbers.scale <- round(n*(numbers - min(numbers, na.rm = TRUE))/(max(numbers, na.rm = TRUE)-min(numbers, na.rm = TRUE)))
  colorVec <- grDevices::colorRampPalette( colors )(n+1)
  colorVec[numbers.scale+1]
}
