
#' @importFrom grDevices colorRampPalette
myColorF<-function(numbers, n=100, colors =  c("white","yellow","red")){
  numbers.scale <- round(n*(numbers - min(numbers, na.rm = TRUE))/(max(numbers, na.rm = TRUE)-min(numbers, na.rm = TRUE)))
  colorVec <- grDevices::colorRampPalette( colors )(n+1)
  colorVec[numbers.scale+1]
}
