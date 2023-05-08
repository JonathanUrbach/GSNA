#' Title
#'
#' @param col
#' @param type
#' @param threshold
#' @param low
#' @param high
#'
#' @return
#' @export
#'
#' @examples
contrasting_color <- function( col, type = "complement", threshold = 127, low = "#000000", high = "#FFFFFF" ){
  rgbArr <- col2rgb( col )
  if( type == "complement" ) { # type == 1
    rgb( t( 255 -  rgbArr ), maxColorValue = 255 )
  } else if( type == "rotate" ){
    rgb( t(( rgbArr + 128 ) %% 255), maxColorValue = 255 )
  } else if( type == "yellow" ){ # For BLUE vs RED
    rgb(t(apply( X = rgbArr,
                 MARGIN = 2,
                 FUN = function(x){
                   #rg <- sapply( max(x[c(1,3)]) - x[2], FUN = function(x){ifelse(x<0, 0, x)} );
                   rg <- sapply( max(c(x[1], x[3] + 64)) - x[2], FUN = function(x){ifelse(x<0, 0, ifelse(x>255, 255, x))} );
                   b <- 0;
                   return(c( rg, rg, b))
                 } )),
        maxColorValue = 255 )
  } else if( type == "gray" ){
    rgb( t(sapply( X = apply( X = rgbArr,MARGIN = 2, FUN = function(x) mean(c(x[1]-20,x[2]-20,x[3] + 40)) )/ 255, FUN = function(x) 1 - c( x, x, x ) )))
  } else if( type == "binary" ){
    apply( X = rgbArr, MARGIN = 2, FUN = function(x)ifelse( mean(x) >= threshold, low, high   ) )
  } else if( type == "blackyellow" ){
    apply( X = rgbArr, MARGIN = 2, FUN = function(x)ifelse( mean(x) >= 180, "#000000", "#FFFF00" ) )
  } else{
    stop( "Type = ", type, " not allowed." )
  }
}
