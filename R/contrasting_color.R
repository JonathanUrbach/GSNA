#' contrasting_color
#'
#' @description Function picks colors to contrast with the colors given as the \code{'col'} argument.
#'
#' @param col A character vector of colors.
#' @param type (optional) Type of contrasting color, i.e. the method of generating the contrasting color.
#' Valid values are \code{'complement'}, \code{'rotate'}, \code{'yellow'}, \code{'gray'}, \code{'binary'}
#' and \code{'blackyellow'}.
#' @param threshold (optional, used only for \code{type='binary'}) The "binary" method works by assessing
#' the mean value of the RGB channels. If the value is above a threshold, the low color is returned, if it
#' is below the threshold, the high color is returned.
#' @param low (optional, used only for \code{type='binary'}) Low color (see \code{threshold} argument).
#' @param high (optional, used only for \code{type='binary'}) High color (see \code{threshold} argument).
#'
#' @return A contrasting color.
#' @export
#'
#' @importFrom grDevices rgb
#'
# @examples
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
