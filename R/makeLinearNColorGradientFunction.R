#' Title
#'
#' @param colors
#' @param x.min
#' @param x.max
#'
#' @return
#' @export
#'
#' @examples
makeLinearNColorGradientFunction <- function( colors = c("#000000",
                                                         "#CCCC00",
                                                         "#FF0000"), # Minimal and maximal color
                                              x.min = 0,              # Number of scaled colors
                                              x.max = 100
){
  colors_count <- length( colors )
  force( x.min )
  force( x.max )

  if( colors_count < 2 ) stop("Need two or more colors to create a gradient function.")

  # Count the colors
  sub_gradients_range_size <- ( x.max - x.min ) / (colors_count - 1 )
  x.i <- x.min
  x_min <- numeric()
  x_max <- numeric()
  # Color Intervals are 1 less than colors_count
  c_min <- matrix( ncol = 3, nrow = colors_count - 1 )
  c_max <- matrix( ncol = 3, nrow = colors_count - 1 )

  for( i in 1:(colors_count - 1) ){
    x_min[[i]] <- x.i
    x_max[[i]] <- x.i + sub_gradients_range_size
    c_min[i,] <- color2IntV( colors[ i ] )
    c_max[i,] <- color2IntV( colors[ i+1 ] )
    x.i <- x.i + sub_gradients_range_size
  }

  function( x, channel ){
    if( is.na( x ) ) return( c(NA, NA, NA)[channel] )
    if( x <= x_min[[1]] ){
      return( c_min[1, channel] )
    }
    for( j in 1:(colors_count - 1 ) ){
      if( x <= x_max[[j]] )
        return( round( c_min[j, channel] + (x - x_min[[j]]) * (c_max[j, channel] - c_min[j, channel]) / (x_max[[j]] - x_min[[j]]) ) )
    }
    return( c_max[nrow(c_max), channel] )
  }
}
