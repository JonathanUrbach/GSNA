#' makeLinearNColorGradientFunction
#'
#' @description Given a set of colors and a range of values, generate a function to encode numbers in
#' the specified range of colors. This serves as a backend for \code{makeOneColorEncodeFunction()}
#' \code{makeTwoColorEncodeFunction()} allowing more than 2 color intervals can be specified, so
#' that color encodings consisting of 3 or more colors per color-dimension/channel can be created.
#'
#' @param colors A vector of colors, either by name or as hexadecimal colors.
#' @param x.min The minimal value for the range of numbers to be encoded.
#' @param x.max The maximal value for the range of numbers to be encoded.
#'
#' @return Returns a function encoding a single numerical value as a numerical vector of length 3 containing
#' RGB values from 0 to 255.
#'
#' @details Given n colors, where n >=1 and a range of numbers from x.min to x.max, the function breaks
#' down the range of numbers into n-1 ranges, and then maps numerical values linearly to numbers in each
#' range bounded by successive colors. This is used by the functions \code{\link{makeOneColorEncodeFunction}()}
#' and \code{\link{makeTwoColorEncodeFunction}()}.
#'
#' @examples
#'
#' three_col_fun <- makeLinearNColorGradientFunction( colors = c("blue", "white", "red"),
#'                                                     x.min = 0,
#'                                                      x.max = 100 )
#'
#' three_col_mat <- t( sapply( c(0, 25, 50, 75, 100 ) ,three_col_fun ) )
#'
#' @seealso
#'  \code{\link{makeOneColorEncodeFunction}()},
#'  \code{\link{makeTwoColorEncodeFunction}()}.
#'
#' @export
#'
makeLinearNColorGradientFunction <- function( colors = c("#000000",
                                                         "#CCCC00",
                                                         "#FF0000"), # Minimal and maximal color
                                              x.min = 0,             # Number of scaled colors
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
