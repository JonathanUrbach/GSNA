

#' makeTwoColorEncodeFunction
#'
#' @description Generate a function to take two numerical vector arguments and return a color, either as a vector of
#' hexadecimal encoded colors, or as a three column matrix.
#'
#' @param numbers.1 A set of numbers to define the range of channel 1 numerical values for which the color encode
#' function will be defined. Only the extreme min and max values are necessary.
#' @param numbers.2 A set of numbers to define the range of channel 2 numerical values for which the color encode
#' function will be defined. Only the extreme min and max values are necessary.
#' @param colors.1 The range of channel 1 colors to be returned by the function function. (default: c("#FFFFFF", "#FF0000"))
#' @param colors.2 The range of channel 2 colors to be returned by the function function. (default: c("#FFFFFF", "#0000FF"))
#' @param combine_method (optional) For dual channel plots this is a string used to indicate how colors are combined to
#' generate a two dimensional color scale. Options are "scaled_geomean" (same as "default"), "standard" (same as "euclidean" ),
#' "negative_euclidean", "mean", and "additive". See details.
#' @param c1.fun (optional) A function to convert the numerical in channel 1 into colors. If not specified, this is
#' generated based on \code{numbers.1} and \code{colors.1}.
#' @param c2.fun (optional) A function to convert the numerical in channel 2 into colors. If not specified, this is
#' generated based on \code{numbers.2} and \code{colors.2}.
#' @param na.color (optional) The color returned from the function for NA values (default: "#CCCCCC").
#'
#' @return \code{makeTwoColorEncodeFunction()} returns a function that takes 3 arguments and returns either a vector of
#' hexadecimal colors or a 3-column matrix of columns. The arguments:
#' \item{\code{numbers.1}}{A vector of numbers for channel 1, to be encoded as a color value.}
#' \item{\code{numbers.2}}{A vector of numbers for channel 2, to be encoded as a color value.}
#' \item{\code{output_as}}{Specifies the type of return value. If \code{'vector'} or \code{'rgb'}, the function returns a vector
#' of hexadecimal colors (e.g."#FFCCAA"), if 'matrix','array', a three column numeric matrix is returned (Columns are "R", "G", or "B").
#' Currently, \code{'vector'} are synonyms \code{'rgb'}, as are \code{'matrix'} and \code{'array'}}.
#'
#' @export
#'
#' @examples
#'
#' # Prepare the function:
#' twoColorEnc.fun <- makeTwoColorEncodeFunction( numbers.1 = c( 0.4, 6 ),
#'                                                numbers.2 = c(0.6, 20),
#'                                                colors.1 = c("white", "red"),
#'                                                colors.2 = c("white", "green" ),
#'                                                combine_method = "mean" )
#' # Encode two vectors of numbers as a single vector of colors:
#' colors_as_vector <- twoColorEnc.fun( numbers.1 = c( 0.4, 1.2, 5, 6 ),
#'                                      numbers.2 = c( 0.6, 6, 9, 20 ),
#'                                      output_as = 'vector' )
#'
makeTwoColorEncodeFunction <- function( numbers.1,
                                        numbers.2,
                                        colors.1 = c("#FFFFFF", "#FF0000"),
                                        colors.2 = c("#FFFFFF", "#0000FF"),
                                        combine_method = "mean", #"default"
                                        c1.fun = NULL,
                                        c2.fun = NULL,
                                        na.color = "#CCCCCC"
){
  force( na.color )
  force( colors.1 )
  force( colors.2 )
  force( c1.fun )
  force( c2.fun )
  force( combine_method )

  if( is.null( c1.fun ) && ( is.null( colors.1 ) ) )
    stop( "If c1.fun is not set, then colors.1 must be." )

  if( is.null( c2.fun ) && ( is.null( colors.2 ) ) )
    stop( "If c2.fun is not set, then colors.2 must be." )

  if( is.null( c1.fun ) ) c1.fun <- makeLinearNColorGradientFunction( colors = colors.1,
                                                                      x.min = min( numbers.1, na.rm = TRUE ),
                                                                      x.max = max( numbers.1, na.rm = TRUE ) )

  if( is.null( c2.fun ) ) c2.fun <- makeLinearNColorGradientFunction( colors = colors.2,
                                                                      x.min = min( numbers.2, na.rm = TRUE ),
                                                                      x.max = max( numbers.2, na.rm = TRUE ) )

  function( numbers.1,
            numbers.2,
            output_as = "vector" # or "rgb"
  ){
    if( length( numbers.1 ) != length( numbers.2 ) ){
      stop( "numbers.1 and numbers.2 have different numbers of elements." )
    }

    c1.mat <- matrix( data = NA, nrow = length( numbers.1 ), ncol = 3 )
    c2.mat <- matrix( data = NA, nrow = length( numbers.2 ), ncol = 3 )

    for( i in 1:length(numbers.1) ){
      c1.mat[i,] <- c1.fun( x = numbers.1[[i]], channel = 1:3 )
      c2.mat[i,] <- c2.fun( x = numbers.2[[i]], channel = 1:3 )
    }

    c12.mat <- combineRGBMatrices( c1.mat, c2.mat, combine_method )

    if( output_as %in% c('matrix', 'array') ){
      return( c12.mat )
    } else if( output_as %in% c('vector', 'rgb')) { # output_as == 'rgb'
      return( apply(X = c12.mat, MARGIN = 1, FUN = function(x){if(any(is.na(x))) return(na.color); intV2Color( unlist(x) )} ) )
    } else {
      stop( "Invalid value: output_as='", output_as, "'" )
    }
  }
}
