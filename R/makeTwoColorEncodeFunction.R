

#' Title
#'
#' @param numbers.1
#' @param numbers.2
#' @param colors.1
#' @param colors.2
#' @param combine_method
#' @param c1.fun
#' @param c2.fun
#' @param na.color
#'
#' @return
#' @export
#'
#' @examples
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

  if( is.null( c1.fun ) && ( is.null( colors.1 ) || is.null( n ) ) )
    stop( "If c1.fun is not set, then colors.1 and n must be." )

  if( is.null( c2.fun ) && ( is.null( colors.2 ) || is.null( n ) ) )
    stop( "If c2.fun is not set, then colors.2 and n must be." )

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

    if( output_as == 'array' ){
      return( c12.mat )
    } else { # output_as == 'rgb'
      return( apply(X = c12.mat, MARGIN = 1, FUN = function(x){if(any(is.na(x))) return(na.color); intV2Color( unlist(x) )} ) )
    }
  }
}
