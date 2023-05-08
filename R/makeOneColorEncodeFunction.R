

makeOneColorEncodeFunction <- function( numbers,
                                        colors = c("#FFFFFF", "#FFFF00", "#FF0000"),
                                        c.fun = NULL,
                                        na.color = "#CCCCCC"
){
  force( na.color )
  force( colors )

  force( c.fun )


  if( is.null( c.fun ) && ( is.null( colors ) ) )
    stop( "If c.fun is not set, then colors must be." )

  if( is.null( c.fun ) ) c.fun <- makeLinearNColorGradientFunction( colors = colors,
                                                                    x.min = min( numbers, na.rm = TRUE ),
                                                                    x.max = max( numbers, na.rm = TRUE ) )

  function( numbers,
            numbers.2, # Ignored
            output_as = "vector" # or "rgb"
  ){
    c1.mat <- matrix( data = NA, nrow = length( numbers ), ncol = 3 )

    for( i in 1:length(numbers) ){
      c1.mat[i,] <- c.fun( x = numbers[[i]], channel = 1:3 )
    }

    if( output_as == 'array' ){
      return( c1.mat )
    } else { # output_as == 'rgb'
      return( apply(X = c1.mat, MARGIN = 1, FUN = function(x){if(any(is.na(x))) return(na.color); intV2Color( unlist(x) )} ) )
    }
  }
}
