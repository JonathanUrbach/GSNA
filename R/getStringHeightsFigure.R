
#' @importFrom graphics strheight
#'
getStringHeightsFigure <- function( strings,
                                    cex = par('cex'),
                                    font_face = par("family"),
                                    method = "auto",              # Also, "strwidth" or "csi"
                                    CSI = par("cin")[2],        # Character Width Inches for cex=1
                                    height.fig = par('fin')[2]

){
  sizes <- NULL
  if( is.null( strings ) ){
    warning( "Called sizes on NULL value." )
    return( sizes )
  }
  if( method %in% c("auto", "strwidth" ) ){
    sizes <- try( graphics::strheight( s = as.character( strings ), units = "figure", cex = cex, family = font_face ), silent = TRUE )
  }
  if( is.null(sizes) && method == "csi" ||
      ( ( "try-error" %in% class(sizes) ) && method == "auto" )
  ){
    sizes <-  sapply( X = strings, FUN = function(x) ifelse( !is.null(x), cex * CSI / height.fig, 0 ) )
  }
  if( is.null( sizes ) ) stop( " Cannot calculate size of string. " )
  return( sizes )
}



