#' @importFrom graphics strheight
#'
# String width in figure coords (0 -> 1)
getStringWidthsFigure <- function( strings,
                                   cex = par('cex'),
                                   font_face = par("family"),
                                   method = "auto",            # Also, "strwidth" or "cwi"
                                   CWI = par("cin")[1],        # Character Width Inches for cex=1
                                   width.fig = par( 'fin' )[1]

){
  sizes <- NULL
  if( is.null( strings ) ){
    warning( "Called sizes on NULL value." )
    return( sizes )
  }
  if( method %in% c("auto", "strwidth" ) ){
    sizes <- try( graphics::strwidth( s = as.character( strings ), units = "figure", cex = cex, family = font_face ), silent = TRUE )
  }
  if( is.null(sizes) && method == "cwi" ||
      ( ( "try-error" %in% class(sizes) ) && method == "auto" )
  ){
    sizes <- nchar( strings ) * cex * CWI / width.fig
  }
  if( is.null( sizes ) ) stop( " Cannot calculate size of string. " )
  return( sizes )
}
