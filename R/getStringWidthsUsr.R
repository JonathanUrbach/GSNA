
#' @importFrom graphics strheight
#'
# Calculate the string width in usr coordinate units.
getStringWidthsUsr <- function( strings,
                                cex = par('cex'),
                                font_face = par("family"),
                                method = "auto",              # Also, "strwidth" or "cwi"
                                CWI = par("cin")[1],        # Character Width Inches for cex=1
                                width.plt = par( 'pin' )[1],
                                .xlim = par( "usr" )[1:2]
){
  sizes <- NULL
  if( is.null( strings ) ){
    warning( "Called sizes on NULL value." )
    return( sizes )
  }
  if( method %in% c("auto", "strwidth" ) ){
    sizes <- try( graphics::strwidth( s = as.character( strings ), units = "user", cex = cex, family = font_face ), silent = TRUE )
  }
  if( is.null(sizes) && method == "cwi" ||
      ( ( "try-error" %in% class(sizes) ) && method == "auto" )
  ){
    sizes <- nchar( strings ) * cex * CWI * (.xlim[2] - .xlim[1]) / width.plt
  }
  if( is.null( sizes ) ) stop( " Cannot calculate size of string. " )
  return( sizes )
}
