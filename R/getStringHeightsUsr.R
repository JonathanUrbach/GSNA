
#' @importFrom graphics strheight
#'
# Calculate the string width in usr coordinate units.
getStringHeightsUsr <- function( strings,
                                 cex = par('cex'),
                                 font_face = par("family"),
                                 method = "auto",              # Also, "strwidth" or "csi"
                                 CSI = par("cin")[2],        # Character Width Inches for cex=1
                                 #height.plt = grDevices::dev.size("in")[2] * (.fig[4] - .fig[3]) * (.plt[4] - .plt[3]),
                                 height.plt = grDevices::dev.size("in")[2] * (par("fig")[4] - par("fig")[3]) * (par("plt")[4] - par("plt")[3]),
                                 .ylim = par( "usr" )[3:4]
){
  sizes <- NULL
  if( is.null( strings ) ){
    warning( "Called sizes on NULL value." )
    return( sizes )
  }
  if( method %in% c("auto", "strwidth" ) ){
    sizes <- try( graphics::strheight( s = as.character( strings ), units = "user", cex = cex, family = font_face ), silent = TRUE )
  }
  if( is.null(sizes) && method == "csi" ||
      ( ( "try-error" %in% class(sizes) ) && method == "auto" )
  ){
    sizes <-  sapply( X = strings, FUN = function(x) ifelse( !is.null(x), cex * CSI * (.ylim[2] - .ylim[1]) / height.plt, 0 ) )
  }
  if( is.null( sizes ) ) stop( " Cannot calculate size of string. " )
  return( sizes )
}

