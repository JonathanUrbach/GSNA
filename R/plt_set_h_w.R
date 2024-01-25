# Raster images require an appropriately proportioned plot area in order to
# plot axes flush with the raster edges. This function, plt_set_h_w
# adjusts plt values to have hight/width == h_w. This is used to adjust plt
# so the plot area of double and single color legend rasters will be plotted
# correctly with axes flush with against the rasters' edge.
plt_set_h_w <- function( plt = graphics::par('plt'),
                         width = grDevices::dev.size("in")[1],
                         height = grDevices::dev.size("in")[2],
                         h_w = 1 ){
  h_w.in <-  (( plt[4] - plt[3] ) / ( plt[2] - plt[1] ) ) * ( height/ width )

  if( h_w.in < h_w ){ # Limited by height, adjust width
    return( c( (plt[2] + plt[1]) / 2 - h_w.in/ h_w  * (plt[2] - plt[1]) / 2,
               (plt[2] + plt[1]) / 2 + h_w.in / h_w  * (plt[2] - plt[1]) / 2,
               plt[3],
               plt[4] ) )
  } else if( h_w.in >= h_w ){ # Limited by width, adjust height
    return( c( plt[1],
               plt[2],
               (plt[4] + plt[3]) / 2 - h_w / h_w.in * (plt[4] - plt[3]) / 2,
               (plt[4] + plt[3]) / 2 + h_w / h_w.in * (plt[4] - plt[3]) / 2 ) )
  }
}
