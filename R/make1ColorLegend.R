
invisible( utils::globalVariables( c("font_face") ) )

#' make1ColorLegend
#'
#' @param numbers  Numbers to set the range of colors.
#' @param oneColorEncode.fun A function that takes a numeric value and returns an encoded RGB color value.
#' @param n (optional) The number of color gradations to include in the legend (default 100).
#' @param lab (optional) The axis label. Since the color scale is vertical, this is a y-axis label.(default: NULL)
#' @param log_scale Boolean value indicating whether the scale should be log scale. (default: FALSE)
#' @param .plt.leg A vector of 4 coordinates indicating the region where the legend is to be ploted.
#' @param .mar.leg.vm A vector of 4 coordinates indicating legend margins. These values are picked
#' automatically depending on the available geometry, so in general, you won't want to change
#' this.
#' @param .fin (optional) Figure width and height of the figure in inches (defaults to \code{par('fin')}).
#' @param h_w.leg (optional) Height to width ratio of the 1-color legend panel. (default: 1)
#' @param legend_thickness (optional) The width of the legend in raster matrix cells. (default: approximately 15% of height)
#' @param legend.lab (optional) The legend label. (default \code{''})
#' @param cex.lab (optional) Font cex size for labels. If unspecified, then the function will attempt to pick
#' an appropriate value.
#' @param cex.axis (optional) Font cex size for axes. If unspecified, then the function will attempt to pick
#' an appropriate value.
#' @param axis_lab_ratio (optional) If cex.lab and cex.axis are unspecified, the function will attempt to pick
#' appropriate values. This argument is the ratio of axis marks to axis labels (default 0.9).
#' @param legend.fg (optional) The forground color of the legend (by default inherited from \code{par('fg')}).
#' @param legend.bg (optional) The background color of the legend (by default inherited from \code{par('bg')}).
#' @param draw.legend.box.bool (optional) Boolean indicated whether a box should be drawn around the legend.
#' (default: \code{FALSE})
#' @param v.adjust (optional) When the size of the legend is optimized for the available space, indicates
#' whether the legend should be adjusted towards the top, bottom, or middle of the available space.
#' (default: \code{'top'})
#' @param h.adjust (optional) When the size of the legend is optimized for the available space, indicates
#' whether the legend should be adjusted towards the left, right or center of the available space. (default"
#' \code{'center'})
#' @param render.bool (optional) Boolean indicating whether the legend should be rendered, or just return graphical
#' parameters. (default: \code{TRUE})
#' @param restore.params.bool (optional) Boolean indicating whether graphical parameters should be restored to
#' original values once the legend is drawn. (default: \code{TRUE})
#' @param optimize.legend.size (optional) Boolean indicated whether the function should attempt to optimize
#' the size of the legend. (default: \code{FALSE})
#'
#' @return Invisible list of graphical parameters.
#'
#' @importFrom grDevices axisTicks
#' @importFrom graphics axis box par plot.window title
#' @importFrom Matrix image
#'
#' @noRd
#' @keywords internal
make1ColorLegend <- function(numbers,
                                 oneColorEncode.fun,
                                 n = 100,
                                 lab = NULL,

                                 log_scale = FALSE,

                                 .plt.leg = c( 0.71, 1.0, 0.70, 1.0 ),
                                 #.mar.leg.vm = c( 1.1, 4.1, 1.1, 1.1 ), # Virtual Margins for Legend (Within region set by .plt.leg)
                                 .mar.leg.vm = adj_mar_leg_vm(.mar.leg.vm = c( 1.1, 4.1, 1.1, 1.1 ) ),
                                 .fin = graphics::par('fin'),

                                 h_w.leg = 1, # Height to width ratio of the 1-color legend panel.

                                 legend_thickness = NULL,
                                 legend.lab = "",

                                 cex.lab = NULL,
                                 cex.axis = NULL,
                                 axis_lab_ratio = 0.9,

                                 legend.fg = graphics::par('fg'),
                                 legend.bg = graphics::par('bg'),
                                 draw.legend.box.bool = FALSE,

                                 v.adjust = 'top',
                                 h.adjust = 'center',

                                 render.bool = TRUE,
                                 restore.params.bool = TRUE,
                                 optimize.legend.size = FALSE

){
  # Backup par, so that original settings are restored on exit:
  .par.orig <- par( no.readonly = TRUE )
  on.exit( add = TRUE, expr = suppressMessages( suppressWarnings( par(.par.orig) ) ) )

  # Optimize width so that it's approximately 15% of height.
  if( is.null( legend_thickness ) ) legend_thickness <- ceiling( n * 15 / 100 )

  # legend.stack <- make1ColorLegendStack( numbers = numbers,
  #                                        oneColorEncode.fun = oneColorEncode.fun,
  #                                        n = n,
  #                                        legend_thickness = legend_thickness )
  legend.mat <- make1ColorLegendRGBMatrix( numbers = numbers,
                                           oneColorEncode.fun = oneColorEncode.fun,
                                           n = n,
                                           legend_thickness = legend_thickness )
  numbers.domain <- range( numbers, na.rm = TRUE )

  # Calculate tic locations for tick_values
  # if( log_scale ){
  #   tick_values <- axisTicks( usr = log10( numbers.domain ), log = TRUE )
  # } else {
  #   tick_values <- axisTicks( usr = numbers.domain, log = FALSE )
  # }
  tick_values <- axisTicks( usr = ifelse( log_scale, log10, identity )( numbers.domain ), log = log_scale )
  tick_values_at <- tick_values_to_at( tick_values = tick_values, numbers.domain = numbers.domain, log_scale = log_scale )

  # Optimize geometry of .plt.leg
  x.dim.actual.in <- min( ( .plt.leg[2] - .plt.leg[1] ) * .fin[1],
                          ( .plt.leg[4] - .plt.leg[3] ) * .fin[2] / h_w.leg,
                          na.rm = TRUE )
  y.dim.actual.in <- x.dim.actual.in * h_w.leg

  x.dim.actual.fu <- x.dim.actual.in / .fin[1]
  y.dim.actual.fu <- y.dim.actual.in / .fin[2]

  # Adjust .plt.adj further
  chi <- graphics::par( 'cin' )[2]

  if( optimize.legend.size ){
    .plt.adj <- adjust_plt( .plt = .plt.leg,
                            y.dim.actual.fu = y.dim.actual.fu,
                            x.dim.actual.fu = x.dim.actual.fu,
                            v.adjust = "top",
                            h.adjust = "center",
                            v.strict = TRUE,
                            h.strict = TRUE )
  } else {
    .plt.adj <- .plt.leg
  }

  # If cex is not set, decide
  if( is.null( cex.lab ) ){
    cex.lab <- graphics::par( 'cex' )
    # Estimate the dimensions of the raster. We want the label to be at most about that size.
    raster.height.est.fu <- ( .plt.adj[4] - .plt.adj[3] ) - graphics::par('cin')[2] * cex.lab * (.mar.leg.vm[2] + .mar.leg.vm[1]) / .fin[2]

    if( ! is.null( legend.lab ) ){
      legend.lab.width.fu <- getStringWidthsFigure( strings = legend.lab, cex = cex.lab, font_face = font_face ) * .fin[1] / .fin[2]
      if( legend.lab.width.fu > raster.height.est.fu ){
        cex.lab <- cex.lab * raster.height.est.fu / legend.lab.width.fu
      }
    }
  }
  if( is.null( cex.axis ) ){
    cex.axis <- cex.lab * axis_lab_ratio
  }
  cex.max <- max( cex.axis, cex.lab, na.rm = TRUE )

  # Calculate .plt.adj.vm for the raster.
  .plt.adj.vm <- plt_set_h_w( plt = c( .plt.adj[1] + chi * cex.lab  * .mar.leg.vm[2] / .fin[1],
                                       .plt.adj[2] - chi * cex.lab  * .mar.leg.vm[4] / .fin[1],
                                       .plt.adj[3] + chi * cex.lab  * .mar.leg.vm[1] / .fin[2],
                                       .plt.adj[4] - chi * cex.lab  * .mar.leg.vm[3] / .fin[2] ),
                              h_w = (n+1)/legend_thickness )

  if( optimize.legend.size ){
    # Re optimize .plt.adj:
    .plt.adj  <- c( .plt.adj.vm[1] - chi * cex.lab * .mar.leg.vm[2] / .fin[1],
                    .plt.adj.vm[2] + chi * cex.lab * .mar.leg.vm[4] / .fin[1],
                    .plt.adj.vm[3] - chi * cex.lab * .mar.leg.vm[1] / .fin[2],
                    .plt.adj.vm[4] + chi * cex.lab * .mar.leg.vm[3] / .fin[2] )
  }

  if( render.bool ){
    # First back up original graphical parameters. # May not be necessary due to on.exit call.
    #.plt.orig <- graphics::par( 'plt' )
    .usr.orig <- graphics::par( 'usr' )
    #.xpd.orig <- graphics::par( 'xpd' )

    if( draw.legend.box.bool ){
      graphics::par( plt = .plt.leg,  xpd = TRUE, new = TRUE ) # This will be restored by on.exit call.
      graphics::box( which = "plot", col = legend.fg, bg = legend.bg )
    }

    graphics::par( plt = .plt.adj.vm, xpd = TRUE,
                   new = TRUE ) # This will be restored by on.exit call.

    .xlim <- c( 0, 1 )
    .ylim <- c( 0, 1 )

    legend.factor <- factor( as.vector( legend.mat ) )
    legend.n <- matrix( as.numeric( legend.factor ), nrow = nrow( legend.mat ), ncol = ncol( legend.mat ) )
    legend.colors <- levels( legend.factor)


    Matrix::image( z = legend.n,
                   xlim = .xlim,
                   ylim = .ylim,
                   col = legend.colors,
                   add = FALSE,
                   axes = FALSE,
                   cex = cex.axis,
                   useRaster = FALSE,
                   lwd = 0.00 ) #,
    #border.col = NA )

    if( ! is.null( lab ) ){
      graphics::title( ylab = lab, mgp=c(2,1,1), cex.lab = cex.lab, col.lab = legend.fg )
    }

    if( ! is.null( tick_values ) ){
      graphics::axis( side = 2, at = tick_values_at, labels = tick_values , tick = TRUE, cex.axis = cex.axis, col = legend.fg, col.axis = legend.fg, col.ticks = legend.fg  )
    }

    graphics::box( col = legend.fg, bg = legend.bg )


    if( restore.params.bool ){
      #graphics::par( "plt" = .plt.orig, xpd = .xpd.orig, new = TRUE )
      graphics::plot.window( xlim = .usr.orig[1:2], ylim = .usr.orig[3:4], xaxs = "i", yaxs = "i" )
    }
  }

  # Return graphical parameters.
  invisible( list( legend.mat = legend.mat,
                   .plt.adj = .plt.adj,
                   plt.adj.vm = .plt.adj.vm,
                   cex = min(cex.axis, cex.lab, na.rm = TRUE),
                   cex.axis = cex.axis,
                   cex.lab = cex.lab  ) )
} # make1ColorLegend


#' make1ColorLegendMatrix
#'
#' @details Generates an RGB color matrix for a 1 color legend.
#'
#' @param numbers A set of numbers to define the range of numerical values to be represented in
#' the legend. Only the extreme min and max values are necessary.
#' @param oneColorEncode.fun A function to take a set of numbers and return a vector of colors,  (e.g. "#FF7700").
#' @param n The number of of color gradations to render.
#'
#' @return An 1-color RGB color matrix correspoding to a legend raster.
#'
#' @noRd
#' @keywords internal
make1ColorLegendRGBMatrix <- function( numbers,
                                       oneColorEncode.fun,
                                       n = 100,
                                       legend_thickness = 15
){
  numbers.domain <- range( numbers, na.rm = TRUE )

  #ln1.mat <- matrix( nrow = n+1, ncol = legend_thickness )
  ln1.mat <- matrix( ncol = n+1, nrow = legend_thickness )


  # (Linear) Axis is divided into n+1 equal segments corresponding to particular values from the domains:
  seq1.scale <- seq( from = numbers.domain[1], to = numbers.domain[2], length.out = n + 1 )

  # Coordinates are plotted as x, y
  y.cells <- length(seq1.scale)
  for( i in 1:y.cells ){
    #ln1.mat[i,] <- seq1.scale[i]
    #ln1.mat[,y.cells + 1 - i] <- seq1.scale[i]
    ln1.mat[,i] <- seq1.scale[i]
  }

  # Linearize ln1.mat and ln2.mat and apply oneColorEncode.fun
  lc12.rgb <- oneColorEncode.fun( numbers = as.vector( ln1.mat ), output_as = "rgb" )
  #browser()
  rgb.mat <- matrix( lc12.rgb, ncol = n + 1, nrow = legend_thickness )

  rgb.mat
}

