
invisible( utils::globalVariables( c("font_face") ) )

#' make2ColorLegend
#'
#' @description
#' This function generates a 2-color legend for network plots and dendrograms, consisting of a
#' square raster with x&y scales and labels.
#'
#' @param numbers.1 Numbers to set the range of colors for the y-axis.
#' @param numbers.2 Numbers to set the range of colors for the x-axis.
#' @param twoColorEncode.fun A function that takes two numeric values and returns an encoded RGB color value.
#' @param n (optional) The number of color gradations per channel to include in the legend (default 100)
#' @param lab.1 (optional) y-axis label (default: NULL)
#' @param lab.2 (optional) x-axis label (default: NULL)
#' @param cex.lab (optional) Font cex size for labels. If unspecified, then the function will attempt to pick
#' an appropriate value.
#' @param cex.axis (optional) Font cex size for axes. If unspecified, then the function will attempt to pick
#' an appropriate value.
#' @param axis_lab_ratio (optional) If cex.lab and cex.axis are unspecified, the function will attempt to pick
#' appropriate values. This argument is the ratio of axis marks to axis labels (default 0.9).
#' @param legend.scale.factor (optional) A fudge factor for scaling the legend (default 1.15).
#' @param legend.ylab (optional) The y label of the legend.
#' @param legend.xlab (optional) The x label of the legend.
#' @param legend.fg (optional) The forground color of the legend (by default inherited from \code{graphics::par('fg')}).
#' @param legend.bg (optional) The background color of the legend (by default inherited from \code{graphics::par('bg')}).
#' @param log_scale.1 (optional) Indicates whether the y-values are log scale or not. (default: FALSE)
#' @param log_scale.2 (optional) Indicates whether the x-values are log scale or not. (default: FALSE)
#' @param .plt.leg A vector of 4 coordinates indicating the region where the legend is to be ploted.
#' @param .mar.leg.vm (optional) A vector of 4 coordinates indicating legend margins. These values are picked
#' automatically depending on the available geometry, so in general, you won't want to change
#' this.
#' @param .fin (optional) Figure width and height of the figure in inches (defaults to \code{graphics::par('fin')}).
#' @param v.adjust (optional) When the size of the legend is optimized for the available space, indicates
#' whether the legend should be adjusted towards the top, bottom, or middle of the available space.
#' (default: \code{'top'})
#' @param h.adjust (optional) When the size of the legend is optimized for the available space, indicates
#' whether the legend should be adjusted towards the left, right or center of the available space. (default"
#' \code{'center'})
#' @param draw.legend.box.bool (optional) Boolean indicated whether a box should be drawn around the legend.
#' (default: \code{FALSE})
#' @param optimize.legend.size (optional) Boolean indicated whether the function should attempt to optimize
#' the size of the legend. (default: \code{FALSE})
#' @param render.bool (optional) Boolean indicating whether the legend should be rendered, or just return graphical
#' parameters. (default: \code{TRUE})
#' @param restore.params.bool (optional) Boolean indicating whether graphical parameters should be restored to
#' original values once the legend is drawn. (default: \code{TRUE})
#'
#' @return Invisible list of graphical parameters.
#'
#'
#' @importFrom grDevices axisTicks
#' @importFrom graphics axis box par plot.window title
#'
#' @noRd
#' @keywords internal
make2ColorLegend <- function(numbers.1,
                             numbers.2,
                             twoColorEncode.fun,
                             n = 100,

                             lab.1 = NULL,
                             lab.2 = NULL,

                             cex.lab = NULL,
                             cex.axis = NULL,
                             axis_lab_ratio = 0.9,
                             legend.scale.factor = 1.15,

                             legend.ylab = NULL,
                             legend.xlab = NULL,

                             legend.fg = graphics::par('fg'),
                             legend.bg = graphics::par('bg'),

                             log_scale.1 = FALSE,
                             log_scale.2 = FALSE,

                             .plt.leg =c( 0.71, 1.0, 0.70, 1.0 ),

                             .mar.leg.vm = adj_mar_leg_vm(.mar.leg.vm = c( 4.1, 4.1, 2.1, 2.1 ) ),
                             .fin = graphics::par('fin'),

                             v.adjust = "top",
                             h.adjust = "center",

                             draw.legend.box.bool = FALSE,

                             optimize.legend.size = FALSE,

                             render.bool = TRUE
                             #restore.params.bool = TRUE
){
  # Backup par, so that original settings are restored on exit:
  .par.orig <- par( no.readonly = TRUE )
  on.exit( add = TRUE, expr = suppressMessages( suppressWarnings( par(.par.orig) ) ) )

  # Make 2 color legend matrix
  legend.mat <- make2ColorLegendRGBMatrix( numbers.1 = numbers.1,
                                           numbers.2 = numbers.2,
                                           twoColorEncode.fun = twoColorEncode.fun,
                                           n = n )

  numbers.1.domain <- range( numbers.1, na.rm = TRUE )
  numbers.2.domain <- range( numbers.2, na.rm = TRUE )

  # Calculate tic locations for tick_values.1
  # if( log_scale.1 ){
  #   tick_values.1 <- axisTicks( usr = log10( numbers.1.domain ), log = TRUE )
  # } else {
  #   tick_values.1 <- axisTicks( usr = numbers.1.domain, log = FALSE )
  # }
  tick_values.1 <- axisTicks( usr = ifelse( log_scale.1, log10, identity )( numbers.1.domain ), log = log_scale.1 )
  tick_values_at.1 <- tick_values_to_at( tick_values = tick_values.1, numbers.domain = numbers.1.domain, log_scale = log_scale.1 )

  # Calculate tic locations for tick_values.2
  # if( log_scale.2 ){
  #   tick_values.2 <- axisTicks( usr = log10( numbers.2.domain ), log = TRUE )
  # } else {
  #   tick_values.2 <- axisTicks( usr = numbers.2.domain, log = FALSE )
  # }
  tick_values.2 <- axisTicks( usr = ifelse( log_scale.2, log10, identity )( numbers.2.domain ), log = log_scale.2 )
  tick_values_at.2 <- tick_values_to_at( tick_values = tick_values.2, numbers.domain = numbers.2.domain, log_scale = log_scale.2 )

  legend.aspect <- (numbers.2.domain[2] - numbers.2.domain[1])/(numbers.1.domain[2] - numbers.1.domain[1])

  x.dim.actual.fu <- min( .plt.leg[2] - .plt.leg[1], (.plt.leg[4] - .plt.leg[3] ) * (.fin[2] / .fin[1]) )
  y.dim.actual.fu <- x.dim.actual.fu * .fin[1] / .fin[2]

  .plt.adj <- adjust_plt( .plt = .plt.leg,
                          y.dim.actual.fu = y.dim.actual.fu,
                          x.dim.actual.fu = x.dim.actual.fu,
                          v.adjust = v.adjust,
                          h.adjust = h.adjust )

  # If cex is not set, decide
  if( is.null( legend.ylab) ) legend.ylab <- lab.1
  if( is.null( legend.xlab ) ) legend.xlab <- lab.2

  if( is.null( cex.lab ) ){
    cex.lab <- graphics::par( 'cex' )
    # Estimate the dimensions of the raster. We want the label to be at most about that size.
    raster.width.est.fu <- ( .plt.adj[2] - .plt.adj[1] ) - graphics::par('cin')[2] * cex.lab * (.mar.leg.vm[2] + .mar.leg.vm[1]) / .fin[1]
    raster.height.est.fu <- ( .plt.adj[4] - .plt.adj[3] ) - graphics::par('cin')[2] * cex.lab * (.mar.leg.vm[2] + .mar.leg.vm[1]) / .fin[2]

    if( ! is.null( legend.xlab ) ){
      legend.xlab.width.fu <- getStringWidthsFigure( strings = legend.xlab, cex = cex.lab, font_face = font_face )
      if( legend.xlab.width.fu > x.dim.actual.fu * legend.scale.factor ){
        cex.lab <- cex.lab * x.dim.actual.fu * legend.scale.factor / legend.xlab.width.fu
      }
    }
    if( ! is.null( legend.ylab ) ){
      legend.ylab.width.fu <- getStringWidthsFigure( strings = legend.ylab, cex = cex.lab, font_face = font_face ) * .fin[1] / .fin[2]
      if( legend.ylab.width.fu > y.dim.actual.fu * legend.scale.factor ){
        cex.lab <- cex.lab * y.dim.actual.fu * legend.scale.factor / legend.ylab.width.fu
      }
    }
  }
  if( is.null( cex.axis ) ){
    cex.axis <- cex.lab * axis_lab_ratio
  }

  cex.max <- max( cex.axis, cex.lab, na.rm = TRUE )

  # Adjust .plt.adj further by using .mar.leg.vm
  chi <- graphics::par( 'cin' )[2]
  # We need to fit the raster into a square plot space, centered on the center of the region defined by :
  .plt.adj.vm  <- c( .plt.adj[1] + chi * cex.max * .mar.leg.vm[2] / .fin[1],
                     .plt.adj[2] - chi * cex.max * .mar.leg.vm[4] / .fin[1],
                     .plt.adj[3] + chi * cex.max * .mar.leg.vm[1] / .fin[2],
                     .plt.adj[4] - chi * cex.max * .mar.leg.vm[3] / .fin[2] )
  if( .fin[1] * ( .plt.adj.vm[2] - .plt.adj.vm[1] ) > .fin[2] * (.plt.adj.vm[4] - .plt.adj.vm[3] ) ){
    x.mid <- ( .plt.adj.vm[2] + .plt.adj.vm[1] ) / 2
    x.dim.new  <-  (.plt.adj.vm[4] - .plt.adj.vm[3] ) * .fin[2] / .fin[1]
    .plt.adj.vm[1] <- x.mid - x.dim.new / 2
    .plt.adj.vm[2] <- x.mid + x.dim.new / 2
  } else {
    y.mid <- ( .plt.adj.vm[4] + .plt.adj.vm[3] ) / 2
    y.dim.new  <-  (.plt.adj.vm[2] - .plt.adj.vm[1] ) * .fin[1] / .fin[2]
    .plt.adj.vm[3] <- y.mid - y.dim.new / 2
    .plt.adj.vm[4] <- y.mid + y.dim.new / 2
  }

  if( optimize.legend.size ){
    # Re optimize .plt.adj:
    .plt.adj  <- c( .plt.adj.vm[1] - chi * cex.max * .mar.leg.vm[2] / .fin[1],
                    .plt.adj.vm[2] + chi * cex.max * .mar.leg.vm[4] / .fin[1],
                    .plt.adj.vm[3] - chi * cex.max * .mar.leg.vm[1] / .fin[2],
                    .plt.adj.vm[4] + chi * cex.max * .mar.leg.vm[3] / .fin[2] )
  }


  if( render.bool ){
    # First back up original graphical parameters # May not be necessary due to on.exit call.
    .usr.orig <- graphics::par( 'usr' )

    if( draw.legend.box.bool ){
      graphics::par( plt = .plt.adj,  xpd = TRUE, new = TRUE ) # This will be restored by on.exit call.
      graphics::box( which = "plot", col = legend.fg, bg = legend.bg )
    }

    graphics::par( plt = .plt.adj.vm, # This will be restored by on.exit call.
                   xpd = TRUE,
                   #cex = cex.lab,
                   new = TRUE )

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
                   lwd = 0.00 )

    .mgp <- c(2, 0.8, 0) * cex.axis
    if( ! is.null( legend.ylab ) ){
      title( ylab = legend.ylab, mgp = .mgp, cex.lab = cex.lab, col.lab = legend.fg )
    }
    if( ! is.null( legend.xlab ) ){
      title( xlab = legend.xlab, mgp = .mgp, cex.lab = cex.lab, col.lab = legend.fg )
    }

    if( ! is.null( tick_values.1 ) ){
      axis( side = 2, at = tick_values_at.1, labels = tick_values.1, mgp = .mgp, tick = TRUE, cex.axis = cex.axis, col = legend.fg, col.axis = legend.fg, col.ticks = legend.fg )
    }
    if( ! is.null( tick_values.2 ) ){
      axis( side = 1, at = tick_values_at.2, labels = tick_values.2, mgp = .mgp, tick = TRUE, cex.axis = cex.axis, col = legend.fg, col.axis = legend.fg, col.ticks = legend.fg )
    }

    graphics::box( col = legend.fg, bg = legend.bg )

    #if( restore.params.bool ){
    plot.window( xlim = .usr.orig[1:2], ylim = .usr.orig[3:4], xaxs = "i", yaxs = "i" )
    #}
  }

  invisible( list( legend.mat = legend.mat,
                   .plt.adj = .plt.adj,
                   plt.adj.vm = .plt.adj.vm,
                   cex = min(cex.axis, cex.lab, na.rm = TRUE),
                   cex.axis = cex.axis,
                   cex.lab = cex.lab ) )
} # make2ColorLegend



#' make2ColorLegendMatrix
#'
#' @details Generates an RGB color matrix for a 2 color legend.
#'
#' @param numbers.1 A set of numbers to define the range of channel 1 numerical values to be represented in
#' the legend. Only the extreme min and max values are necessary.
#' @param numbers.2 A set of numbers to define the range of channel 2 numerical values to be represented in
#' the legend. Only the extreme min and max values are necessary.
#' @param twoColorEncode.fun A function to take two sets of numbers and return a vector of colors (e.g. "#FF0099").
#' @param n The number of of color gradations per channel to render. The total number of gradations to render
#' is the square of this number.
#'
#' @return An 2-color RGB color matrix correspoding to a legend raster.
#'
#' @noRd
#' @keywords internal
make2ColorLegendRGBMatrix <- function( numbers.1,
                                       numbers.2,
                                       twoColorEncode.fun,
                                       n = 100
){
  numbers.1.domain <- range( numbers.1, na.rm = TRUE )
  numbers.2.domain <- range( numbers.2, na.rm = TRUE )

  ln1.mat <- matrix( nrow = n+1, ncol = n+1 )
  ln2.mat <- matrix( nrow = n+1, ncol = n+1 )

  # Both (linear) axes are divided into n+1 equal segments corresponding to particular values from the domains:
  seq1.scale <- seq( from = numbers.1.domain[1], to = numbers.1.domain[2], length.out = n + 1 )
  seq2.scale <- seq( from = numbers.2.domain[1], to = numbers.2.domain[2], length.out = n + 1 )

  for( i in 1:length(seq1.scale) ){
    ln1.mat[,i] <- seq1.scale[i]
    ln2.mat[i,] <- seq2.scale[i]
  }


  # Linearize ln1.mat and ln2.mat and apply twoColorEncode.fun
  lc12.rgb <- twoColorEncode.fun( numbers.1 = as.vector( ln1.mat ), numbers.2 = as.vector( ln2.mat ), output_as = "rgb" )

  rgb.mat <- matrix( lc12.rgb, nrow = n + 1, ncol = n + 1 )

  rgb.mat

}



# Function tick_values_to_at
#
# Description: Takes tick values and a domain of numbers corresponding to a single channel of an experiment,
# and returns the corresponding positions for tick labels. These positions may be used as the "at" argument
# for the axis() function.
#
# Arguments:
#
# tick_values: a numeric vector of desired tick values, such as is generated by the axisTicks() function.
#
# numbers.domain: a range of numbers corresponding to the minimal and maximal values of values for a legend.
#
# log_scale: tells the function if the values are to be mapped as log or linear scale.
#
tick_values_to_at <- function( tick_values, numbers.domain, log_scale = FALSE ){
  numbers.domain <- range( numbers.domain, na.rm = TRUE )
  if( log_scale ){
    return( log10(tick_values / numbers.domain[1] ) / log10( numbers.domain[2] / numbers.domain[1] ) )
  } else {
    return( ( tick_values - numbers.domain[1]) / ( numbers.domain[2] - numbers.domain[1] ) )
  }
}






