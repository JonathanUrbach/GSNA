

#' Title
#'
#' @param numbers.1
#' @param numbers.2
#' @param twoColorEncode.fun
#' @param n
#' @param lab.1
#' @param lab.2
#' @param cex.lab
#' @param cex.axis
#' @param axis_lab_ratio
#' @param legend.scale.factor
#' @param legend.ylab
#' @param legend.xlab
#' @param legend.fg
#' @param legend.bg
#' @param log_scale.1
#' @param log_scale.2
#' @param .plt.leg
#' @param .mar.leg.vm
#' @param .fin
#' @param v.adjust
#' @param h.adjust
#' @param draw.legend.box.bool
#' @param optimize.legend.size
#' @param render.bool
#' @param restore.params.bool
#'
#' @return
#' @export
#'
#' @examples
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

                             legend.fg = par('fg'),
                             legend.bg = par('bg'),

                             log_scale.1 = FALSE,
                             log_scale.2 = FALSE,
                             #.plt = c( 0.20, 0.90, 0.20, 0.80 ),
                             .plt.leg =c( 0.71, 1.0, 0.70, 1.0 ),
                             .mar.leg.vm = c( 4.1, 4.1, 2.1, 2.1 ), # Virtual Margins for Legend (Within region set by .plt.leg)
                             .fin = par('fin'),

                             v.adjust = "top",
                             h.adjust = "center",

                             draw.legend.box.bool = FALSE,

                             optimize.legend.size = FALSE,

                             render.bool = TRUE,
                             restore.params.bool = TRUE
){
  legend.stack <- make2ColorLegendStack( numbers.1 = numbers.1,
                                         numbers.2 = numbers.2,
                                         twoColorEncode.fun = twoColorEncode.fun,
                                         n = n )

  numbers.1.domain <- range( numbers.1, na.rm = TRUE )
  numbers.2.domain <- range( numbers.2, na.rm = TRUE )

  # Calculate tic locations for tick_values.1
  if( log_scale.1 ){
    tick_values.1 <- axisTicks( usr = log10( numbers.1.domain ), log = TRUE )
  } else {
    tick_values.1 <- axisTicks( usr = numbers.1.domain, log = FALSE )
  }

  if( log_scale.2 ){
    tick_values.2 <- axisTicks( usr = log10( numbers.2.domain ), log = TRUE )
  } else {
    tick_values.2 <- axisTicks( usr = numbers.2.domain, log = FALSE )
  }

  legend.aspect <- (numbers.2.domain[2] - numbers.2.domain[1])/(numbers.1.domain[2] - numbers.1.domain[1])

  x.dim.actual.fu <- min( .plt.leg[2] - .plt.leg[1], (.plt.leg[4] - .plt.leg[3] ) * (.fin[2] / .fin[1]) )
  y.dim.actual.fu <- x.dim.actual.fu * .fin[1] / .fin[2]

  .plt.adj <- adjust_plt( .plt = .plt.leg,
                          y.dim.actual.fu = y.dim.actual.fu,
                          x.dim.actual.fu = x.dim.actual.fu,
                          v.adjust = v.adjust,
                          h.adjust = h.adjust )

  #browser()

  # If cex is not set, decide
  if( is.null( legend.ylab) ) legend.ylab <- lab.1
  if( is.null( legend.xlab ) ) legend.xlab <- lab.2

  if( is.null( cex.lab ) ){
    cex.lab <- par( 'cex' )
    # Estimate the dimensions of the raster. We want the label to be at most about that size.
    raster.width.est.fu <- ( .plt.adj[2] - .plt.adj[1] ) - par('cin')[2] * cex.lab * (.mar.leg.vm[2] + .mar.leg.vm[1]) / .fin[1]
    raster.height.est.fu <- ( .plt.adj[4] - .plt.adj[3] ) - par('cin')[2] * cex.lab * (.mar.leg.vm[2] + .mar.leg.vm[1]) / .fin[2]

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
  #cex.max <- 1


  # Adjust .plt.adj further by using .mar.leg.vm
  chi <- par( 'cin' )[2]
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
    # First back up original graphical parameters
    .plt.orig <- par( 'plt' )
    .usr.orig <- par( 'usr' )
    .xpd.orig <- par( 'xpd' )
    #.cex.orig <- par( 'cex' )

    if( draw.legend.box.bool ){
      graphics::par( plt = .plt.adj,  xpd = TRUE, new = TRUE )
      box( which = "plot", col = legend.fg, bg = legend.bg )
    }

    graphics::par( plt = .plt.adj.vm,
                   xpd = TRUE,
                   #cex = cex.lab,
                   new = TRUE )

    raster::plotRGB( legend.stack,
                     r = 1,
                     g = 2,
                     b = 3,
                     add = FALSE,
                     axes = FALSE,
                     margins = TRUE,
                     cex = cex.axis,
                     #cex.lab = cex.lab,
                     #cex.axis = cex.axis,
                     bgalpha = 255,
                     asp = legend.aspect )

    .mgp <- c(2, 0.8, 0) * cex.axis
    if( ! is.null( legend.ylab ) ){
      title( ylab = legend.ylab, mgp = .mgp, cex.lab = cex.lab, col.lab = legend.fg )
    }
    if( ! is.null( legend.xlab ) ){
      title( xlab = legend.xlab, mgp = .mgp, cex.lab = cex.lab, col.lab = legend.fg )
    }

    if( ! is.null( tick_values.1 ) ){
      axis( side = 2, at = tick_values.1, labels = tick_values.1, mgp = .mgp, tick = TRUE, cex.axis = cex.axis, col = legend.fg, col.axis = legend.fg, col.ticks = legend.fg )
    }
    if( ! is.null( tick_values.2 ) ){
      axis( side = 1, at = tick_values.2, labels = tick_values.2, mgp = .mgp, tick = TRUE, cex.axis = cex.axis, col = legend.fg, col.axis = legend.fg, col.ticks = legend.fg )
    }

    box( col = legend.fg, bg = legend.bg )

    if( restore.params.bool ){
      par( "plt" = .plt.orig, xpd = .xpd.orig, new = TRUE )
      plot.window( xlim = .usr.orig[1:2], ylim = .usr.orig[3:4], xaxs = "i", yaxs = "i" )
    }
  }

  invisible( list( legend.stack = legend.stack,
                   .plt.adj = .plt.adj,
                   plt.adj.vm = .plt.adj.vm,
                   cex = min(cex.axis, cex.lab, na.rm = TRUE),
                   cex.axis = cex.axis,
                   cex.lab = cex.lab ) )
} # make2ColorLegend
