#' adj_mar_leg_vm
#'
#' @description
#' This utility function scales the virtual legend margin for figures width smaller than threshold inches (defaults to 5
#' inches) or height (defaults to 2.5 inches). To scale, the the function determines whether the width and height fall
#' below their respective thresholds. If the ratios are less than 1, then the margins are scaled to whichever has the
#' smaller ratio.
#'
#' This is a hack, since we should be scaling, not to the total size of the figure, but to the size available
#' for the legend itself.
#'
#' @param .mar.leg.vm Input virtual legend margins.
#' @param width (optional) Figure width. (defaults to \code{par('fin')[1]} )
#' @param height (optional) Figure height. (defaults to \code{par('fin')[2]} )
#' @param width.threshold (optional) The width threshold for scaling. Below this threshold, scaling is performed.
#' @param height.threshold (optional) The height below threshold for scaling. Below this threshold, scaling is
#' performed.
#'
#' @return A new .mar.leg.vm that is scaled for the current plot legend.
#'
#' @details The default behavior of this function is to scale margins to the total size of the figure, but it should actually be
#' scaling the margins to the size of the legend itself.
#'
#' examples
#'
#' .mar.leg.vm <- adj_mar_leg_vm( .mar.leg.vm = c( 4.1, 4.1, 2.1, 2.1 ),
#'                                 width = 1.5,
#'                                 height = 1.5,
#'                                 width.threshold = 1.8,
#'                                 height.threshold = 1.8 )
#'
#' @noRd
#' @keywords internal
adj_mar_leg_vm <- function( .mar.leg.vm, width = par('fin')[1], height = par('fin')[2], width.threshold = 5, height.threshold = 2.5 ){
  scale_ratio <- min( width/width.threshold, height/height.threshold )
  if( scale_ratio < 1 ){
    .mar.leg.vm <- .mar.leg.vm * scale_ratio
  }
  .mar.leg.vm
}


