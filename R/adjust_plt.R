#' adjust_plt
#'
#' @details This function adjusts the reserved plot areas legends to the proportions required for rendering.
#'
#' @param .plt The current reserved plot area in the format of the 'plt' argument of \code{\link[graphics]{par}()}.
#' @param y.dim.actual.fu The required y dimension.
#' @param x.dim.actual.fu The required x dimension.
#' @param v.adjust (optional) a string telling the function how to adjust the legend plotting area. Acceptable values
#' are \code{'top'}, \code{'bottom'}, and \code{'middle'}, indicating that the plot area should be
#' adjusted to be flush with the top or bottom of the available plot area, or otherwise centered, respectively.
#' (default: \code{'top'})
#' @param h.adjust (optional) a string telling the function how to adjust the legend plotting area. Acceptable values
#' are \code{'left'}, \code{'right'}, and \code{'center'}, indicating that the plot area should be
#' adjusted to be flush with the left or right edge of the available plot area, or otherwise centered, respectively.
#' (default: \code{'center'})
#' @param v.strict (optional) Boolean value that if \code{TRUE}, tells the function to check that the available virtical
#' space is greater or equal to the \code{y.dim.actual.fu}. If \code{FALSE}, than the adjusted vertical dimension may end
#' up being greater than allocated space. (default: \code{TRUE)}
#' @param h.strict  (optional) Boolean value that if \code{TRUE}, tells the function to check that the available horizontal
#' space is greater or equal to the \code{x.dim.actual.fu}. If \code{FALSE}, than the adjusted horizontal dimension may end
#' up being greater than allocated space. (default: \code{TRUE})
#' @return The returned value is a plot area boundaries in the format of the \code{'par'} value returned by the
#' \code{\link[graphics]{par}()} function.
#'
#' @noRd
#' @keywords internal
adjust_plt <- function( .plt,
                        y.dim.actual.fu,
                        x.dim.actual.fu,
                        v.adjust = "top",
                        h.adjust = "center",
                        v.strict = TRUE,
                        h.strict = TRUE
){
  .plt.adj <- .plt
  if( !is.null( v.adjust ) ){
    # If v.strict, check if y.dim.actual.fu > than space in .plt before adjusting
    if( ! ( v.strict && .plt.adj[4] - .plt.adj[3] < y.dim.actual.fu ) ) {
      if( v.adjust == "top" ){
        .plt.adj[3] <- .plt.adj[4] - y.dim.actual.fu
      } else if( v.adjust == "center" ) {
        y.mid <- (.plt.adj[3] + .plt.adj[4]) / 2
        .plt.adj[3] <- y.mid - y.dim.actual.fu
        .plt.adj[4] <- y.mid + y.dim.actual.fu
      } else if( v.adjust == "bottom" ) {
        .plt.adj[4] <- .plt.adj[3] + y.dim.actual.fu
      } else {
        stop("adjust_plt: Invalid v.adjust argument.")
      }
    }
  }

  if( !is.null( h.adjust  ) ){
    # If h.strict, check if x.dim.actual.fu > than space in .plt before adjusting
    if( ! ( h.strict && .plt.adj[2] - .plt.adj[1] < x.dim.actual.fu ) ) {
      if( h.adjust == "right" ){
        .plt.adj[1] <- .plt.adj[2] - x.dim.actual.fu
      } else if( h.adjust == "center" ) {
        x.mid <- (.plt.adj[1] + .plt.adj[2]) / 2
        .plt.adj[1] <- x.mid - x.dim.actual.fu / 2
        .plt.adj[2] <- x.mid + x.dim.actual.fu / 2
      } else if( h.adjust == "left" ) {
        .plt.adj[2] <- .plt.adj[1] + x.dim.actual.fu
      }else {
        stop("adjust_plt: Invalid h.adjust argument.")
      }
    }
  }
  .plt.adj
}
