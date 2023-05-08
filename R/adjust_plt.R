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
