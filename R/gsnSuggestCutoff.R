#' gsnSuggestCutoff
#'
#' @description A function that suggests a paring cutoff value for a GSNData object. This value can
#' optionallybe used as the \code{cutoff} argument for \code{\link{gsnPareNetGenericHierarchic}()} or
#' \code{\link{gsnPareNetGenericToNearestNNeighbors}()}.
#'
#' @param object A GSNData object.
#' @param distance (optional) Specifies the distance metric to be used. (detault is the default distance.)
#' @param lower_is_closer (optional) Logical value specifying whether lower values are to be considered
#' closer, within the distance matrix. This should generally be TRUE for statistics like *p*-values,
#' but FALSE for metrics such as Jaccard, OC, or Kappa, for which higher values are closer, i.e. more
#' overlap. (default is the lower_is_closer attribute of the distance matrix, or TRUE if that is not set.)
#' @param quantile (optional) The quantile to be used for specifying a distance cutoff. Defaults to 0.25
#' for lower_is_closer == TRUE and 0.75 for lower_is_closer == FALSE.
#' @param alpha (optional) Equivalent *p*-value cutoff. This can be used when the distance matrix
#' distance_type attribute is "ln_pval". If the distance matrix is a ln-transformed *p*-value and alpha is
#' less than the quantile based cutoff, a message indicating that the corresponding *p*-value for the
#' specified quantile is not significant will be generated, and the quantile based cutoff will be overridden
#' and the specified *p*-value based cutoff will be indicated instead. If not specified or set to \code{NULL},
#' the quantile-based cuttoff will be returned, even if it does not correspond to a significant *p*-value.
#' In that case, a message will still indicate that it is not significant. For similarity metrics such as
#' Jaccard, Szymkiewicz-Simpson OC and Cohen's kappa, a corresponding *p*-value cannot easily be calculated,
#' so this argument is not used.
#' @param extr_dist_quantile_override A logical value allowing the user to override specified quantile if
#' the distance corresponding to that quantile is the highest or lowest value in the data set, thereby
#' avoiding a situation where either clusters are not resolved, or no clusters are identified. If \code{TRUE},
#' the function selects the next highest or lowest distance value. This is an experimental feature.
#' (default FALSE).
#'
#' @return Invisibly returns a numerical vector indicating a suggested cutoff, based on the specified distance,
#' quantile values, and alpha (if appropriate).
#'
#' @details In addition to the returned value, this function prints messages with various information,
#' including the distance matrix being used, whether lower values represent closer gene sets
#' (lower_is_closer, as specified in the function call and in the distance matrix attribute),
#' the specified quantile, and quantile-based cutoff, the equivalent *p*-value, the cutoff based on
#' \code{alpha} (if specified), and the actual suggested cutoff. A message about the significance of
#' the quantile-based cutoff may be included.
#'
#' @export
#'
#' @seealso
#'  \code{\link{gsnDistanceHistogram}}
#'
gsnSuggestCutoff <- function( object,
                              distance = GSNA::gsn_default_distance( object ),
                              lower_is_closer =  (function(x){ ifelse( is.null( x ), TRUE, x ) })(
                                attr( x = object$distances[[distance]]$matrix,
                                      which = "lower_is_closer",
                                      exact = TRUE) ),
                              quantile = ifelse( lower_is_closer, yes = 0.25, no = 0.75 ),
                              alpha = NULL,
                              extr_dist_quantile_override = FALSE
){
  stopifnot("GSNData" %in% class(object))
  if (is.null(distance))
    distance <- object$default_distance
  if (is.null(distance))
    stop("Need distance argument.")
  if (is.null(object$distances[[distance]]))
    stop("Cannot find data for distance ", distance)
  if( length( quantile ) > 1 ) {
    warning( "The quantile argument must be a numeric vector of length 1. Shortening to length 1." )
    quantile <- quantile[1]
  }
  .alpha <- alpha
  if( is.null( .alpha ) ) .alpha <- 0.05

  .qlz <- stats::quantile(x = as.dist(object$distances[[distance]]$matrix),
                          probs = quantile)
  .distance_type <- attr(x = object$distances[[distance]]$matrix,
                         which = "distance_type", exact = TRUE)
  .out.cat <- c("Distance:", distance, "\ndistance_type attribute:",
                .distance_type, "\nlower_is_closer:", lower_is_closer,
                "\nlower_is_closer attribute:", attr(x = object$distances[[distance]]$matrix,
                                                     which = "lower_is_closer", exact = TRUE), "\nquantile:",
                quantile, "\nquantile-based cutoff: ", .qlz)
  if ("ln_pval" %in% .distance_type) {
    .p <- exp(.qlz)
    .out.cat <- c(.out.cat, "\nquantile-based cutoff equivalent p-value:", .p)
    if( is.null( alpha ) ){
      if (any(.p >= .alpha)) {
        .out.cat <- c(.out.cat, "\nNote: cutoff at specified quantile has a corresponding p-value",
                      "\n  greater than ", .alpha, " (cutoff ", log(.alpha), ").", "\n  As a result, gene sets may be very weakly associated if",
                      "\n  this cutoff is used.")
      }
    } else {
      .out.cat <- c(.out.cat, "\nNote: cutoff at specified quantile has a corresponding p-value",
                    "\n  greater than specified ", .alpha, " (cutoff ", log(.alpha), ").", "\n  Using cutoff based on alpha.")
      .qlz <- log( .alpha )
    }
  }
  .dists <- as.dist(object$distances[[distance]]$matrix)
  .dist.range <- range( .dists, na.rm = TRUE )
  if( any( .qlz == .dist.range[[1]] )  ){
    if( extr_dist_quantile_override ){
      #.qlz <- (min( .dists[.dists != .dist.range[1]], na.rm = TRUE ) + .dist.range[1] )/ 2
      .qlz <- min( .dists[.dists != .dist.range[1]], na.rm = TRUE )
      .qtl <- stats::ecdf( .dists )(.qlz)
      .out.cat <- c(.out.cat,
                    "\nNote: cutoff at specified quantile is equal to the minimum",
                    "\n  distance value. Overriding specified quantile.",
                    "\n  Using: ", .qlz, "(quantile=", .qtl, ")"
      )
    } else {
      .out.cat <- c(.out.cat, "\nNote: cutoff at specified quantile is equal to the minimum",
                    "\n  distance value. Subnets may be poorly resolved. To override this",
                    "\n  behavior, use 'extr_dist_quantile_override=TRUE'" )
    }
  }else if( any( .qlz == .dist.range[[2]] )  ){
    if( extr_dist_quantile_override ){
      #.qlz <- (max( .dists[.dists != .dist.range[2]], na.rm = TRUE ) + .dist.range[2] )/ 2
      .qlz <- max( .dists[.dists != .dist.range[2]], na.rm = TRUE )
      .qtl <- stats::ecdf( .dists )(.qlz)
      .out.cat <- c(.out.cat,
                    "\nNote: cutoff at specified quantile is equal to the maximum",
                    "\n  distance value. Overriding specified quantile.",
                    "\n  Using: ", .qlz, "(quantile=", .qtl, ")"
      )
    } else {
      .out.cat <- c(.out.cat, "\nNote: cutoff at specified quantile is equal to the maximum",
                    "\n  distance value. Subnets may be poorly resolved. To override this",
                    "\n  behavior, use 'extr_dist_quantile_override=TRUE'\n" )
    }
  }

  .out.cat <- c( .out.cat, "\nsuggested cutoff: ", .qlz)

  cat(paste0(.out.cat, collapse = ""))
  invisible(.qlz)
}

