


#' GSNData
#'
#' @description GSNData object constructor, used to generate new GSNData objects.
#'
#' @param distances Optional parameter containing a list of module-module distance metric data organized by the name of the
#'                  distance metric used, e.g. lf, jaccard, stlf.
#'
#' @param ... Additional arguments. Object fields can be set as named arguments this way using name = value pairs.
#'
#' @return A new GSNAData object.
#'
#' @details
#'
#' This method is called by \code{buildGeneSetNetworkLFFFast()}, \code{buildGeneSetNetworkLFFast()} and \code{buildGeneSetNetworkSTLF()}.
#' For most users there will be little reason to call this method except when tying to implement support for new distance metrics or
#' utility functions.
#'
#'
#' # Structure of the GSNData object:
#'
#'  The GSNData object can contain multiple distance matrices including log Fisher (lf) and Jaccard (jaccard). These distances,
#'  along with associated pared-distances, and significance order parameters are stored in named sublists within the \code{$distances}
#'  lists, the sublists are named after their respective distance metric (lf, jaccard, etc.) as \code{$distances[[DIST]]}. These sublists
#'  contain a distance matrix \code{$distances[[DIST]]$matrix}, a significance order \code{$distances[[DIST]]$optimal_extreme} (e.g. "loToHi" for lf,
#'  and "hiToLo" for jaccard), and after paring a \code{$distances[[DIST]]$pared}.
#'
#' # Fields:
#'
#' \describe{
#'   \item{\code{$GSNA_version}}{A character vector of length 1 indicating the version of GSNA used to generate this GSNData object.}
#'   \item{\code{$genePresenceAbsence}}{A sparse logical Matrix containing presence(TRUE) or absence (FALSE) calls for genes (rows) in gene sets (columns).}
#'   \item{\code{$distances}}{a named list(). Names indicate a distance metric 'lf', 'jaccard', etc. indicated as \code{DIST} below.}
#'   \item{\code{$distances[[DIST]]$matrix}}{A matrix of raw distances}
#'   \item{\code{$distances[[DIST]]$optimal_extreme}}{ Significance order where "min" indicates that low values are optimal/
#'                                closer than high values as with log Fisher (lf), and "max" indicates that high
#'                                values are closer, as with Jaccard (jaccard) distance.}
#'   \item{\code{$distances[[DIST]]$pared_optimal_extreme}}{ Significance order for the pared, scaled distance matrix.
#'                                This may differ from \code{$distances[[DIST]]$optimal_extreme} if scaling flips high
#'                                distance values to low ones, as may be necessary for handling distance matrices such
#'                                as the Jaccard for which higher values are closer. (See
#'                                \code{$distances[[DIST]]$optimal_extreme}, above.)}
#'   \item{\code{$distances[[DIST]]$pared}}{A pared distance matrix.}
#'   \item{\code{$distances[[DIST]]$edges}}{A data.frame containing a gathered set of network edges derived from \code{$distances[[DIST]]$pared}}
#'   \item{\code{$distances[[DIST]]$vertices}}{A complete list of gene set IDs in the network.}
#'   \item{\code{$default_distance}}{The default distance used for network construction.}
#'   \item{\code{$ordered_genes}}{A character vector containing the ordered list of genes in the data set (most important first).
#'                                This list is also used as the background of observable genes for creating the
#'                                filteredGeneSetCollection.}
#'   \item{\code{$filteredGeneSetCollection}}{A filtered set of gene lists (a list of character vectors) containing only the genes
#'                                present in the differential expression data set. This is the 'background' of all genes
#'                                observable in the differential expression data.}
#'   \item{\code{$pathways}}{A named list containing pathways results data, as follows:}
#'   \item{\code{$pathways$data}}{A data.frame containing a pathways results set.}
#'   \item{\code{$pathways$type}}{A character vector of length=1 indicating the type of pathways analysis performed, e.g. CERNO, GSEA, ORA.}
#'   \item{\code{$pathways$id_col}}{Indicates the name of the column in $pathways$data that contains the gene set ID.}
#'   \item{\code{$pathways$stat_col}}{A character vector of length 1 indicating the statistic used for assessing significance,
#'                                generally a p-value.}
#'   \item{\code{$pathways$stat_col_2}}{A character vector of length 1 indicating the statistic used for assessing significance,
#'                                generally a p-value.}
#'   \item{\code{$pathways$sig_order}}{Indicates whether low of high values of $pathways$statistic are most significant with
#'                                "loToHi" indicating that low values are optimal/most significant (as with typical p-values)
#'                                and "hiToLo" indicating high values are optimal/most significant.}
#'   \item{\code{$pathways$sig_order_2}}{Indicates whether low of high values of $pathways$statistic are most significant with
#'                                "loToHi" indicating that low values are optimal/most significant (as with typical p-values)
#'                                and "hiToLo" indicating high values are optimal/most significant.}
#'   \item{\code{$pathways$n_col}}{Indicates the name of the pathways column used to indicate effective gene set size, based on
#'                                genes actually observable in an experimental data set.}
#'}
#'
#' @export
#'
#' @examples
#'  library(GSNA)
#'  gsn_obj <- GSNData()
#'
#' @importFrom utils packageVersion
#'
GSNData <- function( distances = list(), ... )
  structure(list(distances = distances, GSNA_version = utils::packageVersion( 'GSNA' ),...), class="GSNData")




####

#' print.GSNData
#'
#' @description Print a short description of a \code{GSNData} object.
#'
#' @param x A GSNData object.
#' @param ... Additional parameters currently ignored, but included for consistency with generic print.
#'
#' @return Invisibly returns the GSNData object.
#'
#' @export
#' @exportS3Method print GSNData
print.GSNData <- function( x, ... ){
  cat( "GSNData object version:", unlist( as.character( x$GSNA_version ) ), "\n" )

  if( !is.null( x$genePresenceAbsence ) ){
    cat( "  Contains data for:\n" )
    cat( "    ", nrow( x$genePresenceAbsence ), "genes.\n" )
    cat( "    ", ncol( x$genePresenceAbsence ), "gene sets.\n" )
  }

  if( ! is.null( distz <- names(x$distances) ) ){
    cat( "  Contains the following distance(s):\n" )
    for( .dist in distz ){
      cat( paste0( "     ", .dist, "\n" ) )
    }
  }
  if( ! is.null( distz <- names(x$pathways) ) ){
    .type <- x$pathways$type
    if( is.null( .type ) ) .type <- "NULL"
    cat( "  Contains pathways data of type: ", .type , "\n" )
    for( .datname in c( "id_col", "stat_col", "sig_order", "stat_col_2", "sig_order_2", "n_col" ) )
      if( !is.null(x$pathways[[.datname]] )  ){
        cat( "    ",  .datname, "=", x$pathways[[.datname]], "\n" )
      }
  }
  return( invisible( x ) )
}


