
invisible( utils::globalVariables( c( "subnetRank" ) ) )

#' gsnSubnetSummary
#'
#' @description Generates a table summarizing subnets that incorporates subnets and pathways data.
#'
#' @param object A GSNData data object containing a distance matrix and subnets data. If pathways
#' data is not specified by the pathways.data argument (described below), the object must contain
#' imported pathways data as well.
#' @param pathways.data An (optional) data.frame containing pathways data (GSEA, CERNO, GSNORA, etc.)
#' with 1 or 2 associated statistical columns, typically *P*-values, specified by stat_col and
#' stat_col_2 below.
#' @param distance A distance metric with associated subnets data.
#' @param id_col (optional) This is the name of the column in the pathways data.frame that corresponds
#' to the names of gene sets. The default value is specified by \code{object$pathways$id_col}.
#' (See details.)
#' @param stat_col (optional) Specifies the name of the first statistical column, if not specified,
#' defefaults to the value in \code{object$pathways$stat_col}.
#' @param sig_order (optional) This indicates the behavior of \code{stat_col}, whether low values
#' (\code{'loToHi'}) or high values (\code{'hiToLo'}) are most significant. The default value is
#' specified in \code{object$pathways$sig_order}.
#' @param stat_col_2 (optional) Specifies the name of the second statistical column, if not specified,
#' defefaults to the value in \code{object$pathways$stat_col_2}.
#' @param sig_order_2 (optional) This indicates the behavior of \code{stat_col_2}, whether low values
#' (\code{'loToHi'}) or high values (\code{'hiToLo'}) are most significant. The default value is
#' specified in \code{object$pathways$sig_order_2}.
#' @param summary_statistics (optional) A character vector specifying which summary statistics are
#' to be calculated from the 'stat_col'. Acceptable values include 'hm' specifying harmonic mean,
#' 'min_max', specifying either minimum or maximum depending on \code{sig_order}, or the name of
#' a function. (default: \code{c('hm', 'min_max')})
#'
#' @return A data.frame with a statistical summary of subnets.
#'
#' @details The output data.frame contains a list of subnets, each with an associated list of gene
#' set IDs. For each subnet, summary statistics are calculated, including the harmonic mean of
#' \code{stat_col} and (if specified) \code{stat_col_2}. In addition, the minimum or maximum of the
#' \code{stat_col} and \code{stat_col_2} is calculated, depending on the \code{sig_order} and
#' \code{sig_order_2}. For \code{loToHi}, the minumum is calculated, and for \code{hiToLo}, the
#' maximum.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'    subnetSummary.df <- gsnSubnetSummary( object = analysis.GSN )
#' }
#'
#' @importFrom psych harmonic.mean
#'
gsnSubnetSummary <- function( object,
                              pathways.data = NULL,
                              distance = NULL,
                              id_col = NULL,
                              stat_col = NULL,
                              sig_order = NULL,
                              stat_col_2 = NULL,
                              sig_order_2 = NULL,
                              summary_statistics = c( "hm", "min_max" )
                              ){
  stopifnot( "GSNData" %in% class( object ) )

  if( is.null( pathways.data ) ) pathways.data <- object$pathways$data
  if( is.null( id_col ) ) id_col <- object$pathways$id_col
  if( is.null( stat_col ) ) stat_col <- object$pathways$stat_col
  if( is.null( sig_order ) ) sig_order <- object$pathways$sig_order
  if( is.null( stat_col_2 ) ) stat_col_2 <- object$pathways$stat_col_2
  if( is.null( sig_order_2 ) ) sig_order_2 <- object$pathways$sig_order_2
  # If stat_col_2 is set, but not sig_order_2, use sig_order
  if( is.null( sig_order_2 ) ) sig_order_2 <- object$pathways$sig_order

  PW.subnets <- gsnMergePathways( object = object, pathways.data = pathways.data, distance = distance, id_col = id_col, stat_col = stat_col, sig_order = sig_order )

  # Aggregate pathways stats:
  SUM.subnets <- data.frame( subnet = unique( PW.subnets$subnet ) )

  if( !is.null( PW.subnets$subnet ) && !is.null( PW.subnets$subnetRank ) && !is.null( PW.subnets$Title ) ){
    #sn.minranks <- with( PW.subnets,  ave( x = subnetRank, subnet, FUN = min ) )
    sn.is.minrank <- with( PW.subnets,  subnetRank == ave( x = subnetRank, subnet, FUN = min ) )
    sn.is.not.dup <- ! with( PW.subnets, duplicated( paste0( subnet, '/', subnetRank ) ) )
    PW.subnets.minrank <- PW.subnets[ sn.is.not.dup & sn.is.minrank, ]
    #y = subset( PW.subnets, subnetRank == 1, select = c("subnet", "Title" ) ),
    SUM.subnets <- merge( x = SUM.subnets,
                          y = PW.subnets.minrank,
                          by = "subnet"
    )
  }

  if( !is.null( PW.subnets$subnet ) && !is.null( PW.subnets$ID ) ) {
    SUM.subnets <- merge( x = SUM.subnets,
                          y = with( PW.subnets, stats::aggregate( x = list(Members=ID), by = list(subnet = subnet) , length )),
                          by = "subnet"
    )
  }

  col_sigs <- list()
  if( !is.null(stat_col) ) col_sigs[[length(col_sigs)+1]] <-c(stat_col, sig_order)
  if( !is.null(stat_col_2) ) col_sigs[[length(col_sigs)+1]] <-c(stat_col_2, sig_order_2)

  for( col_sig in col_sigs ){
    .col <- col_sig[1]
    .sig_ord <- col_sig[2]
    if( ! is.null( .col ) ){
      for( .sum_stat in summary_statistics ){
        x_list <- list()
        if( .sum_stat == "hm" ){
          x_list[[ paste0("Harmonic_Mean_", .col ) ]] <- PW.subnets[[.col]]
          SUM.subnets <- merge( x = SUM.subnets,
                                y = stats::aggregate( x = x_list,
                                                      by = list(subnet = PW.subnets$subnet ),
                                                      FUN = psych::harmonic.mean ),
                                by = "subnet" )
        } else if( .sum_stat == "min_max" ){
          ext_name <- paste0( c( "loToHi" = "min", "hiToLo" = "max" )[[.sig_ord]], "_", .col )
          min_max <- c( "loToHi" = function(x){min(x, na.rm = TRUE)}, "hiToLo" = function(x){max(x, na.rm = TRUE)} )[[.sig_ord]]
          x_list[[ext_name]] <- PW.subnets[[.col]]
          SUM.subnets <- merge( x = SUM.subnets,
                                y = stats::aggregate( x = x_list,
                                                      by = list(subnet = PW.subnets$subnet ),
                                                      FUN = min_max ),
                                by = "subnet" )
        } else {
          # In this case, the .sum_stat is the legit name of a function.
          .fun <- get( .sum_stat )
          if( ! 'function' %in% class( .fun) ) stop( .sum_stat, " is not a function name." )
          ext_name <- paste0( .sum_stat, "_", .col )
          x_list[[ext_name]] <- PW.subnets[[.col]]
          SUM.subnets <- merge( x = SUM.subnets,
                                y = stats::aggregate( x = x_list,
                                                      by = list(subnet = PW.subnets$subnet ),
                                                      FUN = .fun ),
                                by = "subnet" )
        }
      }
    }
  }

  if( ! is.null( id_col ) ){
    # gsnMergePathways currently returns the id_col as ID. (but it may also be present under the name id_col)
    # covering all bases here, but we may not need .id_col and we could maybe just go with 'ID'
    .id_col <- 'ID'
    if( id_col %in% colnames( PW.subnets ) ) .id_col <- id_col
    SUM.subnets <- merge( x = SUM.subnets,
                          y = stats::aggregate( x = list( IDs = PW.subnets[[.id_col]] ),
                                                by = list(subnet = PW.subnets$subnet ),
                                                FUN = function( x ){ paste0(x, collapse = ", ") } ),
                          by = "subnet", check.names = FALSE )
  }

  # Remove Duplicates:
  SUM.subnets <- SUM.subnets[!duplicated(SUM.subnets$subnet),]

  colnames( SUM.subnets ) <- gsub( pattern = "_", replacement = " ", x = colnames( SUM.subnets ) )
  SUM.subnets$subnet <- as.character(SUM.subnets$subnet)
  SUM.subnets <- SUM.subnets[order(as.numeric(as.character(SUM.subnets$subnet))),]
  SUM.subnets
}




gsnSubnetSummary.old <- function( object, pathways.data = NULL, distance = NULL, id_col = NULL, stat_col = NULL, sig_order = NULL, stat_col_2 = NULL, sig_order_2 = NULL  ){
  stopifnot( "GSNData" %in% class( object ) )

  if( is.null( pathways.data ) ) pathways.data <- object$pathways$data
  if( is.null( id_col ) ) id_col <- object$pathways$id_col
  if( is.null( stat_col ) ) stat_col <- object$pathways$stat_col
  if( is.null( sig_order ) ) sig_order <- object$pathways$sig_order
  if( is.null( stat_col_2 ) ) stat_col_2 <- object$pathways$stat_col_2
  if( is.null( sig_order_2 ) ) sig_order_2 <- object$pathways$sig_order_2
  # If stat_col_2 is set, but not sig_order_2, use sig_order
  if( is.null( sig_order_2 ) ) sig_order_2 <- object$pathways$sig_order

  PW.subnets <- gsnMergePathways( object = object, pathways.data = pathways.data, distance = distance, id_col = id_col, stat_col = stat_col, sig_order = sig_order )

  # Aggregate pathways stats:
  SUM.subnets <- data.frame( subnet = unique( PW.subnets$subnet ) )

  if( !is.null( PW.subnets$subnet ) && !is.null( PW.subnets$subnetRank ) && !is.null( PW.subnets$Title ) ){
    #sn.minranks <- with( PW.subnets,  ave( x = subnetRank, subnet, FUN = min ) )
    sn.is.minrank <- with( PW.subnets,  subnetRank == ave( x = subnetRank, subnet, FUN = min ) )
    sn.is.not.dup <- ! with( PW.subnets, duplicated( paste0( subnet, '/', subnetRank ) ) )
    PW.subnets.minrank <- PW.subnets[ sn.is.not.dup & sn.is.minrank, ]
    #y = subset( PW.subnets, subnetRank == 1, select = c("subnet", "Title" ) ),
    SUM.subnets <- merge( x = SUM.subnets,
                          y = PW.subnets.minrank,
                          by = "subnet"
    )
  }

  if( !is.null( PW.subnets$subnet ) && !is.null( PW.subnets$ID ) ) {
    SUM.subnets <- merge( x = SUM.subnets,
                          y = with( PW.subnets, stats::aggregate( x = list(Members=ID), by = list(subnet = subnet) , length )),
                          by = "subnet"
    )
  }

  col_sigs <- list()
  if( !is.null(stat_col) ) col_sigs[[length(col_sigs)+1]] <-c(stat_col, sig_order)
  if( !is.null(stat_col_2) ) col_sigs[[length(col_sigs)+1]] <-c(stat_col_2, sig_order_2)

  for( col_sig in col_sigs ){
    .col <- col_sig[1]
    .sig_ord <- col_sig[2]
    if( ! is.null( .col ) ){
      x_list <- list()
      x_list[[ paste0("Harmonic_Mean_", .col ) ]] <- PW.subnets[[.col]]
      SUM.subnets <- merge( x = SUM.subnets,
                            y = stats::aggregate( x = x_list,
                                                  by = list(subnet = PW.subnets$subnet ),
                                                  FUN = psych::harmonic.mean ),
                            by = "subnet" )
      ext_name <- paste0( c( "loToHi" = "min", "hiToLo" = "max" )[[.sig_ord]], "_", .col )
      min_max <- c( "loToHi" = function(x){min(x, na.rm = TRUE)}, "hiToLo" = function(x){max(x, na.rm = TRUE)} )[[.sig_ord]]
      x_list <- list()
      x_list[[ext_name]] <- PW.subnets[[.col]]
      SUM.subnets <- merge( x = SUM.subnets,
                            y = stats::aggregate( x = x_list,
                                                  by = list(subnet = PW.subnets$subnet ),
                                                  FUN = min_max ),
                            by = "subnet" )
    }
  }

  if( ! is.null( id_col ) ){
    SUM.subnets <- merge( x = SUM.subnets,
                          y = stats::aggregate( x = list( IDs = PW.subnets[[id_col]] ),
                                                by = list(subnet = PW.subnets$subnet ),
                                                FUN = function( x ){ paste0(x, collapse = ", ") } ),
                          by = "subnet", check.names = FALSE )
  }

  # Remove Duplicates:
  SUM.subnets <- SUM.subnets[!duplicated(SUM.subnets$subnet),]

  colnames( SUM.subnets ) <- gsub( pattern = "_", replacement = " ", x = colnames( SUM.subnets ) )
  SUM.subnets$subnet <- as.character(SUM.subnets$subnet)
  SUM.subnets <- SUM.subnets[order(as.numeric(as.character(SUM.subnets$subnet))),]
  SUM.subnets
}
