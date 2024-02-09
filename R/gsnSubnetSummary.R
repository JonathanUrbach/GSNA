
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
#' defaults to the value in \code{object$pathways$stat_col}.
#' @param sig_order (optional) This indicates the behavior of \code{stat_col}, whether low values
#' (\code{'loToHi'}) or high values (\code{'hiToLo'}) are most significant. The default value is
#' specified in \code{object$pathways$sig_order}.
#' @param stat_col_2 (optional) Specifies the name of the second statistical column, if not specified,
#' defaults to the value in \code{object$pathways$stat_col_2}.
#' @param sig_order_2 (optional) This indicates the behavior of \code{stat_col_2}, whether low values
#' (\code{'loToHi'}) or high values (\code{'hiToLo'}) are most significant. The default value is
#' specified in \code{object$pathways$sig_order_2}.
#' @param summary_statistics (optional) A character vector specifying which summary statistics are
#' to be calculated from the 'stat_col'. Acceptable values include 'hm' specifying harmonic mean,
#' 'min_max', specifying either minimum or maximum depending on \code{sig_order}, or the name of
#' a function. (default: \code{c('hm', 'min_max')})
#' @param seed_gs_fields (optional) A character vector specifying the names of additional seed gene
#' set fields to retain from pathways data.
#'
#' @return A data.frame with a statistical summary of subnets.
#'
#' @details The output data.frame contains a list of subnets, each with an associated list of gene
#' set IDs. For each subnet, summary statistics are calculated, including the harmonic mean of
#' \code{stat_col} and (if specified) \code{stat_col_2}. In addition, the minimum or maximum of the
#' \code{stat_col} and \code{stat_col_2} is calculated, depending on the \code{sig_order} and
#' \code{sig_order_2}. For \code{loToHi}, the minimum is calculated, and for \code{hiToLo}, the
#' maximum.
#'
#' @export
#'
#' @examples
#'
#' library(GSNA)
#'
#' # In this example, we generate a gene set network from CERNO example
#' # data. We begin by subsetting the CERNO data for significant results:
#' sig_pathways.cerno <- subset( Bai_CiHep_DN.cerno, adj.P.Val <= 0.05 )
#'
#' # Now create a gene set collection containing just the gene sets
#' # with significant CERNO results, by subsetting Bai_gsc.tmod using
#' # the gene set IDs as keys:
#' sig_pathways.tmod <- Bai_gsc.tmod[sig_pathways.cerno$ID]
#'
#' # And obtain a background gene set from differential expression data:
#' background_genes <- toupper( rownames( Bai_CiHep_v_Fib2.de ) )
#'
#' # Build a gene set network:
#' sig_pathways.GSN <-
#'    buildGeneSetNetworkJaccard(geneSetCollection = sig_pathways.tmod,
#'                               ref.background = background_genes )
#'
#' # Now import the CERNO data:
#' sig_pathways.GSN <- gsnImportCERNO( sig_pathways.GSN,
#'                                     pathways_data = sig_pathways.cerno )
#'
#' # Now we can pare the network and assign subnets:
#' sig_pathways.GSN <- gsnPareNetGenericHierarchic( object = sig_pathways.GSN )
#' sig_pathways.GSN <- gsnAssignSubnets(  object = sig_pathways.GSN )
#'
#' # Now, get a statistacal summary of the subnets:
#' gsnSubnetSummary( sig_pathways.GSN )
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
                              summary_statistics = c( "hm", "min_max" ),
                              seed_gs_fields = NULL
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
    # Keep only specified fields
    seed_gs_fields <- unique( c( c("ID", "NAME", "Title", "subnet" ), seed_gs_fields ) )
    seed_gs_fields <- seed_gs_fields[ seed_gs_fields %in% colnames( PW.subnets.minrank ) ]
    PW.subnets.minrank <- PW.subnets.minrank[,seed_gs_fields]
    # Preappend "seed " to PW.subnets.minrank fields.
    colnames(PW.subnets.minrank) <- paste0( "Seed.", colnames(PW.subnets.minrank) )

    #y = subset( PW.subnets, subnetRank == 1, select = c("subnet", "Title" ) ),
    SUM.subnets <- merge( x = SUM.subnets,
                          y = PW.subnets.minrank,
                          by.x = "subnet",
                          by.y = "Seed.subnet"
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


    # Calculate Harmonic and Geometric Mean LSTF Statistics for Subnets
    .subnets <- SUM.subnets$subnet
    lhm.lstf <- numeric()
    lgm.lstf <- numeric()
    for( .subnet in .subnets ){
      .gs_v <- unlist( PW.subnets[PW.subnets$subnet == .subnet, .id_col] )
      stlf.v <- calculate_stlf_vector( object$genePresenceAbsence[,.gs_v, drop = FALSE] )
      lhm.lstf[[.subnet]] <- lhm( stlf.v )
      lgm.lstf[[.subnet]] <- sum( stlf.v ) / length( stlf.v )
    }
    SUM.subnets$LHM.STLF <- lhm.lstf
    SUM.subnets$LGM.STLF <- lgm.lstf

    # Create list of IDs
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






#### Utility Functions

#' lse
#'
#' @description
#'
#' Implements the "Log-Sum-Exponential trick" for calculating the log of the sums of
#' exponents without arithmetic underflows. This allows very small numbers to be
#' summed in log space.
#'
#' @param a A numeric log value.
#' @param b Another numeric log value.
#'
#' @return The log of the sum of the exponents of a and b.
#'
#' @export
#'
#' @examples
#'
#' A <- 1E-40
#' B <- 3E-41
#'
#' log_AB <- lse( log(A), log(B) )
#'
#' # exp( log_AB ) == A + B
#'
#'
lse <- function(a,b){ a + log( 1 + exp( b - a ) ) }

# Calculates the harmonic mean in log space, i.e. the log of the exponentiated values in the vector.

lhm <- function( v ){
  k <- length( v )
  log_arithmetic_mean_of_reciprocols <- - v[1]
  if( k > 1 ){
    v <- v[order(-v)]
    for( i in 2:k ){
      log_arithmetic_mean_of_reciprocols <- lse( log_arithmetic_mean_of_reciprocols, - v[i] )
    }
  }
  log(k) - log_arithmetic_mean_of_reciprocols
}


# subnet_statistics <- function( mat ){
#   stlf.v <- calculate_stlf_vector( mat )
#   c( lhm.stlf = lhm(stlf.v),
#      lgm.stlf = sum( stlf.v ) / length(stlf.v) )
# }


#' calculate_stlf_vector
#'
#' @description
#' Given a gene presence/absence matrix, this function calculates a vector of unique
#' single-tail log-Fisher values for the different gene sets. This is for calculating
#' the similarities/and significance of all genes sets in a group of gene sets.
#'
#' @param mat A gene-set to gene presence absence matrix with gene-sets as columns,
#' genes as rows, TRUE = present and FALSE = absent.
#'
#' @return A vector of unique distances.
#'
#' @noRd
#' @keywords internal
calculate_stlf_vector <- function( mat ){
  if( is.null( dim(mat) ) ) return( NA )
  .GS_num <- ncol(mat)
  if( .GS_num < 2 ) return(NA)
  mat.stlf <- scoreLFMatrix_C( as.matrix(mat) )
  if( .GS_num == 2 ) return( mat.stlf[2,1] )
  stlf.v <- numeric()
  k <- 0
  for( i in 1:(.GS_num-1) ){
    for( j in (i + 1):.GS_num ){
      k <- k + 1
      stlf.v[k] <- mat.stlf[j,i]
    }
  }
  stlf.v
}







