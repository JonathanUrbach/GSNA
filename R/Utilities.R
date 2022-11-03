
#' gsnDefaultDistance
#'
#' @description Retrieve or set default distances in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the default distance metric.
#' @export
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' gsnDefaultDistance( analysis.GSN )
#' }
gsnDefaultDistance <- function( object ){
  stopifnot( class( object ) == "GSNData" )
  object$default_distance
}

#' @rdname gsnDefaultDistance
#' @param value A character vector of length 1 containing the name of a valid distance metric. The value must be
#' a valid distance metric, for which there exists a distance matrix in the GSNData object, or else an error will
#' be thrown.
#' @export
#' @examples
#' \dontrun{
#' # Set the value of the default_distance to 'jaccard':
#' gsnDefaultDistance( analysis.GSN ) <- 'jaccard'
#' }
#' @seealso \code{\link{gsnDistances}}
`gsnDefaultDistance<-` <- function( object, value ){
  stopifnot( class( object ) == "GSNData" )
  if( value %in% names( object$distances ) ){
    object$default_distance <- value
  } else {
    stop("Object contains no distance called '", value, "'.")
  }
  object
}

#' gsnDistances
#'
#' @description Given a \code{GSNData} object, returns a character vector of distance matrices that are contained
#' within.
#'
#' @param object An object of type \code{GSNData}.
#'
#' @return A character vector containing the names of distance matrices.
#' @export
#'
#' @examples
#' \dontrun{
#' # Print the names of distances in the GSNData object:
#' gsnDistances( analysis.GSN )
#' }
#'
#' @seealso \code{\link{gsnDefaultDistance}()}
gsnDistances <- function( object ){
  stopifnot( class( object ) == "GSNData" )
  names( object$distances )
}




####

#' gsnPathways_id_col
#'
#' @description Retrieve or set the pathways id_col field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the id_col field.
#' @export
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' gsnPathways_id_col( analysis.GSN )
#' }
gsnPathways_id_col <- function( object ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$id_col
}

#' @rdname gsnPathways_id_col
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown.
#'
#' @return A GSNData object with the value of the \code{$pathways$id_col} field set.
#'
#' @export
#' @examples
#' \dontrun{
#' # Set the value of the id_col to 'ID':
#' gsnPathways_id_col( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsnDistances}}
`gsnPathways_id_col<-` <- function( object, value ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) || is.null( object$pathways$data) )
    stop( "Object is missing pathways data." )
  if( is.null( value ) || value %in% colnames( object$pathways$data ) ){
    object$pathways$id_col <- value
  } else {
    stop("Pathways data contains no column '", value, "'.")
  }
  object
}

###


#' gsnPathways_stat_col
#'
#' @description Retrieve or set the pathways stat_col field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the stat_col field.
#' @export
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' gsnPathways_stat_col( analysis.GSN )
#' }
gsnPathways_stat_col <- function( object ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$stat_col
}

#' @rdname gsnPathways_stat_col
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown.
#'
#' @return A GSNData object with the value of the \code{$pathways$stat_col} field set.
#'
#' @export
#' @examples
#' \dontrun{
#' # Set the value of the stat_col to 'ID':
#' gsnPathways_stat_col( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsnDistances}}
`gsnPathways_stat_col<-` <- function( object, value ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) || is.null( object$pathways$data) )
    stop( "Object is missing pathways data." )
  if( is.null( value ) || value %in% colnames( object$pathways$data ) ){
    object$pathways$stat_col <- value
  } else {
    stop("Pathways data contains no column '", value, "'.")
  }
  object
}

####


#' gsnPathways_stat_col_2
#'
#' @description Retrieve or set the pathways stat_col_2 field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the stat_col_2 field.
#' @export
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' gsnPathways_stat_col_2( analysis.GSN )
#' }
gsnPathways_stat_col_2 <- function( object ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$stat_col_2
}

#' @rdname gsnPathways_stat_col_2
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown.
#'
#' @return A GSNData object with the value of the \code{$pathways$stat_col_2} field set.
#'
#' @export
#' @examples
#' \dontrun{
#' # Set the value of the stat_col_2 to 'ID':
#' gsnPathways_stat_col_2( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsnDistances}}
`gsnPathways_stat_col_2<-` <- function( object, value ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) || is.null( object$pathways$data) )
    stop( "Object is missing pathways data." )
  if( is.null( value ) || value %in% colnames( object$pathways$data ) ){
    object$pathways$stat_col_2 <- value
  } else {
    stop("Pathways data contains no column '", value, "'.")
  }
  object
}

####


#' gsnPathways_sig_order
#'
#' @description Retrieve or set the pathways sig_order field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the sig_order field.
#' @export
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' gsnPathways_sig_order( analysis.GSN )
#' }
gsnPathways_sig_order <- function( object ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$sig_order
}

#' @rdname gsnPathways_sig_order
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown. Valid values are
#' \code{'hiToLo'}, and \code{'loToHi'}.
#'
#' @return A GSNData object with the value of the \code{$pathways$sig_order} field set.
#'
#' @export
#' @examples
#' \dontrun{
#' # Set the value of the sig_order to 'ID':
#' gsnPathways_sig_order( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsnDistances}}
`gsnPathways_sig_order<-` <- function( object, value ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) || is.null( object$pathways$data) )
    stop( "Object is missing pathways data." )
  if(  is.null( value ) || value %in% c( "hiToLo", "loToHi" ) ){
    object$pathways$sig_order <- value
  } else {
    stop("Invalid value for sig_order '", value, "'.")
  }
  object
}

####


#' gsnPathways_sig_order_2
#'
#' @description Retrieve or set the pathways sig_order_2 field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the sig_order_2 field.
#' @export
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' gsnPathways_sig_order_2( analysis.GSN )
#' }
gsnPathways_sig_order_2 <- function( object ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$sig_order_2
}

#' @rdname gsnPathways_sig_order_2
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown. Valid values are
#' \code{'hiToLo'}, and \code{'loToHi'}.
#'
#' @return A GSNData object with the value of the \code{$pathways$sig_order_2} field set.
#'
#' @export
#' @examples
#' \dontrun{
#' # Set the value of the sig_order_2 to 'ID':
#' gsnPathways_sig_order_2( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsnDistances}}
`gsnPathways_sig_order_2<-` <- function( object, value ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) || is.null( object$pathways$data) )
    stop( "Object is missing pathways data." )
  if( is.null( value ) || value %in% c( "hiToLo", "loToHi" ) ){
    object$pathways$sig_order_2 <- value
  } else {
    stop("Invalid value for sig_order_2 '", value, "'.")
  }

  object
}

####


#' gsnPathways_type
#'
#' @description Retrieve or set the pathways type field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the type field.
#' @export
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' gsnPathways_type( analysis.GSN )
#' }
gsnPathways_type <- function( object ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$type
}

#' @rdname gsnPathways_type
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown.
#'
#' @return A GSNData object with the value of the \code{$pathways$type} field set.
#'
#' @examples
#' \dontrun{
#' # Set the value of the type to 'ID':
#' gsnPathways_type( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsnDistances}}
`gsnPathways_type<-` <- function( object, value ){
  stopifnot( class( object ) == "GSNData" )
  if( is.null( object$pathways ) || is.null( object$pathways$data) )
    stop( "Object is missing pathways data." )
  if( value %in% colnames( object$pathways$data ) ){
    object$pathways$type <- value
  } else {
    stop("Pathways data contains no column '", value, "'.")
  }
  object
}

####



#' print.GSNData
#'
#' @description Print a short description of a \code{GSNData} object.
#'
#' @param object A GSNData object.
#'
#' @return Invisibly returns the GSNData object.
#'
#' @export
#' @exportS3Method
print.GSNData <- function( object ){
  cat( "GSNData object version:", unlist( as.character( object$GSNA_version ) ), "\n" )

  if( !is.null( object$genePresenceAbsence ) ){
    cat( "  Contains data for:\n" )
    cat( "    ", nrow( object$genePresenceAbsence ), "genes.\n" )
    cat( "    ", ncol( object$genePresenceAbsence ), "gene sets.\n" )
  }

  if( ! is.null( distz <- names(object$distances) ) ){
    cat( "  Contains the following distance(s):\n" )
    for( .dist in distz ){
      cat( paste0( "     ", .dist, "\n" ) )
    }
  }
  if( ! is.null( distz <- names(object$pathways) ) ){
    .type <- object$pathways$type
    if( is.null( .type ) ) .type <- "NULL"
    cat( "  Contains pathways data of type: ", .type , "\n" )
    for( .datname in c( "id_col", "stat_col", "sig_order", "stat_col_2", "sig_order_2" ) )
    if( !is.null(object$pathways[[.datname]] )  ){
      cat( "    ",  .datname, "=", object$pathways[[.datname]], "\n" )
    }
  }
  return( invisible( object ) )
}













#' nzLog10
#'
#' @description Utility function to safely (non-zero) log10 transform p-values that are bounded at 0, and may be zero or
#' may be rounded to zero in certain contexts. To get around this, prior to applying a log10 transformation the function
#' adds a very small pseudocount to all the values if any are detected to be zero. This avoids the generation of negative
#' infinities. (See details, below.)
#'
#' @param x A numerical vector containing non-negative values.
#' @param quiet A boolean that tells the script to suppress warning messages. (This does not suppress errors, however.)
#'
#' @return A vector containing transformed values.
#'
#' @details Prior to log10 transformation, this function first scans for any zeros in the input vector. If it
#' finds any, it warns that zeros have been detected in the raw statistic, and that a pseudocount will be added.
#' To do this the function assesses the precision of the numbers in the numerical vector by counting decimal
#' places and determining the minimal non-zero number represented in the vector. It then takes whichever is the
#' lesser of those numbers and adds a pseudocount equal to the lesser of 1/2 the precision, or 1/2 the lowest
#' non-zero number.
#'
#' @export
#'
#' @examples
#'
#' p_vals <- c( 0.5, 0.001, 0.00001, 5e-19, 6.24e-23, 0 )
#' nzLog10( p_vals )
#'
nzLog10 <- function(x, quiet = FALSE ){
  pcount <- 0
  if( any( x < 0 ) ) stop( "Error: can't log10-transform negative numbers." )
  if( any( x == 0 ) ){
    decimal_precision <- max( max(nchar(gsub(x = as.character(x), pattern = "^\\d+\\.", replacement = "" )),
                                  na.rm = TRUE),
                              max(-log10(x[x>0]),
                                  na.rm = TRUE ),
                              na.rm = TRUE )
    pcount <- 10^(- (decimal_precision + log10(2)))
    if( ! quiet )
      warning( "Warning: raw statistic contains zeros. Adding a pseudocount of ", as.character( pcount ), "\n" )
  }
  log10(x + pcount)
}


#' antiSplit
#'
#' @description Convert a list of vectors to a data.frame. This method does the opposite of the R base split, but
#' more conveniently than unsplit.
#'
#' @param .l A list with named elements that are vectors.
#' @param col.names The names of the output columns. Defaults to col.names = c("V1","V2").
#'
#' @return A data.frame is returned with two character columns. The list element names become the first column,
#' whereas the values within the vectors become the second column.
#'
#' This is used by \code{assignSubnets()}. We're not currently exporting it.
#'
#' @examples
#'
#' library(GSNA)
#' data.l<-list( A = c( 1, 2, 3, 4 ), B = c( 3, 6 ), C = c( 7, 3, 2 ) )
#' data.df <- GSNA:::antiSplit( data.l, c("Letters", "NumsAsCharacters") )
#'
antiSplit <- function( .l, col.names = c("V1","V2") ){
  namez <- NULL
  if(is.null(namez <- names(.l))){
    namez <- as.character(1:length(.l))
  }
  structure(data.frame( as.character(unlist( rep( namez, sapply( X = .l, FUN = length )))),
                        as.character(unlist( .l )),
                        stringsAsFactors = FALSE),
            names = col.names )
}








#' pick_MappedGeneSymbol
#'
#' @description Function for matching values in \code{.from} vector derived from \code{Gene symbol} field from GEO feature
#' data (e.g. "LOC101055758///LOC100041903///Gm2666///Gm7609///Csprs") with the first match in \code{.to} vector. The point
#' of this is for a given differentially expressed feature, match the corresponding gene symbols to gene symbols present in
#' a gene set collection. This (hopefully) leads to mapping more features in a GEO dataset to more gene symbols in a gene
#' set collection to be searched. Symbol matches are done in a case independent way, and the value returned is the value
#' in the .to vector (with its particular capitalization), such that pathways analysis can be easily performed.
#'
#' @param .from Character vector containing concatenated, triple-slash delimited gene symbols
#'  (e.g. "LOC101055758///LOC100041903///Gm2666///Gm7609///Csprs")
#' @param .to Character vector conrtaining gene symbols to be matched (e.g. "Gm2666")
#'
#' @return A vector containing the matched symbols.
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' library(GSNA)
#' library(GEOquery)
#' library(tmod)
#'
#' gset <- getGEO("GSE75203", GSEMatrix =TRUE, AnnotGPL=TRUE)
#' GSE75203.fdata <- fData(gset$GSE75203_series_matrix.txt.gz)
#' msig <- tmodImportMSigDB( file = file.path( "msigdb_v7.5.1.xml.gz" ) )
#'
#' GSE75203.fdata$MappedGeneSymbol <- pick_MappedGeneSymbol( .from = GSE75203.fdata$`Gene symbol`,
#'                                                           .to = msig$GENES$ID )
#' }
#'
#'
pick_MappedGeneSymbol <- function( .from, .to ){
  .mapped <- rep( x = NA, length(.from) )
  .to <- as.character( .to ) # Original case
  .TO <- toupper( .to )      # Upper case for matching
  for( i in 1:length(.from) ){
    SYMZ <- toupper( unlist( strsplit( x = .from[i], split = "///" )))
    symz <- .to[.TO %in% SYMZ]
    if( length(symz) > 0 )
      .mapped[i] <- symz[1]
  }
  .mapped
}


