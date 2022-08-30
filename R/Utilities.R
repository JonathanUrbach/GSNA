
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


