
#' gsn_default_distance
#'
#' @description Retrieve or set default distances in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the default distance metric.
#'
#' @examples
#' \dontrun{
#'   # Print the value of the default_distance:
#'   gsn_default_distance( analysis.GSN )
#' }
#' @export
#'
gsn_default_distance <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  object$default_distance
}

#' @rdname gsn_default_distance
#' @param value A character vector of length 1 containing the name of a valid distance metric. The value must be
#' a valid distance metric, for which there exists a distance matrix in the GSNData object, or else an error will
#' be thrown.
#'
#' @examples
#' \dontrun{
#' # Set the value of the default_distance to 'jaccard':
#' gsn_default_distance( analysis.GSN ) <- 'jaccard'
#' }
#' @seealso \code{\link{gsn_distances}}
#' @export
`gsn_default_distance<-` <- function( object, value ){
  stopifnot( "GSNData" %in% class( object )  )
  if( value %in% names( object$distances ) ){
    object$default_distance <- value
  } else {
    stop("Object contains no distance called '", value, "'.")
  }
  object
}

#' gsn_distances
#'
#' @description Given a \code{GSNData} object, returns a character vector of distance matrices that are contained
#' within.
#'
#' @param object An object of type \code{GSNData}.
#'
#' @return A character vector containing the names of distance matrices.
#'
#' @examples
#' \dontrun{
#' # Print the names of distances in the GSNData object:
#' gsn_distances( analysis.GSN )
#' }
#'
#' @seealso \code{\link{gsn_default_distance}()}
#' @export
gsn_distances <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  names( object$distances )
}




####



#' pw_id_col
#'
#' @description Retrieve or set the pathways id_col field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the id_col field.
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' pw_id_col( analysis.GSN )
#' }
#' @export
pw_id_col <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$id_col
}

#' @rdname pw_id_col
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown.
#'
#' @return A GSNData object with the value of the \code{$pathways$id_col} field set.
#'
#' @examples
#' \dontrun{
#' # Set the value of the id_col to 'ID':
#' pw_id_col( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsn_distances}}
#' @export
`pw_id_col<-` <- function( object, value ){
  stopifnot( "GSNData" %in% class( object )  )
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


#' pw_stat_col
#'
#' @description Retrieve or set the pathways stat_col field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the stat_col field.
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' pw_stat_col( analysis.GSN )
#' }
#' @export
pw_stat_col <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$stat_col
}

#' @rdname pw_stat_col
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown.
#'
#' @return A GSNData object with the value of the \code{$pathways$stat_col} field set.
#'
#' @examples
#' \dontrun{
#' # Set the value of the stat_col to 'ID':
#' pw_stat_col( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsn_distances}}
#' @export
`pw_stat_col<-` <- function( object, value ){
  stopifnot( "GSNData" %in% class( object )  )
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


#' pw_stat_col_2
#'
#' @description Retrieve or set the pathways stat_col_2 field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the stat_col_2 field.
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' pw_stat_col_2( analysis.GSN )
#' }
#' @export
pw_stat_col_2 <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$stat_col_2
}

#' @rdname pw_stat_col_2
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown.
#'
#' @return A GSNData object with the value of the \code{$pathways$stat_col_2} field set.
#'
#' @examples
#' \dontrun{
#' # Set the value of the stat_col_2 to 'ID':
#' pw_stat_col_2( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsn_distances}}
#' @export
`pw_stat_col_2<-` <- function( object, value ){
  stopifnot( "GSNData" %in% class( object )  )
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


#' pw_sig_order
#'
#' @description Retrieve or set the pathways sig_order field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the sig_order field.
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' pw_sig_order( analysis.GSN )
#' }
#' @export
pw_sig_order <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$sig_order
}

#' @rdname pw_sig_order
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown. Valid values are
#' \code{'hiToLo'}, and \code{'loToHi'}.
#'
#' @return A GSNData object with the value of the \code{$pathways$sig_order} field set.
#'
#' @examples
#' \dontrun{
#' # Set the value of the sig_order to 'ID':
#' pw_sig_order( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsn_distances}}
#' @export
`pw_sig_order<-` <- function( object, value ){
  stopifnot( "GSNData" %in% class( object )  )
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


#' pw_sig_order_2
#'
#' @description Retrieve or set the pathways sig_order_2 field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the sig_order_2 field.
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' pw_sig_order_2( analysis.GSN )
#' }
#' @export
pw_sig_order_2 <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$sig_order_2
}

#' @rdname pw_sig_order_2
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown. Valid values are
#' \code{'hiToLo'}, and \code{'loToHi'}.
#'
#' @return A GSNData object with the value of the \code{$pathways$sig_order_2} field set.
#'
#' @examples
#' \dontrun{
#' # Set the value of the sig_order_2 to 'ID':
#' pw_sig_order_2( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsn_distances}}
#' @export
`pw_sig_order_2<-` <- function( object, value ){
  stopifnot( "GSNData" %in% class( object )  )
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

#' pw_n_col
#'
#' @description Retrieve or set the pathways n_col field in a \code{GSNData} object.
#'
#' @param object An GSNData object.
#'
#' @return The name of the n_col field.
#' @export
#'
#' @examples
#' \dontrun{
#' # Print the value of the default_distance:
#' pw_n_col( analysis.GSN )
#' }
#' @export
pw_n_col <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$n_col
}

#' @rdname pw_n_col
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown.
#'
#' @return A GSNData object with the value of the \code{$pathways$n_col} field set.
#'
#' @export
#' @examples
#' \dontrun{
#' # Set the value of the n_col to 'SIZE':
#' pw_n_col( analysis.GSN ) <- 'SIZE'
#' }
#' @seealso \code{\link{gsn_distances}}
#' @export
`pw_n_col<-` <- function( object, value ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) || is.null( object$pathways$data) )
    stop( "Object is missing pathways data." )
  if( is.null( value ) || value %in% colnames( object$pathways$data ) ){
    object$pathways$n_col <- value
  } else {
    stop("Pathways data contains no column '", value, "'.")
  }
  object
}





####


#' pw_type
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
#' pw_type( analysis.GSN )
#' }
#' @export
pw_type <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$type
}

#' @rdname pw_type
#' @param value A character vector of length 1 containing the name of a column within
#' the pathways data to be used as a gene set identifier. The GSNData object must contain
#' a pathways data data.frame, or else an error will be thrown.
#'
#' @return A GSNData object with the value of the \code{$pathways$type} field set.
#'
#' @examples
#' \dontrun{
#' # Set the value of the type to 'ID':
#' pw_type( analysis.GSN ) <- 'ID'
#' }
#' @seealso \code{\link{gsn_distances}}
#' @export
`pw_type<-` <- function( object, value ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) || is.null( object$pathways$data) )
    stop( "Object is missing pathways data." )
  # Currently we don't validate the value
  object$pathways$type <- value
  object
}






#' nzLog10
#'
#' @description Utility function to safely (non-zero) log10 transform p-values that are bounded at
#' 0, and may be zero or may be rounded to zero in certain contexts. To get around this, prior to
#' applying a log10 transformation the function adds a very small pseudocount to all the values if
#' any are detected to be zero. This avoids the generation of negative infinities. (See details, below.)
#'
#' @param x A numerical vector containing non-negative values.
#' @param quiet A boolean that tells the script to suppress warning messages. (default: FALSE. This
#' does not suppress errors, however.)
#' @param pseudocount_frxn A numerical value that sets the added pseudocount as a fraction of the
#' minimum non-zero value. (default: 0.5)
#'
#' @return A vector containing transformed values.
#'
#' @details Prior to log10 transformation, this function first scans for any zeros in the input vector.
#' If it finds any, it warns that zeros have been detected in the raw statistic, and that a pseudocount
#' will be added. To do this the function adds a fraction of the minimum non-zero value to the values
#' in x. (The old version of this function, \code{nzLog10.old()} determined the value of the pseudocount
#' in a much more complex way. This is a simplification here.)
#'
#' @export
#'
#' @examples
#'
#' p_vals <- c( 0.5, 0.001, 0.00001, 5e-19, 6.24e-23, 0 )
#' nzLog10( p_vals )
#'
nzLog10 <- function(x, quiet = FALSE, pseudocount_frxn = 0.5 ){
  pcount <- 0
  if( any( x < 0 ) ) stop( "Error: can't log10-transform negative numbers." )
  if( any( x == 0 ) ){
    pcount <- min( x[x>0], na.rm = TRUE ) * pseudocount_frxn
    if( ! quiet )
      warning( "Warning: raw statistic contains zeros. Adding a pseudocount of ", as.character( pcount ), "\n" )
  }
  log10(x + pcount)
}






#' nzLog10.old
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
nzLog10.old <- function(x, quiet = FALSE ){
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
#' @description Function for matching values in \code{.from} vector derived from \code{Gene symbol} field
#' from GEO feature data (e.g. "LOC101055758///LOC100041903///Gm2666///Gm7609///Csprs") with the first
#' match in \code{.to} vector. The point of this is for a given differentially expressed feature, match
#' the corresponding gene symbols to gene symbols present in a gene set collection. This (hopefully)
#' leads to mapping more features in a GEO dataset to more gene symbols in a gene set collection to be
#' searched. Symbol matches are done in a case independent way, and the value returned is the value in
#' the .to vector (with its particular capitalization), such that pathways analysis can be easily performed.
#'
#' @param .from Character vector containing concatenated, triple-slash delimited gene symbols/identifiers
#'  (e.g. "LOC101055758///LOC100041903///Gm2666///Gm7609///Csprs")
#' @param .to Character vector conrtaining gene symbols to be matched (e.g. "Gm2666")
#'
#' @return A vector containing the matched symbols.
#'
#' @export
#'
#' @examples
#' library(GSNA)
#' # These gene symbols correspond to the `Gene Symbol` field from a GEO dataset:
#' gene_symbols.from <- c( "BNS///CSMH///DDS1///THC8///BKRNS///BRWS1///PS1TP5BP1///ACTB",
#'                         "IP3R///IP3R1///ITPR1",
#'                         "FOS///p55///AP-1///C-FOS",
#'                         "MYC///LMYC///MYCL1///bHLHe38///L-Myc///v-myc"
#'                         )
#'
#' # Extract unique genes from the \code{Bai_gsc.tmod} gene set:
#' gene_symbols.to <- unique( unlist( tmod2gsc( Bai_gsc.tmod ) ) )
#'
#' mapped_symbols <- pick_MappedGeneSymbol( .from = gene_symbols.from,
#'                                          .to = gene_symbols.to )
#'
#' # mapped_symbols returns: "ACTB", "ITPR1", "FOS", "MYC"
#'
#'
#' # A very simple example:
#'
#' \dontrun{
#' # This is particularly useful with when mapping the \code{`Gene symbol`} field of
#' # GEO feature data to gene symbols in a GSC:
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






#' write_gmt
#'
#' @description
#' Takes a gene set collection (as a named list of vectors of genes), and a filename, and writes GMT format.
#' Right now, keeping this private.
#'
#' @param gsc A GSC (gene set collection) as a named list of character vectors of gene symbols, where
#' the names of the list items correspond to gene set identifiers.
#' @param filename An output file name.
#'
#' @return Currently returns a NULL value, invisibly.
#'
#' @details
#'
#' The function checks to see that the gsc argument is in fact a list of character vectors. If not,
#' it fails.
#'
#' @examples
#' library(GSNA)
#' gmtfile <- tempfile()
#' Bai_gsc.GSC <- tmod2gsc( Bai_gsc.tmod )
#' write_gmt( gsc = Bai_gsc.GSC, filename = gmtfile )
#'
#' @export
write_gmt <- function( gsc, filename ){
  if( ! "list" %in% class( gsc ) ||
      length(gsc) < 1 ||
      ! "character" %in% class(gsc[[1]]) ||
      is.null(names(gsc)) ){
    stop("gsc must be a named list of character vectors.")
  }

  gmt.file <- file( description = filename, open = "w" )
  for( gsc_name in names( gsc ) ){
    write( x = paste0( gsc_name, "\t\t", paste0( gsc[[gsc_name]], collapse = "\t" ) ), file = gmt.file )
  }
  close( gmt.file )
  invisible(NULL)
}



### Reading GMT format and converting a list of gene sets into a tmod object.

#' read_gmt
#'
#' @description This function parses a GMT file, documented
#' \href{https://software.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats#GMT:_Gene_Matrix_Transposed_file_format_.28.2A.gmt.29}{here}.
#'
#' @param file The path to GMT file to parse.
#'
#' @return This returns a GSC (gene set collection) as a name list of vectors, where the names correspond to gene set
#' identifiers and the vectors are gene symbols.
#'
#' @seealso [gsc2tmod()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gsc <- read_gmt( "gene_set_collection.GMT" )
#' }
#'
#'
read_gmt <- function( file ){
  .lines <- readLines( con = file )
  gsc <- list()
  .dat <- stringr::str_split( string = .lines, pattern = "\t" )
  for( .datum in .dat ){
    if( length( .datum ) > 2 ){
      gs_name <- .datum[[1]]
      gs <- .datum[3:length(.datum )]
      gsc[[gs_name]] <- gs
    }
  }
 gsc
}

#' gsc2tmod
#'
#' @description Function to convert a GSC in the form of a named list of vectors containing gene symbols
#' to a object of class \code{tmod} which was used by the tmod prior to version \code{0.50.11}. This is
#' a wrapper for \code{\link[tmod]{makeTmod}()} from the \code{tmod} package.
#'
#' @param MODULES2GENES A named list of character vectors in which the vectors correspond to gene sets
#' and contain gene symbols (or other gene identifiers) and the names are the corresponding gene set
#' identifiers.
#'
#' @param MODULES (optional) A data.frame containing an \code{ID} and a \code{Title} field
#' in the same order as the gene sets in \code{MODULES2GENES}. Furthermore, the row names should
#' (apparently) correspond to the IDs in the corresponding rows. If not provided, this will be generated
#' automatically.
#'
#' @param GENES (optional) A data frame with gene metadata. Must contain an ID column. If not provided,
#' this will be generated automatically.
#'
#' @return Returns a \code{tmod} object if the \code{tmod} package version \code{'0.46.2'} or earlier is
#' installed. If the \code{tmod} package version '0.50.11' or later is installed, it returns a
#' \code{tmodGS} object instead.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   gsc <- read_gmt( "gene_set_collection.GMT" )
#'   gsc.tmod <- gsc2tmod( gsc )
#' }
#'
#' @seealso [read_gmt()] [tmod2gsc()]
#'
#' @importFrom methods new
#' @importFrom tmod makeTmod
#'
gsc2tmod <- function( MODULES2GENES, MODULES = NULL, GENES = NULL ){
  if( is.null( MODULES ) )
    MODULES <-
      data.frame( ID = names( MODULES2GENES ),
                  Title = stringr::str_to_title( gsub( pattern = "_",
                                                       replacement = " ",
                                                       x = names( MODULES2GENES ) )
                  ),
                  row.names = names( MODULES2GENES ),
                  stringsAsFactors = FALSE,
                  check.names = FALSE
      )
  if( is.null( GENES ) )
    GENES <- data.frame( ID = unique( unlist( MODULES2GENES ) ) )

  # Sanity checks:
  if( length(names( MODULES2GENES )) != length(MODULES$ID) ||
      ! all( names( MODULES2GENES ) == MODULES$ID )  )
    stop("Mismatch beween names(MODULES2GENES) and MODULES$ID")

  if( any( ! unlist( MODULES2GENES ) %in% GENES$ID ) || any( ! GENES$ID %in% unlist( MODULES2GENES ) ) )
    stop("Mismatch beween unlist( MODULES2GENES ) %in% GENES$ID")

  # This should work with versions up to '0.46.2' as well as '0.50.11' and after.
  tmod::makeTmod( modules = MODULES, modules2genes = MODULES2GENES, genes = GENES )
}




#' tmod2gsc
#'
#' @description Function takes a tmod or tmodGS object and converts it to a gene set collecton. In the case of a
#' tmod object, the function merely extracts the \code{$MODULES2GENES} list of character vectors. In the case of
#' tmodGS objects, the list of vectors of numeric gene identifiers in \code{$gs2gv} is converted to a named list
#' of character vectors of gene names.
#'
#' @param tmod : a tmod or tmodGS object.
#'
#' @return The function returns a gene set collection as a named list of character vectors containing gene names.
#'
#' @seealso [gsc2tmod()]
#'
#' @export
#'
tmod2gsc <- function( tmod ){
  if( 'tmod' %in% class( tmod ) ){
    gsc <- tmod$MODULES2GENES
  } else if( 'tmodGS' %in% class( tmod ) ){
    # This maps the numerical coded genes and gene sets to a named list of character vectors.
    gsc <- lapply( X = tmod$gs2gv,
                   FUN = function( gs ){
                     tmod$gv[unlist(gs)]
                   } )
    names(gsc) <- tmod$gs$ID
  } else {
    stop("Can't convert class '", class(tmod), "'");
  }
  gsc
}


#' intV2Color
#'
#' @description Converts a numeric or integer vector of length 3 containing
#' RGB values in the range of 0 to 255 to 24 bit color specifications in the
#' form "#FFFFFF".
#'
#' @param rgb_v An integer or numeric vector of length 3 containing RGB channel
#' intensities from 0 to 255.
#'
#' @return A 24-bit color specification in the form "#FFFFFF".
#' @export
#'
#' @examples
#'
#' col_v <- c( 255, 100, 240)
#' col <- intV2Color( col_v )
#'
#' @seealso [color2IntV()]
intV2Color <- function( rgb_v ){
  if( ! any( c( "numeric", "integer" ) %in% class( rgb_v ) ) )
    stop( "Incorrect data type '", class( rgb_v ), "', expected numeric."  )

  if( length( rgb_v ) != 3 )
    stop( "Incorrect data length '", length( rgb_v ), "', expected length 3."  )

  if( any( is.na( rgb_v ) ) || any( is.nan( rgb_v ) ))
    stop( "Missing data. Vector contain NA or NaN." )

  if( any( rgb_v > 255) || any( rgb_v < 0 ) )
    stop( "Invalid input." )

  rgb_v[is.na(rgb_v ) | rgb_v > 255] <- 255
  rgb_v[rgb_v < 0] <- 0
  paste0( "#", paste0( sprintf( "%02X", round(rgb_v) ), collapse = "" ) )
}

#' color2IntV
#'
#' @description Convert a color, either as a name or as a RGB hexedecimal value to an integer vector containing
#' the RGB specification.
#'
#'
#' @param color A color specified either by name (e.g. "red") or as a RGB hexadecimal value (e.g. "#FF0000").
#'
#' @return A integer vector containing the RGB specification.
#'
#' @importFrom grDevices col2rgb
#'
#' @seealso [intV2Color()]
#'
color2IntV <- function( color ){
  as.vector(grDevices::col2rgb(color))
}



