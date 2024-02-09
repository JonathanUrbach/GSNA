

#' gsn_default_distance, gsn_distances, pw_id_col, pw_stat_col, pw_sig_order, pw_stat_col_2, pw_sig_order_2, pw_n_col, pw_type
#'
#' @description Get and set values for GSNData internal fields. When evaluated directly or on
#' the right side of an \code{<-} assignment, these functions retrieve stored values. When
#' evaluated on the left side of a \code{<-} assignment, they set the relevant field values.
#'
#' @param object A GSNData object.
#' @param value A valid value for the field to be set. (see *Details*.).
#'
#' @return For get versions of the functions, evaluated alone or on the right side of a \code{<-}
#' assignment, the values stored in the relevant fields are returned, generally as character vectors
#' of length 1 (or NULL), except for gsn_distances which may return character vectors of varying length.
#' For set versions of the functions, for which the function call is on the left side of a \code{<-}
#' assignment, a copy of the \code{GSNData} object with the specified field set is returned. See
#' *Details*.)
#'
#' @details
#' \describe{
#'   \item{**gsn_default_distance()**}{Gets and sets the value stored in the \code{obect$default_distance}
#'         field of a \code{GSNData} object. When a \code{GSNData} object contains distance matrices of
#'         multiple types (e.g. single-tail log fisher & Jaccard, the \code{defailt_distance} field tells
#'         \code{GSNA} which distance to use for network paring, subnet assignment, plotting, etc.
#'         When setting the value, it must be a character vector of length 1 that contains the name of
#'         a valid distance metric for which there exists a distance matrix in the GSNData object, or
#'         else an error will be raised.}
#'   \item{**gsn_distances()**}{Returns a character vector containing the names of the distances for which
#'         there are distance matrices in a \code{GSNData} object.}
#'   \item{**pw_id_col()**}{Gets and sets the value stored in the \code{obect$pathways$id_col} field. This
#'         is the field that determines which column in a pathways data.frame corresponds to a gene set
#'         identifier used in a gene set collection list of vectors, or a \code{tmod} or \code{tmodGS}
#'         object. When setting the value, this the function checks that the value is a valid column in
#'         \code{obect$pathways$data}.}
#'   \item{**pw_stat_col()**}{Gets and sets the value stored in the \code{obect$pathways$stat_col} field.
#'         This is the field that determines which column in a pathways data.frame corresponds to a
#'         significance statistic of interest. When setting the value, this the function checks that the
#'         value is a valid column in \code{obect$pathways$data}.}
#'   \item{**pw_sig_order()**}{Gets and sets the value stored in the \code{obect$pathways$sig_order} field.
#'         This is the field that states the behavior of the significance value in the
#'         \code{obect$pathways$sig_order} field, specifically whether low or high values are significant.
#'         This may be either \code{'loToHi'} or \code{'hiToLo'}. (Other types of statistics are possible,
#'         for example statistics with significant of high or low *absolute* values. We hope to add
#'         support for such statistics in the future.)}
#'   \item{**pw_stat_col_2()**}{Gets and sets the value stored in the \code{obect$pathways$stat_col_2} field.
#'         For two-channel GSNA analysis, this is the field that determines which column in a pathways
#'         data.frame corresponds to the second significance statistic of interest. When setting the value,
#'         this the function checks that the value is a valid column in \code{obect$pathways$data}.}
#'   \item{**pw_sig_order_2()**}{Gets and sets the value stored in the \code{obect$pathways$sig_order_2} field.
#'         For a two-channel GSNA analysis, this is the field that states the behavior of the significance
#'         value in the \code{obect$pathways$sig_order_2} field, specifically whether low or high values
#'         are significant. This may be either \code{'loToHi'} or \code{'hiToLo'}. (Other types of
#'         statistics are possible, for example statistics with significant of high or low *absolute*
#'         values. We hope to add support for such statistics in the future.)}
#'   \item{**pw_n_col()**}{Gets and sets the value stored in the \code{obect$pathways$n_col} field.
#'         This is the field that determines which column in a pathways data.frame corresponds to a
#'         gene set size or gene set effective size. When setting the value, this the function checks
#'         that the value is a valid column in \code{obect$pathways$data}.}
#'   \item{**pw_type()**}{Gets and sets the value stored in the \code{obect$pathways$type} field.
#'         This is the field that describes what kind of pathways data are stored in the object,
#'         e.g. \code{'gsea'} or \code{'cerno'}. If there is no current pathways data in the object
#'         an error will be raised.}
#' }
#'
#' @examples
#' # These examples require some setup.
#' #
#' # First, we will generate a gene set network from CERNO example
#' # data, containing multiple distance metrics, as well as pathways
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
#' # Create a GSNData object containing Jaccard indices:
#' sig_pathways.GSN <-
#'    buildGeneSetNetworkJaccard(geneSetCollection = sig_pathways.tmod,
#'                               ref.background = background_genes )
#'
#' # Within the same object, add an 'stlf' (Single Tail Log Fisher)
#' # distance matrix:
#' sig_pathways.GSN <-
#'    buildGeneSetNetworkSTLF( object = sig_pathways.GSN )
#'
#' # Now import the CERNO data:
#' sig_pathways.GSN <- gsnAddPathwaysData( sig_pathways.GSN,
#'                                         pathways_data = sig_pathways.cerno )
#'
#' # Use gsn_distances() to see what distances are stored in the
#' # GSNData object:
#' gsn_distances( sig_pathways.GSN )
#' # Should return: "jaccard" "stlf"
#'
#' # See what the default distance is:
#' gsn_default_distance( sig_pathways.GSN )
#' # Returns: "stlf". Let's change the default distannce
#' # to "jaccard":
#' gsn_default_distance( sig_pathways.GSN ) <- "jaccard"
#'
#' # Let's examine what the ID column is:
#' pw_id_col( sig_pathways.GSN )
#' # Returns: "ID"
#' pw_id_col( sig_pathways.GSN ) <- "ID"
#' # This is equivalent to the following code. When
#' # invoked on the left side of an assignment, R uses
#' # *syntactic sugar* to comvert the call to:
#' sig_pathways.GSN <- `pw_id_col<-`( object = sig_pathways.GSN,
#'                                    value = "ID" )
#'
#' # On the other hand, the following returns an error
#' # because there is no column in the pathways dataframe
#' # named "invalid.name":
#'  class( try( pw_id_col( sig_pathways.GSN ) <- "invalid.name" ) )
#'  # "try-error"
#'
#'
#'
#' # Likewise we can get and set the value of stat_col
#' # and sig_order:
#' pw_stat_col(sig_pathways.GSN )
#' # Returns "adj.P.Val". Let's set it to "AUC"
#' pw_stat_col(sig_pathways.GSN) <- "AUC"
#' # And likewise, sig_order:
#' pw_sig_order(sig_pathways.GSN) # "loToHi"
#' pw_sig_order(sig_pathways.GSN) <- "hiToLo"
#'
#'
#' # For 2-channel GSNA analyses, we can set the values
#' # of stat_col_2 and sig_order_2:
#' pw_stat_col_2(sig_pathways.GSN )
#' # Returns NULL. Let's set it to "P.Value"
#' pw_stat_col_2(sig_pathways.GSN) <- "P.Value"
#' # And likewise, sig_order:
#' pw_sig_order_2(sig_pathways.GSN) # NULL
#' pw_sig_order(sig_pathways.GSN) <- "loToHi"
#'
#' # pw_n_col() works the same way to set n_col:
#' pw_n_col(sig_pathways.GSN) # "N1"
#' pw_n_col(sig_pathways.GSN) <- "N1"
#'
#' # And also, pw_type()
#' pw_type(sig_pathways.GSN) # "cerno"
#' # For setting via pw_type, the value is not
#' # currently checked, since pathways data may
#' # be of many types:
#' pw_type(sig_pathways.GSN) <- "other"
#'
#' pw_type(sig_pathways.GSN) # "other"
#'
#'
#' @rdname getAndSetFunctions
#' @export
#'

gsn_default_distance <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  object$default_distance
}



#' @rdname getAndSetFunctions
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


#' @rdname getAndSetFunctions
#' @export
gsn_distances <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  names( object$distances )
}


#####

#' @export
#' @rdname  getAndSetFunctions
pw_id_col <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$id_col
}

#' @export
#' @rdname  getAndSetFunctions
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


#' @export
#' @rdname  getAndSetFunctions
pw_stat_col <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$stat_col
}



#' @export
#' @rdname  getAndSetFunctions
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




#' @export
#' @rdname  getAndSetFunctions
pw_stat_col_2 <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$stat_col_2
}



#' @export
#' @rdname  getAndSetFunctions
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



#' @export
#' @rdname  getAndSetFunctions
pw_sig_order <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$sig_order
}



#' @export
#' @rdname  getAndSetFunctions
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


#' @export
#' @rdname  getAndSetFunctions
pw_sig_order_2 <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$sig_order_2
}


#' @export
#' @rdname  getAndSetFunctions
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

#' @export
#' @rdname  getAndSetFunctions
pw_n_col <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$n_col
}

#' @export
#' @rdname  getAndSetFunctions
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

#' @export
#' @rdname  getAndSetFunctions
pw_type <- function( object ){
  stopifnot( "GSNData" %in% class( object )  )
  if( is.null( object$pathways ) ) stop("Object is missing pathways data.")
  object$pathways$type
}

#' @export
#' @rdname  getAndSetFunctions
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
#' @details
#'
#' example:
#' \code{
#' library(GSNA)
#' data.l<-list( A = c( 1, 2, 3, 4 ), B = c( 3, 6 ), C = c( 7, 3, 2 ) )
#' data.df <- GSNA:::antiSplit( data.l, c("Letters", "NumsAsCharacters") )
#' }
#'
#' @noRd
#' @keywords internal
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
#' @param .to Character vector containing gene symbols to be matched (e.g. "Gm2666")
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
#' \donttest{
#'  # This example requires a web-based download of a GEO data set
#'  # and takes > 20 seconds to run on some platforms.
#'
#'  # This function is particularly useful with when mapping
#'  # the \code{`Gene symbol`} field of GEO feature data to
#'  # gene symbols in a GSC:
#'
#'  library(GSNA)
#'  library(GEOquery)
#'  library(tmod)
#'
#'  gset <- getGEO("GSE75203", GSEMatrix =TRUE, AnnotGPL=TRUE)
#'  GSE75203.fdata <- fData(gset$GSE75203_series_matrix.txt.gz)
#'
#'  # We can match the gene gene symbols in GSE75203.fdata with
#'  # those in the provided Bai_gsc.tmod object, and add the
#'  # mapped gene symbol to a new column in GSE75203.fdata,
#'  # 'MappedGeneSymbol':
#'  GSE75203.fdata$MappedGeneSymbol <-
#'    pick_MappedGeneSymbol( .from = GSE75203.fdata$`Gene symbol`,
#'                           .to = Bai_gsc.tmod$GENES$ID )
#'  # NOTE, if you were using a tmodGS object, the above
#'  # would be this instead:
#'  # GSE75203.fdata$MappedGeneSymbol <-
#'  #   pick_MappedGeneSymbol( .from = GSE75203.fdata$`Gene symbol`,
#'  #                          .to = Bai_gsc.tmodGS$gv )
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
#' @description Function takes a tmod or tmodGS object and converts it to a gene set collection. In the case of a
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
#' @description Convert a color, either as a name or as a RGB hexadecimal value to an integer vector containing
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



