


# get_stopwords
#
# @description Private function that returns a curated list of stopwords for use with the
# \code{filterTerms()} and \code{termSummary()} functions. This a curated list derived from
# the **stopwords** package and us similar but not identical to the output of the following
# command:
#
# \code{stopwords::stopwords( language = "en", source = "snowball" )}
#
# The primary difference is that terms like "a", "and", "of", "by", "for", "to" & "up" were
# removed since they may be relevant for understanding summary terms, and some terms were
# added ("gobp", "gocc", "gomf", "kegg", "reactome" & "biocarta"). The word vector generated
# is used to filter words out of the terms to be summarized.
#
# @returns A character vector of stopwords.
#' @importFrom stopwords stopwords
get_stopwords <- function(  ){
  .stopwords <- stopwords::stopwords("en", source = "snowball")
  .stopwords <- c(.stopwords, "gobp", "gocc", "gomf", "kegg", "reactome", "biocarta", "wp" )
  .stopwords <- .stopwords[! .stopwords %in% c("for", "of", "by", "and", "a", "to", "up", "dn", "i" )]
  .stopwords
}



# get_zero_score_words
#
# @description These are common stopwords and identifiers in MSigDB.
#
# @returns A vector of words for which the score should be zero.
#
get_zero_score_words <-  function( ){
  c("gobp", "of", "gomf", "reactome", "gocc", "reactome", "wp", "and", "dn", "up", "biocarta", "a", "to" )
}



#' score_word_clusters
#'
#' @param term_cluster A vector of terms corresponding to a group of descriptive gene signature names.
#' These names may contain underscores or spaces and be in either upper or lower case.
#' @param single_word_score The score awarded to a word cluster for a single non-zero score word. (default 1)
#' @param additional_word_score The score awarded to a word cluster for subsequent non-zero score words. (default 1)
#' @param priority_factor The factor by which successive occurrences of word clusters are attenuated. (default 1.2)
#' @param zero_score_words A list of zero-score words. (defaults to the value returned by
#' \code{get_zero_score_words()})
#'
#' @description
#' This function takes a vector of terms (a 'term cluster') corresponding to a group of descriptive
#' gene signature names, and breaks them down into subgroupings "word clusters", which are then scored
#' on the basis of the number of times they appear, number of words comprising them, and by their
#' priority, with words earlier in the list generally receiving higher priority
#' (if \code{priority_factor > 1}). The score for single words can be set independently of additional
#' words (\code{single_word_score} and \code{additional_word_score}), but by default they are both 1.
#' Words for which no score is given (e.g. "gobp","of") can also be specified as a character vector.
#' This function is the basis of \code{\link{termSummary}()}.
#'
#' @returns A named numeric vector for which the values are scores for each word-cluster, and the names
#' are word clusters summarizing the input cluster of terms. The results are sorted by descending score,
#' such that the first name:value pair corresponds to the best/highest scoring word cluster and its
#' score.
#'
#' @export
#'
#' @examples
#' library( GSNA )
#' terms <- c("complement activation", "complement cascade", "complement binding",
#'             "wp complement activation classical pathway",
#'             "complement activation classical pathway",
#'             "humoral immune response mediated by circulating immunoglobulin",
#'             "comp pathway", "regulation of complement activation",
#'             "classic pathway", "complement activation alternative pathway",
#'             "initial triggering of complement",  "lectin pathway",
#'             "opsonin binding")
#'  word_cluster_scores <- score_word_clusters( terms )
#'  # Best summary word cluster:
#'  word_cluster_scores[1]
#'  # complement activation
#'  # 5.06769
#'
#'  # With different scoring weights:
#'  score_word_clusters( terms,
#'                       single_word_score = 0.4,
#'                       additional_word_score = 1,
#'                       priority_factor = 1.1  )[1]
#'
#'  # complement activation classical pathway
#'  # 4.876716
#'
#'
#' @seealso
#'  \code{\link{termSummary}}
score_word_clusters <- function( term_cluster,
                                single_word_score = 1,
                                additional_word_score = 1,   # Add to term score based on length
                                priority_factor = 1.2,       # Divided such that earlier occurrences score higher.
                                                             #      Penalty for being later in the list of terms.
                                zero_score_words = get_zero_score_words() ){
  word_clusters <- stringr::str_split( string = term_cluster, pattern = "\\s" )

  .combos <- character() # These are the combos as joined character vectors
  .combos.scores <- numeric()

  for( .cluster in word_clusters ){
    .cluster_len <- length( .cluster )
    .c_word_combos.l <- list() # These are the combos as vectors of words

    for( .combo_len in 1:.cluster_len ){
      for( i in 1:(.cluster_len - .combo_len + 1)){
        .combo.v <- .cluster[i : (i + .combo_len -1) ]
        .c_word_combos.l[[ length( .c_word_combos.l ) + 1 ]] <- .combo.v
      }
    }
    .c_word_combos.len <- structure( sapply(.c_word_combos.l, function(x) length(x[!x %in% zero_score_words]) ),
                                     names = sapply( .c_word_combos.l, function(x) paste0( x, collapse = " " ) ) )
    .c_word_combos.score <- sapply( .c_word_combos.len,
                                    FUN = function(x) ifelse(x == 0,
                                                             0,
                                                             single_word_score + ( x - 1 ) * additional_word_score  ) )

    .combos <- c( .combos, names(.c_word_combos.score) )
    .combos.scores <- c( .combos.scores, .c_word_combos.score )
  }

  # Deduplicate:
  .combos.dups <- duplicated( .combos )
  .combos <- .combos[ !.combos.dups ]
  .combos.scores <- .combos.scores[ !.combos.dups ]

  .combos.totals <- numeric()

  for( .combo in names( .combos.scores ) ){
    .combo.score <- .combos.scores[.combo]
    .combo.total <- .combo.score * sum( grepl( pattern = paste0("\\b", .combo, "\\b"), x = term_cluster, ignore.case = TRUE, perl = TRUE ) / ( priority_factor ** (0:(length(term_cluster)-1) ) ) )
    .combos.totals[[.combo]] <- .combo.total
  }
  .combos.totals[order(-.combos.totals)]
}


#' filterTerms
#'
#' @description This function preprocesses a vector of terms, removing stopwords, converting
#' to lower case, and optionally converting underscores (as found in term names) to spaces.
#'
#' @param terms A vector of terms.
#' @param .stopwords A vector of stopwords (defaulting to the value returned by the private
#' function \code{get_stopwords()}.
#' @param convert_underscores A single logical value indicating whether or not underscores
#' should be converted to spaces. (default: TRUE)
#'
#' @returns A character vector containing filtered (but not summarized) terms.
#' @export
#'
#' @examples
#' library( GSNA )
#' unfiltered_terms <- c("GOBP_COMPLEMENT_ACTIVATION", "REACTOME_COMPLEMENT_CASCADE",
#'          "GOMF_COMPLEMENT_BINDING", "WP_COMPLEMENT_ACTIVATION_CLASSICAL_PATHWAY",
#'          "GOBP_COMPLEMENT_ACTIVATION_CLASSICAL_PATHWAY", "BIOCARTA_LECTIN_PATHWAY",
#'          "GOMF_OPSONIN_BINDING", "GOCC_GOLGI_LUMEN" )
#' filterTerms( unfiltered_terms )
#'
filterTerms <- function( terms, .stopwords = get_stopwords(), convert_underscores = TRUE ){
  terms.orig <- terms
  terms <- tolower( terms )

  if( convert_underscores )
    terms <- gsub( pattern = "_", replacement = " ", terms )

  # Stopword filter:
  if( ! is.null( .stopwords ) )
    terms <- sapply( X = terms,
                     function(x){
                       .x <- unlist( .yass_tokenizer(x) )
                       .x <- .x[! .x %in% .stopwords]
                       paste0(.x, collapse = "")
                     }
    )

  structure( stringr::str_squish( terms ),
             names = terms.orig )
}

#' termSummary
#'
#' @description This is a utility function that takes a character vector of terms
#' (generally the names of gene signatures) and optionally a grouping vector and
#' returns a vector of best guesses for the summary terms best matching the input
#' terms. The function works by applying a scoring algorithm that weights terms
#' according to the number of words, priority in the supplied list of terms, and
#' the number of times a word cluster is found in the list.
#'
#' @param terms A character vector containing terms, specifically the names of
#' gene sets. If a \code{group} vector is supplied, then it is used to group terms
#' into term clusters. If no \code{group} vector is supplied, the function treats
#' each element of the character vector as a group, and splits on internal commas
#' (or an arbitrary character specified using the argument \code{sep}) for each
#' term cluster.
#' @param group An optional vector for grouping terms into term clusters.
#' @param sep Character to dilineate terms in a term cluster. (default ",")
#' @param convert_underscores Logical value telling the function whether or not to
#' convert underscores to spaces. (default TRUE)
#' @param single_word_score The numeric base score awarded for the first word of a
#' term cluster (default 1).
#' @param additional_word_score The numeric base score awarded for subsequent words
#' of a term cluster (default 1).
#' @param priority_factor Numeric value determining the the score advantage due
#' to priority. Successive occurrences of a term summary in a vector of terms
#' have their scores divided by this number, so terms later in the list are
#' increasingly penalized (default 1.2)
#' @param .stopwords A vector of stopwords to filter out of terms (defaults to the
#' output value of the private function get_stopwords().)
#' @param .zero_score_words A vector of score free words, words that do not
#' contribute to term scores. (defaults to the output value of the private
#' function get_zero_score_words().)
#' @param num_combine The number of (best) term summaries to combine for each term
#' cluster. (default 1).
#' @param make_unique Logical value that tells the function to make output term
#' summaries unique. (default: TRUE)
#'
#' @returns A vector of summarized terms for each group.
#' @export
#'
#' @examples
#'
#' library( 'GSNA' )
#' .terms = c( "GOBP_REGULATION_OF_NATURAL_KILLER_CELL_ACTIVATION", # 1...
#'             "GOBP_POSITIVE_REGULATION_OF_NATURAL_KILLER_CELL_ACTIVATION",
#'             "GOBP_NATURAL_KILLER_CELL_ACTIVATION",
#'             "GOBP_POSITIVE_REGULATION_OF_NATURAL_KILLER_CELL_PROLIFERATION",
#'             "GOBP_ANTIBODY_DEPENDENT_CELLULAR_CYTOTOXICITY",
#'             "GOBP_TYPE_II_HYPERSENSITIVITY",
#'             "GOBP_NATURAL_KILLER_CELL_PROLIFERATION",
#'             "GOBP_MYELOID_LEUKOCYTE_ACTIVATION",                 # 2...
#'             "GOBP_MYELOID_LEUKOCYTE_MEDIATED_IMMUNITY",
#'             "GOBP_NATURAL_KILLER_CELL_ACTIVATION_INVOLVED_IN_IMMUNE_RESPONSE",
#'             "GOBP_MYELOID_CELL_ACTIVATION_INVOLVED_IN_IMMUNE_RESPONSE",
#'             "GOBP_LEUKOCYTE_DEGRANULATION",
#'             "GOBP_NATURAL_KILLER_CELL_DEGRANULATION",
#'             "GOBP_PHAGOCYTOSIS",
#'             "GOBP_REGULATED_EXOCYTOSIS" )
#' .groups = c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)
#' termSummary( terms = .terms, group = .groups )
#'
#' # 1                                     2
#' # "natural killer cell" "cell activation involved immune response"
#'
termSummary <- function( terms,                      # Either a vector of gene signature terms, possibly delimited by sep.
                         group=NULL,                 # Either NULL or a vector of subnet/gene set cluster identifiers.
                         #   If a vector of subnet identifiers, this is used to group the
                         #   values in the terms vector.
                         sep = ",",
                         convert_underscores = TRUE,
                         single_word_score = 1,
                         additional_word_score = 1,  # Add to term score based on length
                         priority_factor = 1.2,      # Divided such that earlier occurrences rated higher.
                         #      Penalty for being later in the list of terms.
                         .stopwords = get_stopwords(),
                         .zero_score_words = get_zero_score_words(),
                         num_combine = 1,
                         make_unique = TRUE
){
  terms <- filterTerms( terms = terms, .stopwords = .stopwords, convert_underscores = convert_underscores )

  if( is.null( group ) ){
    terms.ll <- stringr::str_split( string = terms, pattern = sep )
    terms.ll <- lapply( X = terms.ll, FUN = stringr::str_squish )
  } else {
    terms.ll <- split( x = terms, f = group )
  }
  .summary_terms <-
    lapply( terms.ll,
            function(x) score_word_clusters(term_cluster = x,
                                           single_word_score,
                                           additional_word_score,
                                           priority_factor,
                                           .zero_score_words ) )

  .out <- sapply( .summary_terms,
                  FUN = function( x ){
                    paste0( utils::head( names(x), n = num_combine ), collapse = ", ")
                  })
  if( make_unique ){
    .out <- structure( base::make.unique( names = .out ), names = names(.out) )
  }
  .out
}
