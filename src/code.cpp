#include <Rcpp.h>
using namespace Rcpp;

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

//' lfisher_cpp
//'
//' @description Takes a four integers corresponding to a 2x2 contingency matrix and calculates a natural-log transformed
//' Fisher *p*-value.
//'
//' @param a (required) This corresponds to cell 1 of the contingency matrix. In the context of GSNA, assuming two gene
//' sets, this is used as the number of observable genes in the background that are not present either gene set.
//'
//' @param b (required) This corresponds to cell 2 of the contingency matrix. For GSNA this is the number of observable
//' genes present in gene set 1, but not gene set 2.
//'
//' @param c (required) This corresponds to cell 3 of the contingency matrix. For GSNA this is the number of observable
//' genes present in gene set 2, but not gene set 1.
//'
//' @param d (required) This corresponds to cell 4 of the contingency matrix. For GSNA this is the number of observable
//' genes present in both gene sets 1 and 2.
//'
//' @param e_precision (optional) Numeric value that determines the precision of summation of partial *p*-values. For
//' the less-than, greater-than and two-sided options in calculating the log-Fisher *p*-value, log-space summation of
//' partial *p*-values must be accomplished using the so-called *Log-Sum-Exponent* trick. Due to limitations of precision
//' in C++, however, numbers that differ by more than about 11 powers of *e* cannot be summed. By specifying a value less
//' than 12, slightly less precise p-values can be calculated slightly faster. This option was included as way to
//' accelerate calculation of *p*-values, but has not proven to significantly improve performance, so it may be removed
//' in the future. Defaults to 12.
//'
//' @param alternative (optional) Integer corresponding to 4 options:
//' \describe{
//'   \item{\code{1}}{single-sided, greater-than. Sums *p*-values for intersections greater than and equal to \code{d}.}
//'   \item{\code{2}}{single-sided, less-than. Sums *p*-values for intersections less than and equal to \code{d}.}
//'   \item{\code{3}}{two-sided, sums all partial *p*-values less than or equal to the partial *p*-value for intersections
//'      equal to \code{d}.}
//'   \item{\code{4}}{partial. Calculates single *p*-value for intersections equal to \code{d}.}
//'   }
//'
//' @return This function returns a numeric (double in C++) natural log-Fisher p-value.
//'
//' @export
//'
//' @details Calculation of Fisher *p*-values is discussed in detail elsewhere, but partial natural-log
//'  transformed *p*-values are calculated as follows:
//'
//'  Given a 2x2 contingency matrix of the form:
//'
//'  \deqn{\biggl[\begin{matrix}a & b \\ c & d\end{matrix}\biggr]}
//'
//' The natural log of the partial *p*-values is given by:
//'
//' \deqn{ln(p)  = ln((a+b)!) + ln((c+d)!) +
//'                ln((a+c)!) + ln((b+d)!) -
//'                ln(a!) - ln(b!) - ln(c!) -
//'                ln(d!) - ln((a + b + c + d)!)}
//'
//'
//' For the single and two-tailed alternatives, partial *p*-values are summed using the so-called 'log-sum-exponent' method.
//'
//' @examples
//'
//' library(GSNA)
//'
//' # Calculate a single natural log Fisher_p value:
//' log_fisher_p <- lfisher_cpp( a = 16000,
//'                              b = 200,
//'                              c = 170,
//'                              d = 100,
//'                              alternative = 3 )
//'
//'
//' @seealso
//'  \code{\link{gsIntersectCounts}}
//'  \code{\link{scoreLFMatrix_C}}
//'
// [[Rcpp::export]]
double lfisher_cpp( int a, int b, int c, int d,
                    double e_precision = 12.0,
                    int alternative = 1

){
  int ab_ = a + b;
  int ac_ = a + c;
  int bd_ = b + d;
  int cd_ = c + d;
  int abcd_ = ab_ + cd_;
  int starting_d_ = d;
  int d_max = std::min( bd_, cd_ );
  int d_min = std::max( d - a, 0 );

  /* When calculating the Fisher p-value by summing a series of values, a portion of the summation remains constant:
   * p_partial = lfactorial(ab_) + lfactorial(cd_) + lfactorial(ac_) + lfactorial(bd_) -  # CONSTANT
   *             lfactorial(a) - lfactorial(b) - lfactorkal(c) - lfactorial(d) -          # CHANGES
   *             lfactorial(abcd_)                                                        # CONSTANT
   *
   * We will calculate the constant portion ahead:
   */
  double l_constant = std::lgamma( ab_ + 1 )  + std::lgamma( cd_ + 1 ) +
    std::lgamma( ac_ + 1 ) + std::lgamma( bd_ + 1 ) -
    std::lgamma( abcd_ + 1 );

  double lf2s = 1;

  if( alternative == 1 ){ // alternative: greater than
    for( int d_ = starting_d_; d_ <= d_max; d_++ ){
      int a_ = ab_ - bd_ + d_;
      int b_ = bd_ - d_;
      int c_ = cd_ - d_;

      double part_lp = l_constant -
        std::lgamma( a_ + 1 ) - std::lgamma( b_ + 1 ) - std::lgamma( c_ + 1 ) - std::lgamma( d_ + 1 );

      if ( lf2s > 0 ){
        lf2s = part_lp;
        //maxPartial = part_lp;
      } else if ( lf2s - part_lp > e_precision + 0.69 + std::log(d_max - d_ + 1) ){ //Heuristic that stops summing for small values of part_lp
        continue;                                                // (precision in powers of e) + log(2) + log(number of remaining terms to sum)
      } else {
        //R-function to do this: lse<- function(a,b){ a + log( 1 + exp( b - a ) ) }
        lf2s = lf2s + std::log( 1 + std::exp( part_lp - lf2s ) );
      }
    }
  } else if( alternative == 2 ){ // alternative: less than
    // lf2s = std::log( 1 - std::exp( l2fs ) ) // While numerically correct, this is not as precise as summing the bottom tail.
    for( int d_ = starting_d_; d_ >= d_min; d_-- ){
      int a_ = ab_ - bd_ + d_;
      int b_ = bd_ - d_;
      int c_ = cd_ - d_;

      double part_lp = l_constant -
        std::lgamma( a_ + 1 ) - std::lgamma( b_ + 1 ) - std::lgamma( c_ + 1 ) - std::lgamma( d_ + 1 );

      if ( lf2s > 0 ){
        lf2s = part_lp;
        //maxPartial = part_lp;
      } else if ( lf2s - part_lp > e_precision + 0.69 + std::log(d_max - d_ + 1) ){ //Heuristic that stops summing for small values of part_lp
        break;                                                // (precision in powers of e) + log(2) + log(number of remaining terms to sum)
      } else {
        //R-function to do this: lse<- function(a,b){ a + log( 1 + exp( b - a ) ) }
        lf2s = lf2s + std::log( 1 + std::exp( part_lp - lf2s ) );
      }
    }
  } else if( alternative == 3 ){  // alternative: two-sided
    // Max Partial is the partial p-value represented by the contingency table.
    double maxPartial = l_constant -
      std::lgamma( ab_ - bd_ + starting_d_ + 1 ) - std::lgamma( bd_ - starting_d_ + 1 ) -
      std::lgamma( cd_ - starting_d_ + 1 ) - std::lgamma( starting_d_ + 1 );

    // Sum all the more extreme tables.
    for( int d_ = d_min; d_ <= d_max; d_++ ){
      double part_lp = l_constant -
        std::lgamma( ab_ - bd_ + d_ + 1 ) -   //a
        std::lgamma( bd_ - d_ + 1 ) -         //b
        std::lgamma( cd_ - d_ + 1 ) -         //c
        std::lgamma( d_ + 1 );                //d
      if( part_lp > maxPartial ){
        continue;
      }
      if ( lf2s > 0 ){
        lf2s = part_lp;
        //maxPartial = part_lp;
      } else if ( lf2s - part_lp > e_precision + 0.69 + std::log(d_max - d_ + 1) ){ //Heuristic that stops summing for small values of part_lp
        continue;                               // (precision in powers of e) + log(2) + log(number of remaining terms to sum)
      } else {
        //R-function to do this: lse<- function(a,b){ a + log( 1 + exp( b - a ) ) }
        lf2s = lf2s + std::log( 1 + std::exp( part_lp - lf2s ) );
      }
    }
  } else if ( alternative == 4 ) { // alternative = partial
    lf2s = l_constant -
      std::lgamma( ab_ - bd_ + starting_d_ + 1 ) - //a
      std::lgamma( bd_ - starting_d_ + 1 ) -       //b
      std::lgamma( cd_ - starting_d_ + 1 ) -       //c
      std::lgamma( starting_d_ + 1 );              //d
  } else {
    throw "Invalid value for argument alternative.";
  }

  return lf2s;
}






//' scoreJaccardMatrix_C
//'
//' @description Takes a presence/absence matrix with genes as the rows and modules as columns and calculates
//' a matrix of Jaccard index values.
//'
//' @param geneSetCollection_m (required) A logical presence/absence matrix representation of a gene set collection
//' in which columns correspond to gene sets, rows correspond to genes and values are \code{TRUE} if a gene is present
//' in a gene set and \code{FALSE} otherwise. Row and column names correspond to gene symbols and gene set
//' identifiers, respectively. NOTE: for a typical GSNA analysis, this matrix would include only observed filtered
//' genes and significant gene set hits from pathways analysis. Using a matrix version of the full MSigDB without filtering
//' genes, for example, would likely be unworkably slow and memory intensive.
//'
//' @return This function returns a matrix of Jaccard index values between gene modules. Values on the diagonal
//' corresponding to self-Jaccard indices are returned as NA.
//'
//' @export
//'
//' @details The Jaccard index J for two sets A and B is defined as:
//'
//' \deqn{ J(A,B) = \dfrac{\lvert A \cap B \rvert}{\lvert A \cup B \rvert} }
//'
//'
//' @examples
//'
//' library(GSNA)
//'
//' # Get the background of observable genes set from
//' # expression data:
//' gene_background <- toupper(rownames( Bai_empty_expr_mat ))
//'
//' # Using the sample gene set collection **Bai_gsc.tmod**,
//' # generate a gene presence-absence matrix filtered for the
//' # ref.background of observable genes:
//' presence_absence.mat <-
//'  makeFilteredGenePresenceAbsenceMatrix( ref.background = gene_background,
//'                                         geneSetCollection = Bai_gsc.tmod )
//'
//' jaccard.mat <- scoreJaccardMatrix_C( presence_absence.mat )
//'
//' @seealso
//'  \code{\link{buildGeneSetNetworkJaccard}()}
//'  \code{\link{scoreLFMatrix_C}()}
//'
//'  @import Rcpp
//'
// [[Rcpp::export]]
SEXP scoreJaccardMatrix_C( SEXP geneSetCollection_m ){
  //  scoreJaccardMatrix_C <- inline::rcpp(signature( geneSetCollection_m ="integer"), body='
  Rcpp::LogicalMatrix GLCF(geneSetCollection_m);
  Rcpp::NumericMatrix JI(GLCF.ncol(), GLCF.ncol());
  int bg_size = GLCF.nrow();
  int n_modules = GLCF.ncol();

  for (int j=0; j<n_modules; j++){
    for (int i=0; i<n_modules; i++){
      JI(j,i) = NA_REAL;
    }
  }

  for (int j=0; j<n_modules; j++){
    for (int i=j+1; i<n_modules; i++){
      int set_iIj = 0;
      int set_iUj = 0;

      for( int k=0; k<bg_size; k++){
        if( GLCF(k,i) == TRUE && GLCF(k,j) == TRUE )
          set_iIj++;
        if( GLCF(k,i) == TRUE || GLCF(k,j) == TRUE )
          set_iUj++;
      }

      float jaccard_ = (float)set_iIj / (float)set_iUj;

      JI(j,i) = jaccard_;
      JI(i,j) = jaccard_;
    }
  }
  JI.attr("lower_is_closer") = LogicalVector::create( FALSE );
  JI.attr("distance") = CharacterVector::create( "jaccard" );
  JI.attr("distance_type") = CharacterVector::create( "similarity" );
  rownames( JI ) = colnames(GLCF);
  colnames( JI ) = colnames(GLCF);
  return(JI);
}









//' scoreOCMatrix_C
//'
//' @description Takes a presence/absence matrix with genes as the rows and modules as columns and calculates
//' a matrix of overlap coefficient values (also known as Szymkiewicz–Simpson coefficients^1).
//'
//' @param geneSetCollection_m (required) A logical presence/absence matrix representation of a gene set collection
//' in which columns correspond to gene sets, rows correspond to genes and values are \code{TRUE} if a gene is present
//' in a gene set and \code{FALSE} otherwise. Row and column names correspond to gene symbols and gene set
//' identifiers, respectively. NOTE: for a typical GSNA analysis, this matrix would include only observed filtered
//' genes and significant gene set hits from pathways analysis. Using a matrix version of the full MSigDB without filtering
//' genes, for example, would likely be unworkably slow and memory intensive.
//'
//' @return This function returns a matrix of overlap coefficient values between gene modules. Values on the diagonal
//' corresponding to self-overlap coefficients are returned as NA.
//'
//' @export
//'
//' @details The overlap (or Szymkiewicz–Simpson) coefficient for two sets A and B is defined as:
//'
//' \deqn{ OC(A,B) = \dfrac{\lvert A \cap B \rvert}{min(\lvert A \rvert, \lvert B \rvert)} }
//'
//'
//' @examples
//'
//' library(GSNA)
//'
//' # Get the background of observable genes set from
//' # expression data:
//' gene_background <- toupper(rownames( Bai_empty_expr_mat ))
//'
//' # Using the sample gene set collection **Bai_gsc.tmod**,
//' # generate a gene presence-absence matrix filtered for the
//' # ref.background of observable genes:
//' presence_absence.mat <-
//'   makeFilteredGenePresenceAbsenceMatrix( ref.background = gene_background,
//'                                          geneSetCollection = Bai_gsc.tmod )
//'
//' # Now generate an overlap coefficient matrix.
//' oc.mat <- scoreOCMatrix_C( presence_absence.mat )
//'
//'
//' @seealso
//'  \code{\link{buildGeneSetNetworkOC}}
//'  \code{\link{scoreLFMatrix_C}}
//'
//' @references 1.  M.K V, K K. A Survey on Similarity Measures in Text Mining. MLAIJ. 2016;3: 19–28. doi:10.5121/mlaij.2016.3103
//'
//'  @import Rcpp
//'
// [[Rcpp::export]]
SEXP scoreOCMatrix_C( SEXP geneSetCollection_m ){
  //  scoreJaccardMatrix_C <- inline::rcpp(signature( geneSetCollection_m ="integer"), body='
  Rcpp::LogicalMatrix GLCF(geneSetCollection_m);
  Rcpp::NumericMatrix OC(GLCF.ncol(), GLCF.ncol());
  int bg_size = GLCF.nrow();
  int n_modules = GLCF.ncol();

  for (int j=0; j<n_modules; j++){
    for (int i=0; i<n_modules; i++){
      OC(j,i) = NA_REAL;
    }
  }

  for (int j=0; j<n_modules; j++){
    for (int i=j+1; i<n_modules; i++){
      int set_iIj = 0;
      int set_i = 0;
      int set_j = 0;

      for( int k=0; k<bg_size; k++){
        if( GLCF(k,i) == TRUE && GLCF(k,j) == TRUE )
          set_iIj++;
        if( GLCF(k,i) == TRUE )
          set_i++;
        if( GLCF(k,j) == TRUE )
          set_j++;
      }
      float oc_ = 0;

      if( set_i <= set_j )
        oc_ = (float)set_iIj / (float)set_i;
      else
        oc_ = (float)set_iIj / (float)set_j;

      OC(j,i) = oc_;
      OC(i,j) = oc_;
    }
  }
  OC.attr("lower_is_closer") = LogicalVector::create( FALSE );
  OC.attr("distance") = CharacterVector::create( "oc" );
  OC.attr("distance_type") = CharacterVector::create( "similarity" );
  rownames( OC ) = colnames(GLCF);
  colnames( OC ) = colnames(GLCF);
  return(OC);
}






//




//' gsIntersect
//'
//' @description For two character vectors, returns the set of shared elements. This is used by GSNA to find shared genes
//' in two gene sets.
//'
//' @param gs1 A character vector representing gene symbols in a gene set.
//' @param gs2 A character vector representing gene symbols in a second gene set.
//'
//' @return A character vector consisting of the overlap of the two gene sets.
//'
//' @details This version of the function is used in gsnORAtest_cpp. (In another version of the function, used in
//' \code{gsnFilterGeneSetCollectionList()} and accessible only from C++ the first argument is gs1Set, a set of strings
//' of type \code{std::set<std::string>}.)
//'
//' This function does essentially what R's base::intersect does, so it is not necessarily useful to export.
//'
//' @examples
//'
//' library(GSNA)
//'
//' # We can extract 2 gene sets from the sample data:
//' Bai.gsc <- tmod2gsc( Bai_gsc.tmod )
//' M29994.gs = Bai.gsc[['M29994']]
//' M40825.gs = Bai.gsc[['M40825']]
//'
//' # Find the intersection:
//' intersect.gs <- gsIntersect( gs1 = M29994.gs, gs2 = M40825.gs )
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::CharacterVector gsIntersect( Rcpp::CharacterVector gs1, Rcpp::CharacterVector gs2 ){
  gs1 = unique( gs1 );
  gs2 = unique( gs2 );

  int gs1_len = gs1.length();
  int gs2_len = gs2.length();

  Rcpp::CharacterVector gs_filt = Rcpp::CharacterVector::create();
  for(int j=0; j<gs1_len; j++ ){  // Genes in gs1
    for( int k=0; k<gs2_len; k++ ){ // Genes in gs2
      if( std::strcmp( gs1[j], gs2[k] ) == 0 ){
        gs_filt.push_back( gs1[j] );
      }
    }
  }
  return gs_filt;
}


// This version of gsIntersect, used by gsnFilterGeneSetCollectionList(), takes a std::set<std::string> and
// a Rcpp::CharacterVector as arguments and returns a character vector containing the intersection.
Rcpp::CharacterVector gsIntersect( std::set<std::string> gs1Set,  Rcpp::CharacterVector gs2 ){
  gs2 = unique( gs2 );
  Rcpp::CharacterVector gs_filt = Rcpp::CharacterVector::create();
  for( Rcpp::CharacterVector::iterator j= gs2.begin(); j != gs2.end(); j++ ){  // Genes in gs2
    if( gs1Set.find( as<std::string>(*j) ) != gs1Set.end() ){
      gs_filt.push_back( as<std::string>(*j) );
    }
  }
  return gs_filt;
}

// This function takes a character vector and makes a set. This is used by gsnFilterGeneSetCollectionList.
// (We are converting a character vector into a set because finding elements in a set is much faster than
// with a CharacterVector, so shared elements can be collected or counted much faster this way.)
std::set<std::string> gsMakeSet( Rcpp::CharacterVector gs ){
  std::set<std::string> gsSet;
  for( Rcpp::CharacterVector::iterator i = gs.begin(); i != gs.end(); i++ ){
    std::string geneName = as<std::string>(*i);
    gsSet.insert( geneName );
  }
  if( gsSet.find( as<std::string>(NA_STRING) ) != gsSet.end() ){
    gsSet.erase( as<std::string>(NA_STRING) );
  }
  return gsSet;
}




//' gsIntersectCounts
//'
//' @description For two character vectors representing two gene sets (gs1 and gs2) and a total number of background
//' observable genes (that may also be present in gs1 and or gs2 or neither), this function calculates the counts in a
//' 2x2 contingency table for presence and absence of genes in one or both sets or neither. The output of this function
//' is used as the input for a Fisher test calculation by the GSNA package.
//'
//' @param gs1 A character vector representing gene symbols in a gene set.
//' @param gs2 A character vector representing gene symbols in a second gene set.
//' @param bg_size An integer representing the size of the background, i.e. the total number of observable genes.
//'
//' @return A numeric vector of length 4 containing the following 4 elements:
//' \itemize{
//'    \item{\code{a}: The number of genes in the background that are absent in gs1 and gs2.}
//'    \item{\code{b}: The number of background genes in gs1 but not gs2.}
//'    \item{\code{c}: The number of background genes in gs2 but not gs1.}
//'    \item{\code{d}: The number of background genes in in both gs1 and gs2.}
//'}
//' @details This version of the function may not be retained since it's not currently used. Two alternative versions of the
//' function in C++ that find the overlap between a \code{std::set<std::string>} and a character vector are used since those versions
//' are much faster.
//'
//' NOTE: This function assumes that all genes in gs1 and gs2 are present in the background, so to use this properly, gs1
//' and gs2 must be filtered to include only genes present in the background.
//'
//' @examples
//'
//' library( GSNA )
//'
//' # We can extract 2 gene sets from the sample data:
//' Bai.gsc <- tmod2gsc( Bai_gsc.tmod )
//' M29994.gs = Bai.gsc[['M29994']]
//' M40825.gs = Bai.gsc[['M40825']]
//'
//' # Get background gene cout:
//' bg_gene_count <- nrow( Bai_empty_expr_mat )
//'
//' # Generate a vector containing the number of contents of
//' # the 2x2 contingency table:
//' counts.v <- gsIntersectCounts( gs1 = M29994.gs,
//'                                gs2 = M40825.gs,
//'                                bg_size = bg_gene_count )
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericVector gsIntersectCounts( Rcpp::CharacterVector gs1, Rcpp::CharacterVector gs2, int bg_size ){
  gs1 = unique( gs1 );
  gs2 = unique( gs2 );

  int gs1_len = gs1.length();
  int gs2_len = gs2.length();

  int intersect_count = 0;

  Rcpp::CharacterVector gs_filt = Rcpp::CharacterVector::create();
  for(int j=0; j<gs1_len; j++ ){  // Genes in gs1
    for( int k=0; k<gs2_len; k++ ){ // Genes in gs2
      if( std::strcmp( gs1[j], gs2[k] ) == 0 ){
        intersect_count++;
      }
    }
  }
  return Rcpp::NumericVector::create(
    Rcpp::Named( "a" ) = bg_size - gs1_len - gs2_len + intersect_count,
    Rcpp::Named( "b" ) = gs1_len - intersect_count,
    Rcpp::Named( "c" ) = gs2_len - intersect_count,
    Rcpp::Named( "d" ) = intersect_count
  );
}

// Essentially the same as gsIntersectCounts( Rcpp::CharacterVector gs1, Rcpp::CharacterVector gs2, int bg_size ),
// except that instead of two CharacterVectors, one is a std::set<string> which is much faster to search.
Rcpp::NumericVector gsIntersectCounts( std::set<std::string> gs1Set, Rcpp::CharacterVector gs2, int bg_size ){
  gs2 = unique( gs2 );
  int gs1_len = gs1Set.size(); // Ordered set has no duplicates.
  int gs2_len = gs2.length();

  int intersect_count = 0;
  //Rcpp::CharacterVector gs_filt = Rcpp::CharacterVector::create();
  for( Rcpp::CharacterVector::iterator j= gs2.begin(); j != gs2.end(); j++ ){  // Genes in gs2
    if( gs1Set.find( as<std::string>(*j) ) != gs1Set.end() ){
      intersect_count++;
    }
  }

  return Rcpp::NumericVector::create(
    Rcpp::Named( "a" ) = bg_size - gs1_len - gs2_len + intersect_count,
    Rcpp::Named( "b" ) = gs1_len - intersect_count,
    Rcpp::Named( "c" ) = gs2_len - intersect_count,
    Rcpp::Named( "d" ) = intersect_count
  );
}

/* This is the same as gsIntersectCounts( std::set<std::string> gs1Set, Rcpp::CharacterVector gs2, int bg_size )
 * except the inputs and output vector are reordered.
 */
Rcpp::NumericVector gsIntersectCounts( Rcpp::CharacterVector gs1, std::set<std::string> gs2Set, int bg_size ){
  Rcpp::NumericVector countz =  gsIntersectCounts( gs2Set, gs1, bg_size );

  return Rcpp::NumericVector::create(
    Rcpp::Named( "a" ) = countz[0],
    Rcpp::Named( "b" ) = countz[2],
    Rcpp::Named( "c" ) = countz[1],
    Rcpp::Named( "d" ) = countz[3]
  );
}







//' gsnFilterGeneSetCollectionList
//'
//' @description Given a vector of gene symbols and a gene set collection, filter the gene set collection to include
//' only gene symbols present in the background.
//'
//' @param bg A character vector representing gene symbols in a background of observable genes.
//'
//' @param geneSetCollection A list of gene sets, in which the gene sets are character vectors containing
//' gene symbols, and the list names are the corresponding gene set identifiers. NOTE: This must be a list, not a
//' \code{tmod} object. It is trivial to extract such a list from a \code{tmod} object, however. The
//' \code{$MODULES2GENES} field of the \code{tmod} object contains a suitable list.
//'
//' @return A filtered gene set as a list of vectors of gene symbols in which the list names correspond to gene
//' set IDs.
//'
//' @details This function is used in gsnORAtest_cpp to automatically filter the gene set provided. It may be used
//' manually during GSNA analysis.
//'
//' @examples
//'
//' library(GSNA)
//'
//' # Get the background of observable genes set from
//' # expression data:
//' gene_background <- toupper(rownames( Bai_empty_expr_mat ))
//'
//' # Generate a gene set collection as a list of vectors from
//' # **Bai_gsc.tmod**, included in sample data:
//' Bai.gsc <- tmod2gsc( Bai_gsc.tmod )
//'
//' # Using the sample gene set collection **Bai_gsc.tmod**,
//' # generate a gene set collection filtered for the bg of
//' # observable genes:
//' Bai.filt.gsc <- gsnFilterGeneSetCollectionList( bg = gene_background,
//'                                          geneSetCollection = Bai.gsc )
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::List gsnFilterGeneSetCollectionList( Rcpp::CharacterVector bg,
                                           Rcpp::List geneSetCollection ){
  Rcpp::List gsc_filt = Rcpp::List::create();
  Rcpp::CharacterVector gsc_names = geneSetCollection.names();
  Rcpp::CharacterVector gsc_names_filt = Rcpp::CharacterVector::create();

  std::set<std::string> bgSet = gsMakeSet( bg );

  // Filter the geneSetCollection to retain only genes in bg;
  for( Rcpp::List::iterator i = geneSetCollection.begin(); i != geneSetCollection.end(); i++ ){
    Rcpp::CharacterVector gs_filt = gsIntersect( bgSet, (Rcpp::CharacterVector)*i );
    if( gs_filt.length() > 0 ){
      gsc_filt.push_back( gs_filt );
      gsc_names_filt.push_back( gsc_names[i - geneSetCollection.begin()] );
    }
  }
  gsc_filt.attr("names") = gsc_names_filt;
  return gsc_filt;
}





//' gsnORAtest_cpp
//'
//' @description This function performs ORA analysis and returns a data.frame containing various statistics including
//' fold enrichment, and 1 and 2-tailed p-values. (see details)
//'
//' @param l (required) A character vector containing a list of gene identifiers. These are generally differentially
//' expressed genes either genes significantly up or significantly down, but they can also be a list of genes that came
//' out of a genetic screen, gene loci with differential chromatin accessibility generated by ATAC-Seq data, lists of genes
//' from GWAS, etc. The order of the genes is unimportant.
//'
//' @param bg (required) A character vector containing a list of gene identifiers corresponding to the total background of
//' observable genes.
//'
//' @param geneSetCollection (required) A list of gene sets, in which the gene sets are character vectors containing
//' gene symbols, and the list names are the corresponding gene set identifiers. NOTE: This must be a list, not a
//' \code{tmod} object. It is trivial to extract such a list from a \code{tmod} object, however. The
//' \code{$MODULES2GENES} field of the \code{tmod} object contains a suitable list.
//'
//' @return A data frame containing the results of overrepresentation analysis.
//' \itemize{
//'   \item{*ID*: the gene set identifiers.}
//'   \item{*a*: the number of genes observed in the background but not in *l* or the queried gene set.}
//'   \item{*b*: the number of observed genes in *l* but not the queried gene set.}
//'   \item{*c*: the number of observed genes in the queried gene set but not *l* and}
//'   \item{*d*: the number of observed genes in both *l* and the queried gene set, i.e. the overlap.}
//'   \item{*N*: the number of observed genes the queried gene set.}
//'   \item{*Enrichment*: The fold overrepresentation of genes in the overlap set *d* calculated as:
//'       \deqn{E = (d / (c+d)) / ((b+d)/(a+b+c+d))}
//'        }
//'   \item{*P_2S*: 2-sided Fisher *p*-value. (*NOT* log-transformed.)}
//'   \item{*P_1S*: 1-sided Fisher *p*-value. (*NOT* log-transformed.)}
//' }
//'
//' @details This is the main workhorse function for the ORA test in the \code{GSNA} package, however, it performs
//' no filtering of the output data set, nor *p*-value adjustment, and most users of the package will want to use
//' \code{gsnORAtest()} function instead, which calculates adjusted *p*-values, filters the output data for
//' significance, and can include a \code{Title} field in the output data.frame.
//'
//' @seealso \code{\link{gsnORAtest}}
//'
//' @examples
//'
//' library(GSNA)
//'
//' # From a differential expression data set, we can generate a
//' # subset of genes with significant differential expression,
//' # up or down. Here we will extract genes with significant
//' # negative differential expression with
//' # avg_log2FC < 0 and p_val_adj <= 0.05 from **Seurat** data:
//'
//' sig_DN.genes <-
//'    toupper( rownames(subset( Bai_CiHep_v_Fib2.de,
//'                       avg_log2FC < 0  & p_val_adj < 0.05 )) )
//'
//' # Using all the genes in the differential expression data set,
//' # we can obtain a suitable background:
//' bg <- rownames( Bai_CiHep_v_Fib2.de )
//'
//' # Next we need a gene set collection in the form of a list of
//' # character vectors. We can convert the **Bai_gsc.tmod** object
//' # included in the sample data to such a list:
//' Bai.gsc <- tmod2gsc( Bai_gsc.tmod )
//'
//' # Now, we can do a overrepresentation analysis search on this
//' # data using **Bai.gsc**:
//' sig_DN.gsnora <- gsnORAtest_cpp( l = sig_DN.genes,
//'                                  bg = bg,
//'                                  geneSetCollection = Bai.gsc )
//'
//' @export
//'
//'
// [[Rcpp::export]]
SEXP gsnORAtest_cpp( Rcpp::CharacterVector l,
                     Rcpp::CharacterVector bg,
                     Rcpp::List geneSetCollection ){

  // Handle duplicates in bg
  int bg_len0 = bg.length();
  bg = unique( bg );
  int bg_len1 = bg.length();
  if( bg_len1 < bg_len0 ){
    warning( "Duplicates in background 'bg' removed.\n" );
  }

  // Handle duplicates in l and genes not in bg
  int l_len0 = l.length();
  l = unique( l );
  int l_len1 = l.length();
  if( l_len1 < l_len0 ){
    warning( "Duplicates in gene query set 'l' removed.\n" );
  }
  l = gsIntersect( l, bg );
  int l_len2 = l.length();
  if( l_len2 < l_len1 ){
    warning( "Genes in gene list 'l' but not in background 'bg' have been removed.\n" );
  }
  // Create lSet ordered set object for faster search.
  std::set<std::string> lSet = gsMakeSet(l);

  // Filter geneSetCollection for genes in bg
  Rcpp::List gsc_filt = gsnFilterGeneSetCollectionList( bg, geneSetCollection );

  // length of gene set collection
  int gsc_len = gsc_filt.length();

  // Set up data vectors for output data.frame. Setting up the size of the vectors ahead of time should make access faster.
  Rcpp::CharacterVector gsc_filt_names = gsc_filt.names();
  Rcpp::IntegerVector a (gsc_len, NA_INTEGER);
  Rcpp::IntegerVector b (gsc_len, NA_INTEGER);
  Rcpp::IntegerVector c (gsc_len, NA_INTEGER);
  Rcpp::IntegerVector d (gsc_len, NA_INTEGER);
  Rcpp::IntegerVector N (gsc_len, NA_INTEGER);
  Rcpp::NumericVector Enrichment (gsc_len, NA_REAL);
  Rcpp::NumericVector P_2S (gsc_len, NA_REAL);
  Rcpp::NumericVector P_1S (gsc_len, NA_REAL);

  for(int i=0; i<gsc_len; i++){ // Gene sets
    //Rcpp::CharacterVector gs = geneSetCollection[i]; // Seems to be a bug
    Rcpp::CharacterVector gs = gsc_filt[i];
    Rcpp::NumericVector ic = gsIntersectCounts( gs, lSet, bg_len1 );
    a[i] = ic[0];
    b[i] = ic[1];
    c[i] = ic[2];
    d[i] = ic[3];
    N[i] = gs.length();
    Enrichment[i] = std::round( 1000 * ((double)ic[3] / (ic[2]+ic[3])) / ((double)(ic[1]+ic[3])/(ic[0]+ic[1]+ic[2]+ic[3]))) / 1000;
    P_1S[i] = std::exp( lfisher_cpp( ic[0], ic[1], ic[2], ic[3], 12.0, 1 ) );
    P_2S[i] = std::exp( lfisher_cpp( ic[0], ic[1], ic[2], ic[3], 12.0, 3 ) );
  }

  return Rcpp::DataFrame::create( Rcpp::Named( "ID" ) = gsc_filt_names,
                                  Rcpp::Named( "a" ) = a,
                                  Rcpp::Named( "b" ) = b,
                                  Rcpp::Named( "c" ) = c,
                                  Rcpp::Named( "d" ) = d,
                                  Rcpp::Named( "N" ) = N,
                                  Rcpp::Named( "Enrichment" ) = Enrichment,
                                  Rcpp::Named( "P.1S" ) = P_1S,
                                  Rcpp::Named( "P.2S" ) = P_2S
                                );
}








// As of 20220526, this is my best version of scoreLFMatrix_C in C, because it includes 4 alternatives:
//
//    1. 'upper tail' (default)
//    2. 'lower tail'
//    3. 'two tail'
//    4. 'partial'
//

//' scoreLFMatrix_C
//'
//' @description Takes a presence/absence matrix with genes as the rows and modules as columns and calculates
//' a matrix of log-transformed Fisher *p*-values.
//'
//' @usage
//'  scoreLFMatrix_C( geneSetCollection_m,
//'                   e_precision = as.numeric(c(12)),
//'                   alternative = as.integer(c(1)))
//'
//' # # NOTE: The following also works and may be preferable for
//' # # many users:
//' # scoreLFMatrix_C( geneSetCollection_m,
//' #                  e_precision = 12,
//' #                  alternative = 1 )
//'
//' @param geneSetCollection_m (required) A logical presence/absence matrix representation of a gene set collection
//' in which columns correspond to gene sets, rows correspond to genes and values are \code{TRUE} if a gene is present
//' in a gene set and \code{FALSE} otherwise. Row and column names correspond to gene symbols and gene set
//' identifiers, respectively. NOTE: for a typical GSNA analysis, this matrix would include only observed filtered
//' genes and significant gene set hits from pathways analysis. Using a matrix version of the full MSigDB without filtering
//' genes, for example, would likely be unworkably slow and memory intensive.
//'
//' @param e_precision (optional, default 12) Numeric to control the precision of the log p-value calculated.
//' Due to precision limits inherent in C++ double precision numbers, log p-values for which the corresponding
//' untransformed p-values differ by more than a certain magnitude cannot effectively be added. This feature
//' was introduced as a way to accelerate summation of p-values so as to allow summation to be cut off
//' when the acceptable level of precision had been reached, but it was found that it also seems to prevent
//' artifacts caused by arithmetic underflow.
//'
//' @param alternative (optional, default 1) An integer value specifying one of 4 alternative p-value calculations
//' where \code{1} specifies single, upper tail log Fisher p-value, \code{2} signifies single, lower-tail Fisher
//' p-value, \code{3} signifies 2-tailed Fisher p-value, and \code{4} signifies partial Fisher p-value (see below).
//'
//' @return A numerical matrix containing the specified log Fisher *p*-values for all non-self pairs. Values on the
//' diagonal (which would correspond to self-self comparison *p*-values) are NA. The \code{'lower_is_closer'}
//' attribute on the matrix is set to \code{TRUE}, except in the case of \code{alternative=2} where it is set
//' to \code{FALSE}.
//'
//' The \code{distance} attribute in the output matrix is set to \code{'stlf'} for option 1 (single, upper tail),
//' \code{'ltlf'} for option 2 (lower tail), \code{'ttlf'} for option 3 (two-tailed), and \code{'lf'} for option 4
//' (log partial Fisher *p*-value).
//'
//' @export
//'
//' @details
//'
//' Fisher *p*-values have long been used to assess the statistical significance of over- or underrepresentation
//' of a component of a mixture to assess whether a sample is drawn from a particular mixture. The test has also
//' long been used in pathways analysis as a way to assess whether an experimentally derived list of genes
//' contains a statistical overrepresentation of genes from predefined gene sets or modules. Such experimental gene
//' lists may include differentially expressed genes from a transcriptomic experiment, genes possessing promoters
//' with differential chromatin accessibility from an ATAC-Seq experiments, genes that were positive in screens of
//' mutants, genes that were identified from GWAS experiments, and genes from other analyses. Likewise, the gene
//' sets or modules are generally drawn from databases of experimentally characterized pathways, sets of genes
//' over- or under-expressed in particular conditions, or associated with particular biological processes,
//' chromosome regions, etc.
//'
//' In the case of GSNA, we use the Fisher test to assess the overlap of genes not between an experimentally
//' derived gene list and predefined gene sets from a database, but between the predefined gene sets themselves
//' given their observability in a particular experiment.
//'
//' # Implementation
//'
//' We use the Fisher test to assess the statistical significance of the overlap of two gene sets. For our purposes
//' the test determines whether two gene sets share a greater (or in some cases less) than expected number of common
//' members, assuming a null hypothesis of random membership. The two sets need not necessarily be of the same size,
//' but are for the purposes of the test assumed to have set sizes.
//'
//' Consider a 2x2 contingency matrix of the following form:
//'
//'  \deqn{\biggl[\begin{matrix}a & b \\ c & d\end{matrix}\biggr]}
//'
//' Given a background of observable genes and two gene sets, *i* and *j* that may overlap, this contingency
//' table is used to represent four numbers:
//'
//' \itemize{
//'   \item{*a*: the number of genes observed in the background but not in *i* or *j*}
//'   \item{*b*: the number of observed genes in *i* but not *j*}
//'   \item{*c*: the number of observed genes in *j* but not *i* and}
//'   \item{*d*: the number of observed genes in both *j* and *i*, i.e. the overlap.}
//' }
//'
//' The *partial*-Fisher *p*-value, signifying the likelihood of that particular contingency
//' table is given by:
//'
//' \deqn{p = \dfrac{(a + b)! (c + d)! (a + c)! (b + d)!}{a! b! c! d! (a+b+c+d)!}}
//'
//' This partial *p*-value is what is returned in the distance matrix when the argument \code{alternative = 4}
//' and it is less than, though tracks closely with, the two-tailed p-value, in most cases.
//'
//' The actual single- and two-tailed *p*-values are derived from this number by summation, keeping the sum of
//' each row and column of the 2x2 contingency matrix constant, as per the assumptions of the Fisher test.
//' For the single-tailed alternative representing the upper-tail 'greater-than' expected overlap of the two gene
//' sets (\code{alternative = 1}), the terms start with *d* as the observed number of shared members between set
//' *i* and set *j*. Then *d* is incremented toward the maximal number possible shared genes (the lesser of the
//' number of genes in sets *i* and *j*). *a*, *b*, and *c* adjusted accordingly to keep constant row and
//' column sums, and the partial *p*-values are thus summed.
//'
//' For the lower-tail ('less-than') alternative (\code{alternative = 2}), the summation starts with *d* as the
//' number of shared members of sets between *i* and *j*, (as with the upper-tail calculation) but then decrements
//' that to 0.
//'
//' For the 2-tailed alternative, the function sums all the terms with values equal to or less than the the
//' partial *p*-value defined above.
//'
//' All calculations are done on log-transformed values to avoid arithmetic underflow:
//'
//'\deqn{
//'  ln(p) = ln(( a + b )!) + ln(( c + d )!) +
//'          ln(( a + c )!) + ln(( b + d )!) -
//'          ln(a!) - ln(b!) - ln(c!) - ln(d!) -
//'          ln(( a + b + c + d )!)
//'}
//'
//' Since log-transformed *p*-values cannot be directly added, the so-called log-sum-exponential trick is used to
//' combine them.
//'
//'
//' @examples
//'
//' library( GSNA )
//'
//' # Get the background of observable genes set from
//' # expression data:
//' gene_background <- toupper(rownames( Bai_empty_expr_mat ))
//'
//' # Using the sample gene set collection **Bai_gsc.tmod**,
//' # generate a gene presence-absence matrix filtered for the
//' # ref.background of observable genes:
//' presence_absence.mat <-
//'   makeFilteredGenePresenceAbsenceMatrix( ref.background = gene_background,
//'                                          geneSetCollection = Bai_gsc.tmod )
//'
//' lf.mat <- scoreLFMatrix_C( presence_absence.mat,  1 )
//'
//' @seealso
//'  \code{\link{buildGeneSetNetworkLFFast}}
//'  \code{\link{scoreJaccardMatrix_C}}
//'
// [[Rcpp::export]]
SEXP scoreLFMatrix_C(SEXP geneSetCollection_m,
                     Rcpp::Nullable<NumericVector> e_precision = Rcpp::NumericVector::create(12),  //R_NilValue
                     SEXP alternative = Rcpp::IntegerVector::create( 1 )
) {
  if( ! Rf_isMatrix(geneSetCollection_m) ){
    stop("Argument 'geneSetCollection_m' must be a matrix.");
  }
  Rcpp::LogicalMatrix GLCF(geneSetCollection_m);

  double e_precis = 12;
  if( e_precision.isNotNull() ){
    e_precis = Rcpp::as<double>(e_precision);
  }

  int alternative_ = as<int>(alternative);

  Rcpp::NumericMatrix LF(GLCF.ncol(), GLCF.ncol());
  int bg_size = GLCF.nrow();
  int n_modules = GLCF.ncol();

  for (int j=0; j<n_modules; j++){
    for (int i=0; i<n_modules; i++){
      LF(j,i) = NA_REAL;
    }
  }

  for (int j=0; j<n_modules-1; j++){
    for (int i=j+1; i<n_modules; i++){
      int set_ij = 0;
      int set_i = 0;
      int set_j = 0;
      int set_bg = 0;

      // Count the background, i, j and ij sets:
      for( int k=0; k<bg_size; k++){
        if( GLCF(k,i) != TRUE && GLCF(k,j) != TRUE )
          set_bg++;
        else if( GLCF(k,i) != TRUE )
          set_j++;
        else if( GLCF(k,j) != TRUE )
          set_i++;
        else
          set_ij++;
      }
      // Calculate log Fisher p-value
      double lf2s = lfisher_cpp( set_bg, set_i, set_j, set_ij, e_precis, alternative_ );
      LF(j,i) = lf2s;
      LF(i,j) = lf2s;
    }
  }
  rownames( LF ) = colnames(GLCF);
  colnames( LF ) = colnames(GLCF);

  /* Set the attribute for lower_is_closer. For alternative 1 (greater than), 3 (two-sided), and 4 (partial), the value
   * is set to true which will be the desired behavior for most situations. For alternative 2 (less than), the value is
   * set to FALSE.
   */
  bool lower_is_closer = TRUE;
  String distance_ = NA_STRING;
  if( alternative_ == 1 ){        // Upper-tail
    distance_ = "stlf";
  } else if( alternative_ == 2 ){ // Lower-tail
    lower_is_closer = FALSE;
    distance_ = "ltlf";
  } else if( alternative_ == 3 ){ // Two-tail
    lower_is_closer = FALSE;
    distance_ = "ttlf";
  } else if( alternative_ == 4 ){ // Partial
    distance_ = "lf";
  }

  LF.attr("lower_is_closer") = LogicalVector::create( lower_is_closer );
  LF.attr("distance") = CharacterVector::create( distance_ );
  LF.attr("distance_type") = CharacterVector::create( "ln_pval" );
  return(LF);
}









