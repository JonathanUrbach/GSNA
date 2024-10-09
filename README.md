
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GSNA version 0.1.4.7

**Jonathan M. Urbach**

**Ragon Institute of MGH, MIT, and Harvard**

**2024-10-09**

<!-- badges: start -->
<!-- badges: end -->

**GSNA** stands for **G**ene **S**et **N**etwork **A**nalysis. **GSNA**
is a toolkit for clustering gene sets based on metrics of similarity and
distance such as the Jaccard and Szymkiewicz–Simpson overlap indices,
Cohen’s kappa (κ) and log Fisher *p*-values. The intended purpose of the
**GSNA** package to provide a means to simplify data sets generated by
pathways analysis methods such as **GSEA** (Subramanian et al.
(2005),Mootha et al. (2003)), and **CERNO** (Zyla et al. (2019)).
**GSNA** can be used subsequent to pathways analysis methods that
generate lists of gene sets with associated significance statistics,
e.g. *p*-values. From such data, groups of similar pathways are
inferred, greatly simplifying the task of analyzing complex pathways
datasets. On the basis of similarity, networks and clusters (or subnets)
can be generated and represented graphically, and statistical parameters
can be assessed.

## Dependencies

We recommend R version 4.0 and later for **GSNA**, though it may be
installable on some later R version 3 distributions. In addition to base
R, the **GSNA** package requires some other R packages including the
following:

- circlize  
- DT  
- dendextend  
- dplyr  
- ggplot2  
- graphics  
- grDevices  
- igraph  
- Matrix  
- methods  
- psych  
- stringr  
- stringi  
- stats  
- tibble  
- tidyr  
- tmod  
- utils  
- withr  
- Rcpp

Several of these packages have cascading dependencies, which become
particularly important when installing from source. If binary R packages
are available, such as for up-to-date Windows and Mac OS X R
installations, we recommend installing those. Compiling these packages
from source generally requires a C, C++, and/or Fortran compiler, which
on Windows means installing Rtools for Windows, and for Mac OS X, Xcode.
Linux installations generally include the required compilers,
specifically the GCC compilers.

## Installation

The easiest way to install the **GSNA** package is to install directly
from GitHub. To do this, you need to have the **devtools** package
installed. The following command checks to see if devtools is installed,
and if it is not, installs the package:

``` r
if( ! require( 'devtools' ) ){
  install.packages( 'devtools' )
  require( 'devtools' )
}
```

To install GSNA from GitHub, run this R command:

``` r
devtools::install_github( "JonathanUrbach/GSNA", build_vignettes = TRUE )
```

We hope to make the **GSNA** package available (again) on CRAN in the
near future. If and when it is accepted by CRAN, you will be able to
install **GSNA** using the following command:

``` r
install.packages('GSNA')
```

## Loading **GSNA** and Accessing Documentation

To load **GSNA**, type the following in your R console:

``` r
library( GSNA )
```

To access a vignette containing additional information and usage
examples, run:

``` r
vignette( "using_the_gsna_package" )
```

A package description and a list of function documentation can be
obtained via:

``` r
help( package = "GSNA" )
```

# Citing **GSNA**

To cite package **GSNA** in publications, please use:

Collins, R. D, Urbach, M. J, Racenet, J. Z, Arshad, Umar, Power, A. K,
Newman, M. R, Mylvaganam, H. G, Ly, L. N, Lian, Xiaodong, Rull, Anna,
Rassadkina, Yelizaveta, Yanez, G. A, Peluso, J. M, Deeks, G. S, Vidal,
Francesc, Lichterfeld, Mathias, Yu, G. X, Gaiha, D. G, Allen, M. T,
Walker, D. B (2021). “Functional impairment of HIV-specific CD8+ T cells
precedes aborted spontaneous control of viremia.” *Immunity*,
S107476132100337X. <doi:10.1016/j.immuni.2021.08.007>
<https://doi.org/10.1016/j.immuni.2021.08.007>,
<https://linkinghub.elsevier.com/retrieve/pii/S107476132100337X>.

Collins, R. D, Hitschfel, Julia, Urbach, M. J, Mylvaganam, H. G, Ly, L.
N, Arshad, Umar, Racenet, J. Z, Yanez, G. A, Diefenbach, J. T, Walker,
D. B (2023). “Cytolytic CD8+ T cells infiltrate germinal centers to
limit ongoing HIV replication in spontaneous controller lymph nodes.”
*Science Immunology*, *8*(83), eade5872.
<doi:10.1126/sciimmunol.ade5872>
<https://doi.org/10.1126/sciimmunol.ade5872>,
<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10231436>.

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-mootha_pgc-1-responsive_2003" class="csl-entry">

Mootha, Vamsi K., Cecilia M. Lindgren, Karl-Fredrik Eriksson, Aravind
Subramanian, Smita Sihag, Joseph Lehar, Pere Puigserver, et al. 2003.
“PGC-1α-Responsive Genes Involved in Oxidative Phosphorylation Are
Coordinately Downregulated in Human Diabetes.” *Nature Genetics* 34 (3):
267–73. <https://doi.org/10.1038/ng1180>.

</div>

<div id="ref-subramanian_gene_2005" class="csl-entry">

Subramanian, Aravind, Pablo Tamayo, Vamsi K. Mootha, Sayan Mukherjee,
Benjamin L. Ebert, Michael A. Gillette, Amanda Paulovich, et al. 2005.
“Gene Set Enrichment Analysis: A Knowledge-Based Approach for
Interpreting Genome-Wide Expression Profiles.” *Proceedings of the
National Academy of Sciences* 102 (43): 15545–50.
<https://doi.org/10.1073/pnas.0506580102>.

</div>

<div id="ref-zyla_gene_2019" class="csl-entry">

Zyla, Joanna, Michal Marczyk, Teresa Domaszewska, Stefan H E Kaufmann,
Joanna Polanska, and January Weiner 3rd. 2019. “Gene Set Enrichment for
Reproducible Science: Comparison of CERNO and Eight Other Algorithms.”
*Bioinformatics* 35 (24): 5146–54.
<https://doi.org/10.1093/bioinformatics/btz447>.

</div>

</div>
