
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GSNA version 0.1.0

**Jonathan M. Urbach**

**Ragon Instute of MGH, MIT, and Harvard**

**2023-12-21**

<!-- badges: start -->
<!-- badges: end -->

GSNA stands for **G**ene **S**et **N**etwork **A**nalysis. GSNA is a
toolkit for assessing the similarity of gene sets based on metrics of
similarity and distance such as the Jaccard and Szymkiewicz–Simpson
overlap indices, and log Fisher *p*-values. The intended purpose of the
GSNA package is to provide a set of ancillary methods to pathways
analysis such as **GSEA** (Subramanian et al. (2005),Mootha et al.
(2003)), and **CERNO** (Zyla et al. (2019)). GSNA can be used subsequent
to pathways analysis methods that generate lists of gene sets with
associated significance statistics, e.g. *p*-values. From such data,
groups of similar pathways can be inferred, greatly simplifying the task
of analyzing complex pathways datasets. On the basis of similarity,
networks and clusters (or subnets) can be generated and represented
graphically, and statistical parameters can be assessed.

## Dependencies

Installation of the **GSNA** package depends on other R packages,
including the following:

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
- raster  
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
Linux installations generally include the requred compilers,
specifically the GCC compilers.

**NOTE:** Particular care should be paid to the installation of the R
**raster** package. The **raster** package requires the **terra**
package, which in turn requires the **GEOS** C++ computational library,
available from <https://libgeos.org/> to compile from source. Binary
packages for the **GEOS** library are available for numerous Linux
distributions, and may offer a more convenient alternative than
installing **GEOS** from source.

## Installation

We intend to make the **GSNA** package available on CRAN in the near
future. If and when it is accepted by CRAN, you will be able to install
\*\*GSNA using the following command:

``` r
install.packages('GSNA')
```

In the meantime, if you have the **devtools** package installed, you can
install the development version of GSNA directly from GitHub like so:

``` r
devtools::install_github( repo = "https://github.com/JonathanUrbach/GSNA" )
```

Note: Currently, this method omits installation of the package’s
Roxygen2 man pages.

If you have downloaded the source code from GitHub and opened the
project in an Rstudio session, the following command can be used to
install the package, including documentation:

``` r
devtools::install( build_vignettes = TRUE, args = "--no-multiarch --with-keep.source" )
```

## Loading GSNA and Accessing Documentation

To load GSNA, type the following in your R console:

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
