# GSNA 0.1.6.8
* Development version
* Fixes a bug in `gsnSubnetsDotPlot()` that can lead to inappropriate duplication of x-axis labels.  

# GSNA 0.1.6.7
* Development version
* `gsnSubnetsDotPlot()` was improved to allow it to function properly when passed a data.frame via the `merged.pathways.data` argument. Prior to this, it would reqire a GSNData object to produce a plot.  
* `gsnSuggestCutoff()` was enhanced by the addition of the `extr_dist_quantile_override` argument, a logical value allowing users to override a quantile value when the distance corresponding to that quantile was an extreme value, either the maximum or minimum value. Hence, users can avoid a situation where clusters/subnets are either not resolved, no clusters are identified.  
* `yassifyPathways()` was enhanced by the addition of the `color_by` argument, allowing users to specify a column by which to color output rows. (The default is 'subnet')  

# GSNA 0.1.6.6
* Development version
* The `gsnSubnetsDotPlot()` was enhanced:
  + to allow it to handle naked expressions without the need to enclose them in the `expression()` function.
  + to include an argument allowing the user to configure the fill color used to represent NA and infinite values (na.color).
  + to include the argument return_subnets indicating that the function should return a data.frame containing the subnet data rather than a plot object. 
* Tests were added for `gsnSubnetsDotPlot()`.
* Fixed a bug in `gsnPathways<-()` that prevented assignment of columns that are factors, and added a test for that.
* The `termSummary()` function was fixed to make the returned summaries unique. This behavior, which is default, can be overidden however by specifying `make.unique=FALSE`.
* The `nzLog10.old()` function was removed.  

# GSNA 0.1.6.5
* Development version  
* Numerous changes to `gsnSubnetsDotPlot()`:
  + The `max_subnets` argument was added allowing users to specify a maximal number of (top) subnets plotted.
  + The `subset` argument was added, giving users fine control over which subnets and gene sets are plotted.
  + Added the ability to specify a transformation (like `"log10"`) for color values using the `color_transform`.
  + Discrete (character or factor) data is now supported for the `color_col` (character or factor).
* Prior to this version, the `gsnMergePathways()` function has always returned data.frames in which the column specified by the `id_col` argument was named simply `ID`. This version allows the user to suppress this behavior by setting a (new) `id_reassign` argument to `FALSE`. The default behavior (to return the column as `ID`) is unchanged.  
* Several tests were added for `gsnMergePathways()`.

# GSNA 0.1.6.4
* Development version  
* Contains a fix for `gsnSubnetsDotPlot()` in which interactive plots were not occupying available space.  

# GSNA 0.1.6.3
* Development version  
* Contains bug-fixes for `gsnSubnetsDotPlot()`.  

# GSNA 0.1.6.2
* Development version
* Adds tests for `gsnPathways()` and `buildGeneSetNetworkKappa()`.

# GSNA 0.1.6.1
* Development version  
* Added tests for `termSummary()`, `gsnSubnetsDotPlot()`, and `myColorF()`.  
* Updated the vignette.  

# GSNA 0.1.6.0
* Development version  
* Added the `termSummary()` function (along with some associated helper functions `filterTerms()` and `score_word_clusters()`) that can take multiple gene set names/terms and convert them to summarized names.    
* Added the `gsnSubnetsDotPlot()` function to provide an additional way to represent gene set subnets, ploting gene sets grouped on the y-axis by subnet, positioned on the x-axis by a configurable statistical column, with scaling by number of gene set members and coloring by an additional configural statistic.  
* Added the `prepend_subnet` function for the `gsnPlotNetwork()` function.  

# GSNA 0.1.5.2
* Development version  
* The `prepend_subnet` argument was added to the gsnPlotNetwork function, allowing the users to prepend the subnet name to gene set identifiers when plotting networks. This feature is now enabled by default.

# GSNA 0.1.5.1
* Development version  
* The function `gsnSuggestCutoff()` is added. This function suggests appropriate cutoffs for paring using `gsnPareNetGenericHierarchic()` or `gsnPareNetGenericToNearestNNeighbors()`.  
* The `yassifyPathways()` function has been updated to better handle the insertion of URL links via the `url_map_by_words_list` argument.  
* The `gsnDistanceHistogram()` function has been enhanced with the option to plot the distance distribution as a set of stacked plots, which may optionally have independent *x* and/or *y* coordinates. These new featuresa are accessed via the `use_facets`, `scales`, and `ncol` arguments.  
* The `gsnPlotNetwork()` and `gsnHierarchicalDendrogram()` functions have been fixed to properly indicate in the one and two-color figure legends the transformed nature of the statistal columns used for node coloring.  

# GSNA 0.1.5
* In previous versions of the GSEA package, sample data included in the vignette contained gene sets derived from the academic version of the dorothea package, and therefore may not be used for commercial purposes. In this version, the GSEA example in the vignette and the corresponding gene sets from the dorothea package used as example data in the vignette were updated to include only genes sets and genes from the non-academic version of the dorothea package, so as to be compatible with commercial use.  
* Includes numerous corrections to the vignette.  

# GSNA 0.1.4.8
* Development version  
* Fixes a bug with `gsnAddPathwayData()` (not present in `gsnAddPathwaysData()`).  
* Adds additional tests for `gsnAddPathwayData()` and `gsnAddPathwaysData()`.  

# GSNA 0.1.4.7
* Development version  
* Fixed a serious bug in `lfisher_cpp()` that intermittantly produced erroneously significant log Fisher p-values with large backgrounds/numbers of total genes.  This bug, which also affected `GSNA::buildGeneSetNetworkSTLF()`, `GSNA::buildGeneSetNetworkLF()` and `GSNA::scoreLFMatrix_C()`,  was due to an interaction between a rounding error in the summation of partial p-values with a programming logic issue. Also, added tests to tests/test-lfisher_cpp.R to detect this problem.    
* Fixed numeric overrun problem with the `lse()` function that led to incorrect calculation of log harmonic means with the private `lhm()` function, which resulted in incorrect log harmonic mean P values being calculated by `gsnSubnetSummary()`. This bug was a contributor to incorrect calculation of the log of harmonic means of within subnet single-tail log-Fisher values.    
* Fixed a sorting issue that also contributed to incorrect calculation of log harmonic mean within-subnet single tail Fisher *p*-values.  
* Updated `yassifyPathways()` so that identifiers containing colons such as GO terms (e.g. "GO:0045824") can be used with the `url_map_by_words_list` argument. Currently, word characters `"\\w"` and colons `":"` can be used.  
* Fixed problem with gsnImportGenericPathways() that caused explicitly specified values of `stat_col` to be ignored.  

# GSNA 0.1.4.6  
* Development version
* Changed install command in README.Rmd/README.md to `devtools::install_github( "JonathanUrbach/GSNA", build_vignettes = TRUE )` so that the vignette will be installed.
* Changed names of output columns of gsnTestSummary() results set (LHM.STLF and LGM.STLF were changed to STLF.HM and STLF.GM respectively).   
* Tests for gsnTestSummary() was updated to work with new column names and include a test for GSNA:::lhm() correctly working.

# GSNA 0.1.4.5  
* Development version  
* Added the ability to construct networks using Cohen's Kappa (*&kappa;*), which is the method employed by **DAVID**.
* Removed ./doc from .gitignore so that installation of vignette from GitHub would work.  

# GSNA 0.1.4.4  
* Development version  
* 1 and 2-color legends are now generated by `make1ColorLegend()` and `make2ColorLegend()` using `Matrix::image` rather than `raster::plotRGB()`, hence allowing removal of the obsolete **raster** package as a dependency and hopefully simplifying the package install process.   
* The `makeNodeSizeLegend()` now handles node sizes differently, such that node sizes in the legend are properly scaled when the images are scaled.  

# GSNA 0.1.4.3  
* Includes the ./man directory and .Rd files as part of the GitHub repository, so that installations from GitHub via `devtools::install_github()` will now include the manual pages, without the need to run `devtools::document()` or `roxygen2::roxygenize()`.  
* Adds tests for gsIntersectCounts() & lfisher_cpp(), and silences the messages for gsnAddPatheaysData() and gsnImportGenericPathways().  
* Includes the ./man directory and .Rd files as part of the GitHub repository, so that installations from GitHub via `devtools::install_github()` will now include the manual pages, without the need to run `devtools::document()` or `roxygen2::roxygenize()`.  
* GSNA documentation cannot currently be built roxygen2 version 7.3.2. A bug report has been submitted (https://github.com/r-lib/roxygen2/issues/1635).  
* The previous version of GSNA, version 0.1.4.2 was archived on CRAN due to failure to fix a memory overflow that occurred during package tests (see: https://cran-archive.r-project.org/web/checks/2024/2024-04-04_check_results_GSNA.html). Code from the **ragg** package was called when CRAN curators tested the code on CRAN's testing platform, which included a Linux environment with R packages linked with the **ASAN** library (Address Sanitizer). Although the **GSNA** package does not directly call any **ragg** functions, and has been confirmed to run on platforms from which the **ragg** library has been removed, when the package tests were run by the curators on the **ASAN** testing platform, a (fatal) a container overflow (memory access outside of allocated memory) resulted when **ragg** functions were called. This was presumably the result of an *optional* function call by one of the **GSNA** dependencies. This error was not reproducible on our development platforms, making it difficult to fix, so for the moment, **GSNA** development is targeted for GitHub distribution.  
* This is intended to be the last version with dependency on the obsolete **raster** library. (This may be the source of calls to the **ragg** library, see above.)  

# GSNA 0.1.4.2
* This version attempts to address a problem coming up with CRAN's development R linux platform tests, which identified a problem with validation of the HTML manual for gsnAddPathwayData & gsnAddPathwaysData that resulted in the note warning "Warning: trimming empty <dt>". The warning is not reproduced on r-hub builder or other uploadable R package build testing services, but these changes should remove the problematic documentation. 

# GSNA 0.1.4.1
* This is the fifth submission to CRAN.
* Spelling errors were fixed, including 'pakage' in DESCRIPTION, 'ATACSeq' which should be 'ATAC-Seq', 'Imagee' which should be 'Image' and a few others.

# GSNA 0.1.4  
* This is the fourth submission to CRAN.  
* Fixes were made to the 'Description' section of the 'DESCRIPTION' file, to apply the required single quote guidelines for package names, program names and API names only.  
* Unexported functions have @noRd tags added to suppress the generation of .Rd files.  
* The @examples tags have been removed for unexported functions, so that example code is not tested or run.  


# GSNA 0.1.3
* This is the third submission to CRAN.  
* It fixes numerous issues raised by the CRAN curator/moderator, including:  
  + Format issues in the DESCRIPTION file.  
  + Missing \value tag in gsnParedVsRawDistancePlot.Rd.  
  + @example tags in documentation for non-exported @noRd tagged functions were removed. The example code was retained in '@details'.  
  + Example code wrapped in \dontrun{} tags was either:
    - Removed and replaced with code updated with runnable code using example data.  
    - Removed altogether, generally in cases where additional external files would be required. (e.g. parsers for GMT (read_gmt()), DAVID (read_david_data_file()) file formats.) Example code may be re-added in the future once example data files are included.  
    - In some cases, examples needed to be wrapped in \donttest{} tags, due to long run-time or requirement of a network connection to download data sets. (gsnPlotNetwork(), pick_MappedGeneSymbol())
  + Several plotting-associated functions have now been changed to internal, non-exported functions. (makeLeafSizeLegend(), makeNodeSizeLegend(), make2ColorLegend(), make1ColorLegend(), etc.)  
  + Functions that formerly set graphical parameters through par() now backup the original parameters before doing so, and restore them automatically through a call to on.exit(par(.par.orig)) (gsnHierarchicalDendrogram(), gsnPlotNetwork(), make1ColorLegend(), make2ColorLegend(), makeLeafSizeLegend(), makeNodeSizeLegend(), and renderCircularDendrogram()). 
  
  

# GSNA 0.1.2
* This version is the second submission version to CRAN.
* Fixes three main NOTE issues:
  1. Use of .plt instead of .plt.leg for make1ColorLegend() and make2ColorLegend()
  2. The 'lost braces' problem.
  3. Excessive package size, due largely to the use of SVG format in the vignette graphics. PNGs are used instead.
* There remain some additional NOTES, generated by devtools::check_rhub(), but they may not be important.
* Additional fixes.
  + Fixed a bug in scoreLFMatrix_C() that hard-coded e_precision = 12. It now works properly.
  + Fixed a bug in gsnPlotNetwork() & gsnHierarchicalDendrogram() that prevented proper rendering of PNG format outputs.

# GSNA 0.1.1
* This is (hopefully) the first public CRAN release version.

# GSNA 0.1.0
* This is the development version prior to CRAN submission.

