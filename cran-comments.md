# Package GSNA  

* This is a new release.  


# devtools::check( cran = TRUE ) (2024-01-18/MacOS X)

## Results:

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


# devtools::check_rhub() (2024-01-18)

## Results:


>     Status: 4 NOTEs
>     ─  Done with R CMD check
>     ─  Cleaning up files and user
>         
>     
>     ── GSNA 0.1.1: NOTE
>     
>       Build ID:   GSNA_0.1.1.tar.gz-88418bcff5d2486592e5f082a6c9a5aa
>       Platform:   Windows Server 2022, R-devel, 64 bit
>       Submitted:  8m 43.5s ago
>       Build time: 8m 25.7s
>     
>     ❯ checking CRAN incoming feasibility ... [14s] NOTE
>       Maintainer: 'Jonathan M Urbach <jurbach@mgh.harvard.edu>'
>       
>       New submission
>       
>       Possibly misspelled words in DESCRIPTION:
>         CERNO (9:85)
>         GSEA (9:92)
>         GSNA (8:69, 11:20, 14:18)
>         epigenetic (13:66)
>         transcriptomic (13:50)
>       
>       Found the following (possibly) invalid URLs:
>         URL: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10231436
>           From: inst/CITATION
>           Status: 403
>           Message: Forbidden
>     
>     ❯ checking HTML version of manual ... [12s] NOTE
>       
>     
>     ❯ checking for non-standard things in the check directory ... NOTE
>       Found the following files/directories:
>         ''NULL''
>     
>     ❯ checking for detritus in the temp directory ... NOTE
>       Found the following files/directories:
>         'lastMiKTeXException'
>     
>     0 errors ✔ | 0 warnings ✔ | 4 notes ✖
>     
>     ── GSNA 0.1.1: CREATED
>     
>       Build ID:   GSNA_0.1.1.tar.gz-dfb87b6bd3954095b12ed3c9461aea09
>       Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC
>       Submitted:  8m 43.9s ago
>     
>     
>     ── GSNA 0.1.1: CREATED
>     
>       Build ID:   GSNA_0.1.1.tar.gz-b1814bcffe09423cada90ad49de33d9a
>       Platform:   Fedora Linux, R-devel, clang, gfortran
>       Submitted:  8m 44s ago
>     
>     
>     ── GSNA 0.1.1: CREATED
>     
>       Build ID:   GSNA_0.1.1.tar.gz-a52bea30871e423da33485334c58e2df
>       Platform:   Debian Linux, R-devel, GCC ASAN/UBSAN
>       Submitted:  8m 44.1s ago
>     
>     


## Responses to devtools::check_rhub() checks  

>     ❯ checking CRAN incoming feasibility ... [14s] NOTE
>       Maintainer: 'Jonathan M Urbach <jurbach@mgh.harvard.edu>'
>       
>       New submission
>       
>       Possibly misspelled words in DESCRIPTION:
>         CERNO (9:85)
>         GSEA (9:92)
>         GSNA (8:69, 11:20, 14:18)
>         epigenetic (13:66)
>         transcriptomic (13:50)
>       
>       Found the following (possibly) invalid URLs:
>         URL: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10231436
>           From: inst/CITATION
>           Status: 403
>           Message: Forbidden

* The terms 'CERNO', 'GSEA', and 'GSNA' are abbreviations and correct. The terms 'epigenetic' and 'transcriptomic' are also correct.  
* The URL: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10231436 is valid and correct.  

>     ❯ checking HTML version of manual ... [12s] NOTE

* This note is cryptic and difficult to fix.  

>     ❯ checking for non-standard things in the check directory ... NOTE
>       Found the following files/directories:
>         ''NULL''

* This appears to be incorrect. There is no such file in the package.  

>     ❯ checking for detritus in the temp directory ... NOTE
>       Found the following files/directories:
>         'lastMiKTeXException'

* The file 'lastMiKTeXException' is not present in the package. It may be an artifact of the creation of the manual.   


# devtools::check_mac_release() (2024-01-18/MacOS X)

* Build system: r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 
* No errors, warnings, or notes.

# devtools::check_win_devel() (2024-01-18/Windows)

* x86_64-w64-mingw32  
* R was compiled by  
  +  gcc.exe (GCC) 12.3.0  
  +  GNU Fortran (GCC) 12.3.0  
* 1 NOTE:

>     * checking CRAN incoming feasibility ... NOTE
>     Maintainer: 'Jonathan M Urbach <jurbach@mgh.harvard.edu>'
>     
>     New submission
>     
>     Possibly misspelled words in DESCRIPTION:
>       CERNO (9:85)
>       GSEA (9:92)
>       GSNA (8:69, 11:20, 14:18)
>       epigenetic (13:66)
>       transcriptomic (13:50)
>     

## Response to this note:  

* As in the rhub checks, the terms 'CERNO', 'GSEA', 'GSNA', 'epigenetic', and 'transcriptomic' are correct.


