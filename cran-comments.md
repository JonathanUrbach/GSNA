# Package GSNA  

* This is a new release.  

## R CMD check results (2024-01-10/MacOS X and Linux platforms:)

0 errors | 0 warnings | 2 notes

* The first note relates to package size of 6.4Mb, of which the largest portions are in the **doc/** (4.6Mb) and **data/** (1.0Mb) subdirectories.  
  + With respect to the **data** size, the inclusion of representative sample data was necessary for the purpose of the vignette and manual examples, and efforts were made to minimize the sample data size while allowing new users to work with example data similar to what they might expect from their own studies.  
  + With respect to the **doc** size, we have made efforts to be very thorough when documenting the GSNA package's functions and data. If necessary, we can attempt to be more concise in the future.  
* The second note relates to partial argument matches for partial argument match of '.plt' to '.plt.leg' in the **gsnHierarchicalDendrogram()** and **gsnPlotNetwork()** functions. These arguments are used internally to control the layout of plots and the legends within them, but are not likely to be used by most users.  

## R CMD check results (2024-01-10/Windows:)

0 errors | 0 warnings | 3 notes

* The windows check, using **devtools::check_win_devel()** had 3 notes, two of which were the same as above.  
  + In the third Windows note, the check function complains of a possible 'Lost braces; missing escapes or markup?' in the Rd file for the sample data, Bai_data.Rd. This may be because @format tags were used that span multiple lines and have embedded itemized lists. Despite the note, the resulting Rd data displays properly.   



