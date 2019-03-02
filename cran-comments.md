## Test environments
* Local Windows 10 x86_64 running R version 3.5.2
* R for Travis-CI Ubuntu 14.04.5 LTS x86_64-pc-linux-gnu (64-bit) R version 3.3.0
* R for Travis-CI Ubuntu 14.04.5 LTS x86_64-pc-linux-gnu (64-bit) R version 3.5.2 (2017-01-27)
* R for Travis-CI Ubuntu 14.04.5 LTS x86_64-pc-linux-gnu (64-bit) R-devel (2019-03-02 r76189)
* R-Hub Builder Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* R-Hub Builder Ubuntu Linux 16.04 LTS, R-release, GCC
* R-Hub Builder Fedora Linux, R-devel, clang, gfortran
* Win-Builder x86_64-w64-mingw32 (64-bit), R-devel (2019-03-01 r76188)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs across all test environments except for 1 WARNING when built under R-Hub Builder Fedora Linux, R-devel, clang, gfortran:

  Warning in (function (filename = "Rplot%03d.png", width = 480, height = 480,  :
    unable to open connection to X11 display ''
  Quitting from lines 55-63 (googleAnalyticsR-table-filters.Rmd) 
  Error: processing vignette 'googleAnalyticsR-table-filters.Rmd' failed with diagnostics:
  unable to start device PNG
  --- failed re-building ‘googleAnalyticsR-table-filters.Rmd’

## Downstream dependencies
There are currently no downstream dependencies for this package.

