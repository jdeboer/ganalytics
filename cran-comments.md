## Test environments
* Windows 10 x86_64 running R version 3.5.2
* Ubuntu 14.04.5 LTS x86_64 running R version 3.3.0
* Ubuntu 14.04.5 LTS x86_64 running R version 3.5.1
* Ubuntu 14.04.5 LTS x86_64 running R version (2018-12-21 r75883 devel)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs or NOTEs.

There was 1 warning:
> checking re-building of vignette outputs ... WARNING
  Error in re-building vignettes:
    ...
  Quitting from lines 30-43 (googleAnalyticsR-dynamic-segments.Rmd) 
  Error: processing vignette 'googleAnalyticsR-dynamic-segments.Rmd' failed with diagnostics:
  Authentication options didn't match existing session token and not interactive session
             so unable to manually reauthenticate
  Execution halted
  
The vignettes cannot be rebuilt remotely as they require an interactive session to authenticate the user with the Google Analytics account needed to obtain the data used in generating the examples in the vignettes.

## Downstream dependencies
There are currently no downstream dependencies for this package.

