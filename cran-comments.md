## Test environments
* Local Windows 10 x86_64 running R version 3.5.2
* Travis CI Ubuntu 14.04.5 LTS x86_64 running R version 3.3.0
* Travis CI Ubuntu 14.04.5 LTS x86_64 running R version 3.5.1
* Travis CI Ubuntu 14.04.5 LTS x86_64 running R version (2018-12-21 r75883 devel)
* R-Hub Builder Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* R-Hub Builder Ubuntu Linux 16.04 LTS, R-release, GCC
* R-Hub Builder Fedora Linux, R-devel, clang, gfortran
* Win-Builder x86_64-w64-mingw32 (64-bit), R-devel (2018-12-21 r75883)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

The vignettes cannot be rebuilt remotely as they require an interactive session. This is because the examples in the vignettes obtain data from a specific Google Analytics account. An interactive session is required for authorised users to authenticate with Google Analytics.

## Downstream dependencies
There are currently no downstream dependencies for this package.

