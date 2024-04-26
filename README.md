# One-line description of this awesome package

R package **mrfaocore**, version **0.1.0.9001**

[![CRAN status](https://www.r-pkg.org/badges/version/mrfaocore)](https://cran.r-project.org/package=mrfaocore)  [![R build status](https://github.com/pik-piam/mrfaocore/workflows/check/badge.svg)](https://github.com/pik-piam/mrfaocore/actions) [![codecov](https://codecov.io/gh/pik-piam/mrfaocore/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrfaocore) 

## Purpose and Functionality

One-paragraph description of this awesome package.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrfaocore")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact David Chen <david.chen@pik-potsdam.de>.

## Citation

To cite package **mrfaocore** in publications use:

Kreidenweis U, Mishra A, Karstens K, Leon Bodirsky B, Leip D, Stevanovic M, Leon Bodrisky B, Chen D, Klein D, Molina Bacca E (2024). _mrfaocore: One-line description of this awesome package_. R package version 0.1.0.9001, <https://github.com/pik-piam/mrfaocore>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrfaocore: One-line description of this awesome package},
  author = {Ulrich Kreidenweis and Abhijeet Mishra and Kristine Karstens and Benjamin {Leon Bodirsky} and Debbora Leip and Mishko Stevanovic and Benjamin {Leon Bodrisky} and David Chen and David Klein and Edna {Molina Bacca}},
  year = {2024},
  note = {R package version 0.1.0.9001},
  url = {https://github.com/pik-piam/mrfaocore},
}
```
