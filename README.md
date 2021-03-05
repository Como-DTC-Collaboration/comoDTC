---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# comoDTC

<!-- badges: start -->
[![R-CMD-check](https://github.com/ben18785/comoDTC/workflows/R-CMD-check/badge.svg)](https://github.com/ben18785/comoDTC/actions)
[![Codecov test coverage](https://codecov.io/gh/Como-DTC-Collaboration/comoDTC/branch/master/graph/badge.svg)](https://codecov.io/gh/Como-DTC-Collaboration/comoDTC?branch=master)
<!-- badges: end -->

The goal of comoDTC is to provide an R package version of https://github.com/bogaotory/comoOdeCpp

## Installation

You can install the released version of comoDTC from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("comoDTC")
```

## Example

This is a basic example which shows you how to solve a common problem:


```r
library(comoDTC)
## basic example code
```

## Contributing
The comoDTC package is under active development. If you find a bug or have ideas about new features, please open an issue.

If you've not created an R package before and would like to contribute, we'd recommend reading [the whole game](https://r-pkgs.org/whole-game.html) chapter of Wickham and Bryan. In developing this package, we encourage contributors to use the `usethis` package (discussed within the chapter), since it automates many of the commonly required workflows like:

- adding a new function script and a paired test file by calling `usethis::use_r("my-func")` then calling `usethis::use_test()`
- adding a new dataset via `usethis::use_data(x)` where `x` is a data file we'd like to put in the package
- adding a new R package requirement via `usethis::use_package("example-R-package")`

We ask that contributions to comoDTC follow the following workflow:

1.  search existing issues to check whether one exists that describes the change / addition to the code base you'd like to make; if not, make a new issue
2.  clone the repo and create a new branch that is named `i[issue number]-[approximate issue name]`. For example, the "Create contributing docs issue" that was the basis for this change was issue \#8, so the branch is named `i8-contributing-docs`
3.  make local changes to your branch
4. pre-commit checks:
    - [No style issues](https://style.tidyverse.org/) (see below): `lintr::lint_package()`
    - All tests pass(see below): ``devtools::check()``
5. push your changes to the repo.
6.  create a pull request which describes this contribution and refers back to the original issue
7.  check that pull request passes github workflow tests
8.  assign at least one reviewer
9.  respond to reviewer comments and continue to work on code until it passes reviewer tests
10.  merge branch into master
11. close original issue referring to the pull request that merged into master

### Code tests in comoDTC

To check the validity of the code, we run a series of tests that (amongst other things):

-   check package meta data and structure
-   check package dependencies
-   check R code for syntax errors and any missing dependencies
-   check the data directory contains files of a type R know how to handle
-   check documentation: that functions and datasets are correctly documented (see [object documentation chapter](https://r-pkgs.org/man.html) of Wickham and Bryan). Note that you will have to run `devtools::document()` to build documentation locally before submitting code to the repo (which requires that you have `devtools` installed via `install.packages("devtools")`. Note also that if you make a change to the `README.md` file, you'll need to knit the document using `knitr::knit("README.Rmd")`
-   runs tests specified in `tests` directory

All of these tests can be run locally using `devtools::check()`. It is often useful to run individual checks locally (as they're faster) when trying to address a failing test:

-   `devtools::check_man()` checks the documentation
-   `devtools::test()` checks that the tests in the `tests` directory pass
##### Style and Linting
To make the package easier to read for users and the large number of participants we are using the style guidelines of [Hadley Wickham's The tidyverse style guide](https://style.tidyverse.org/). 

Compliants with this style guide can be checked for using the `lintr` package (installed via `install.packages('lintr')`) as follows:
-The R code in a file may be analysed using `lintr::lint(file_path)`
-The R code in a directory may be analysed using `lintr::lint_dir(dir_path)`
-The R code in an entire package may be analysed using `lintr::lint_package(pkg_path)` or `lintr::lint_package()` from the open package.
As with tests, it can be useful to lint individual files and directories worked on as this will run faster.
