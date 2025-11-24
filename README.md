<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![NSF-1948926](https://img.shields.io/badge/NSF-1948926-blue.svg)](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1948926)
[![Codecov test coverage](https://codecov.io/gh/NeotomaDB/neotoma2/branch/production/graph/badge.svg)](https://app.codecov.io/gh/NeotomaDB/neotoma2?branch=production)
[![DOI](https://zenodo.org/badge/265675258.svg)](https://zenodo.org/badge/latestdoi/265675258) 
[![status](https://joss.theoj.org/papers/80636d1e6d32175f13edb7e7ed14089a/status.svg)](https://joss.theoj.org/papers/10.21105/joss.05561)
<!-- badges: end -->

# `neotoma2` R Package

The `neotoma2` R package represents a set of breaking changes with the original `neotoma` R package. The `neotoma` package was deprecated following end-of-life for the Neotoma Windows Server in 2020 and the migration of the Neotoma backend infrastructure to a PostgreSQL database and JavaScript API.

The `neotoma2` package is built on the new [Neotoma API](https://api.neotomadb.org/api-docs) and is intended as a starting point for a fully interactive experience with the [Neotoma Paleoecology Database](https://www.neotomadb.org), to support both data access and data input through R.

## Contributors

This project is an open project, and contributions are welcome from any individual.  All contributors to this project are bound by a [code of conduct](https://github.com/NeotomaDB/neotoma2/blob/production/CODE_OF_CONDUCT.md).  Please review and follow this code of conduct as part of your contribution.

* [![ORCID](https://img.shields.io/badge/orcid-0000--0002--7926--4935-brightgreen.svg)](https://orcid.org/0000-0002-7926-4935) [Socorro Dominguez Vidana](https://ht-data.com/)

* [![ORCID](https://img.shields.io/badge/orcid-0000--0002--2700--4605-brightgreen.svg)](https://orcid.org/0000-0002-2700-4605) [Simon Goring](http://www.goring.org)

### Tips for Contributing

Issues and bug reports are always welcome.  Code clean-up, and feature additions can be done either through pull requests to [project forks](https://github.com/NeotomaDB/neotoma2/network/members) or [project branches](https://github.com/NeotomaDB/neotoma2/branches).

Please direct development questions to Simon Goring by email: [goring@wisc.edu](mailto:goring@wisc.edu).

All products of the Neotoma Paleoecology Database are licensed under an [MIT License](LICENSE) unless otherwise noted.

## How to use this repository

All R functions for the package should be written in the `R` folder.  Any documentation should be added to `.R` files using [`roxygen2`](https://cran.r-project.org/package=roxygen2) notation.  Because we are using `roxygen2` for documentation in this package, all edits to documentation should take place in the associated functions `.R` file. The files in the `man` folder should not be manually changed.

Class definitions and new methods should be added to the files `01_classDefinitions.R` and `02_genericDefinitions.R` respectively, to ensure they are properly loaded during the package build process.

### Development Workflow Overview

The `neotoma2` package is built for R.  Build tools include elements from the `usethis`, `devtools` and `testthat` R packages, and build and compilation occurs within (and outside) the RStudio IDE environment.

Package use requires the use of the `devtools::install_github()` function, to pull this working repository into a user's environment:

```r
devtools::install_github('NeotomaDB/neotoma2', build_vignettes = TRUE)
```

The expectation for this repository is that all commits to the `prod` branch will support a clean package build.  This is supported through [GitHub Actions](https://github.com/NeotomaDB/neotoma2/actions) in the `.github` folder of the repository.

### Analysis Workflow Overview

There is considerable information in the vignettes for the package, which can be [accessed directly](https://github.com/NeotomaDB/neotoma2/tree/production/vignettes).

### Report of Sites Statistics

In order to see the total available sites that can be queried by this package, use the function:

```
neotoma2::get_stats(start=0, end=1)
```

### System Requirements

This project is built with R > v4.0.  The packages needed for proper package use are detailed in the `DESCRIPTION` file for this repository.

### Data Requirements

The `neotoma2` R package pulls data from the [Neotoma Paleoecology Database](https://www.neotomadb.org).  Neotoma maintains a [permissive data use policy](https://www.neotomadb.org/data/data-use-and-embargo-policy).  Within the data use policy there is a statement on co-authorship which reads:

> Normal ethics apply to co-authorship of scientific publications. Paleoecological datasets are labor-intensive and complex: they take years to generate and always have additional attributes and metadata not captured in Neotoma. Neotoma data quality also relies on expert curation by data stewards, each associated with one or more Constituent Databases. **Users of data stored in Neotoma’s Constituent Databases should consider inviting the original data contributor, or Constituent Database steward(s), to be a co-author(s) of any resultant publications if that contributor’s data are a major portion of the dataset analyzed, or if a data contributor or steward makes a significant contribution to the analysis of the data or to the interpretation of results.** For large-scale studies using many Neotoma records, contacting all contributors or stewards or making them co-authors will not be practical, possible, or reasonable. Under no circumstance should authorship be attributed to data contributors or stewards, individually or collectively, without their explicit consent.

## Metrics

This project is to be evaluated using the following metrics:

* CRAN release 1.0.8 with speleothem data support and improved documentation.**DONE** November 18, 2025
* Maintenance Github 1.0.7 release **DONE** June 15, 2025
* Maintenance Github 1.0.6 release **DONE** June 23, 2025
* Maintenance Github 1.0.5 release, **DONE** December 27, 2024
* Maintenance Github 1.0.4 release, **DONE** July 23, 2024
* Updated production Branch with CRAN changes, May 24, 2024.
* Maintenance Github 1.0.3 release, **DONE** February 28, 2024
* Published JOSS paper, **DONE** 28 November, 2023
* Submitted paper to JOSS **DONE** 03 May, 2023
* Completion CRAN 1.0.0 release **DONE** April 23, 2023
* Completion of core functionality for data access **DONE** Feb 10, 2022
* Completion of core functionality for data presentation **DONE**
* Completion of clear vignettes for major data types or Constituent Databases represented within the Neotoma Database.
