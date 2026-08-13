# News neotoma2 R package

## neotoma2 1.1.0

* Added `count()` to tally sites, collection units, or datasets in a record object.
* `samples()` gains a `chronname` argument to report ages against a chosen chronology.
* `filter()` now handles multiple conditions together with spatial (`loc`) filters.
* More robust parsing of malformed API records (empty slots no longer create phantom IDs).
* Faster `parseURL()` and automatic retry when the API is temporarily unavailable.
* `all_data = TRUE` stops as soon as a page comes back short, rather than spending
  an extra request to confirm the end of the results. Queries whose results fit in
  one page now cost one request instead of two.
* A request that times out is retried at most once. Retrying a slow query stacks
  work onto an API that is already struggling, which made spatial (`loc`) queries
  worse rather than better; other transient failures still retry as before.
* Raised the default request timeout to 180 seconds, above the observed latency of
  the spatial endpoints, so healthy-but-slow queries are no longer aborted
  mid-flight. `NEOTOMA_TIMEOUT` and `NEOTOMA_RETRIES` still override both.
* A spatial geometry is now converted once per query rather than once per page,
  and the API's parameter list is fetched once per session rather than on every
  `get_*()` call.
* Paginated queries report their progress in interactive sessions.

## neotoma2 1.0.12

Updated description file to match CRAN's comments.
Now follows:
 comment = c(ORCID = .......)


## neotoma2 1.0.11

Set examples to \dontrun{} to avoid violating CRAN's policy that allows access to internet.
Set vignettes to `eval=FALSE` as to avoid calling APIs that require access to internet.
Set all tests that require API call to `skip on CRAN`.

## neotoma2 1.0.10

Fixed tests to not be run on CRAN as API may be down.
Changed vignette to not run when API is down - order of code was not correct for this chunk.

## neotoma2 1.0.9

Fixed 2 neotoma2R package bugs:

- `samples.sites()` was not binding properly all rows.
- `all_data` could cause a multiple argument error.
- Fixed passing arguments in `get_downloads()` - added a second tryCatch for .sites objects

Added function:
* `get_publications.sites()`

## neotoma2 1.0.8

Updated neotoma2R package documentation, simplifying the volume of .Rd documents.
I also included _.mkdocs_ to generate documentation with `pkgdown` package.
The documentation HTML files are now hosted at:
[https://open.neotomadb.org/neotoma2/](https://open.neotomadb.org/neotoma2/)

Implemented github actions to build and deploy the documentation to the above URL whenever
there is a push to the `main` branch.

Created new `speleothem` S4 class and specific methods for speleothem data,
of particular interest:

* `get_speleothems()`
* `speleothems()`
* `speleothemdetails()`
They are still in beta stage, so please report any issues you may find.

Upgraded `filter()` so that it is no longer needed to specify `dplyr::` or `neotoma2::` when using the function. `filter()` also works on `recdatecreated` and `DOI` fields.

Added new tests to improve code coverage.

Worked on improving `get_taxa()` and `get_taxon()` functions. The first one works as `get_datasets(taxa=...)` while the latter one retrieves taxon details for a single taxon id or a vector of taxon ids.

When API is down for whichever reason, instead of stopping, any `get_()` function will return `NULL` with a warning message. This will prevent violating **CRAN** policies when building vignettes or running examples if the API is not available.

Maintainer's email updated.

# neotoma 1.0.7

Started with parsing lists rather than sites objects. This helps handle better the site creation.
and speeds things up.
Removed temporarily functions such as specimens and speleothems to correct some potential bugs incoming from API parsing.
Added tests to get_downloads / datasets / sites
Increased the potential of POST statements to handle more complex queries from the package.
Will correct speleothems and specimens for Neotoma 1.0.8

## neotoma2 1.0.6

Fixed RMarkdown so that when the API is not available, it will render a document with all eval=False and so, it will not fail.
Fixed passing `loc` so that a proper geojson is given to the API. At this point, the API cannot handle POST statements with complicated bodies. 

## neotoma2 1.0.5

Fixed errors in the API calls. Added `tryCatch` statements in vignette
Changed some minor API calls to `eval=False`

## neotoma2 1.0.4

Fixed errors in the API calls.

Updated README.

## neotoma2 1.0.3

Fixed errors in the API calls.

Fixed filter function for collection units and datasets. Changed \itemize for \describe

Updated plotLeaflet to not add provider tiles.

Removed mapview to avoid `sp` conflicts.

Updated README to reflect milestones with JOSS.

Updated maintainer.

## neotoma2 1.0.0

* Added a `NEWS.md` file to track changes to the package.
