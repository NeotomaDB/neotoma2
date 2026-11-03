# News neotoma2 R package

## neotoma2 1.1.1.9000 (development version)

* `samples(chronname = )` now says when it could not use the chronology you asked for. A collection unit that does not have the named chronology falls back to its default chronology, and the result has a new logical column `chronname_matched` (`FALSE` for those rows). One warning, naming the collection units and datasets, replaces the previous one-per-unit warnings, whose multi-line text showed little more than `samples(y, chronname = chronname)`. Without `chronname` the output is unchanged.
* `samples(chronname = )` no longer fails with "arguments imply differing number of rows" when some samples of a collection unit do not carry the requested chronology. Those samples are dropped, as intended.
* `as.data.frame(chronologies(x))` gains `siteid`, `collunitid` and `datasetid`, so each chronology can be matched to its dataset. A chronology belongs to a collection unit and is shared by all of its datasets, so the table has one row per chronology and dataset. `siteid` is `NA` when `x` is a collection unit. `show()` for `chronologies` also prints the collection unit and datasets when they are known.
* Fixes from the review of the 1.1.1 release:
  * `get_downloads()` returns `NULL`, as before, when the API sends no data (1.1.1 returned an empty `sites` object).
  * The session caches of `get_downloads()` and `get_table()` are kept per server, so results fetched before `set_server()` are not returned for another server.
  * `get_taxon(taxonname = , limit = , offset = )` returns the slice asked for, rather than every page, unless `all_data = TRUE` is set.
  * The `toWide()` example queries `"vertebrate fauna"` (a line break had split the string).

## neotoma2 1.1.1

* The default request timeout is 125 seconds, just above the 120 seconds at which the API gateway gives up, so the client and the gateway agree and a slow query  is reported once instead of being abandoned and repeated. `NEOTOMA_TIMEOUT` and `NEOTOMA_RETRIES` still override both.
* `limit` and `offset` now describe the whole answer when a long list of ids is split into several requests. `get_sites(1:1000, limit = 10)` returned 100 sites, because every batch of ids was given the `limit`; it returns 10, from one request.
* Updated API servers (api-dev now, production after its next deploy) change what queries return:
  * Site and dataset totals are lower than before. Sites without datasets and
    deleted datasets are no longer returned, so counts saved from earlier
    sessions, or fixed in scripts and tests, need re-baselining.
  * A list of IDs returns every ID asked for, not the first 25, and `limit` and
    `offset` give the same records whatever the page size. `all_data = TRUE`
    still requests 2000 records per page; set the environment variable
    `NEOTOMA_PAGE` (for example `Sys.setenv(NEOTOMA_PAGE = 500)`) for smaller
    pages. Older servers lose records when walked in small pages, so keep the
    default there.
  * `datasettype` takes several values (`datasettype = c("pollen", "diatom")`)
    in `get_sites()` and `get_datasets()`.
* New fields for records from `get_downloads()`: `collunit@collectors` (a list of
  names) and, on each sample, `analysisunitnotes`, `analysisdate` and `samplenotes`.
  `set_site()` now records `area`, and a collection unit's `gpslocation` is a
  one-row `sf` point, so `sf::st_coordinates()` works on it. The fields are empty
  until the Neotoma database functions that supply them are updated.
* Gentler on the Neotoma API:
  * An HTTP 500 is no longer retried (it cannot succeed on a repeat), and a busy
    gateway (502, 503, 504) is retried once, after a jittered wait of 30 to 60
    seconds. A `Retry-After` header on a 429 or 503 is honoured.
  * Requests identify the package: the User-Agent is now
    `neotoma2/<version> (R <version>)`. Set `options(neotoma2.contact = "you@example.org")`
    or the `NEOTOMA_CONTACT` environment variable to add a contact address.
  * `get_table()` keeps its answer for the rest of the session (use `refresh = TRUE`
    to ask again), and `get_downloads()` requests long vectors of dataset IDs in
    batches of 25 and keeps the answers for the session. `options(neotoma2.cache = FALSE)`
    turns the cache off.
  * `get_downloads()` no longer repeats a failed request with its arguments removed.
  * A long list of IDs for `data/taxa`, `data/publications` or other endpoints
    without a POST route is now sent as several short `GET` requests, instead of a
    `POST` the API answered with a redirect to its documentation.
  * A long `get_downloads()` request that used `all_data = TRUE` sent a malformed
    list of dataset IDs, which the API rejected with an HTTP 500.
  * An answer that is a web page rather than data now raises a clear error.
  * A failed request reports the reason the API gives (`The API said: ...`), in
    both the current response shape (`data: null`, `message`) and the earlier one
    (the error in `data`).
* The development server is now reached over `https`. `http://api-dev...` redirects
  to `https`, and the redirect turned a `POST` into a `GET` without its body, so a
  spatial (`loc`) query sent with `set_server("dev")` came back as the unfiltered
  first page of records. A `POST` now also keeps its method and body through any
  redirect.
* Long lists of IDs for `get_sites()`, `get_datasets()` and `get_downloads()` are
  sent as `POST` requests in chunks of 100, as the API accepts them there, and a
  long list of filter values (for example many taxon names) is sent as a `POST`
  body instead of a URL too long to send. An answer that does not match the IDs
  asked for raises an error instead of being returned.
* `get_taxa()` was rewritten. `get_taxa(<id>)` used to ask for datasets with no
  taxon filter at all; `get_taxa(taxa = )` passed a function in place of the names;
  `all_data`, `datasettype`, `loc`, `limit` and other arguments were dropped or
  ignored. It now forwards them, never requests datasets without a taxon filter,
  and warns before sending more than 25 taxon names in one request.
* `get_taxon(taxonname = "Pinus%")` now returns every page of matches (`all_data`
  defaults to `TRUE`, as documented) instead of only the first, and a taxon record
  without a `taxagroupid` no longer makes it fail. `get_taxon(numeric(0))` returns `NULL`.
* `all_data` is now honoured in `get_datasets()` and `get_downloads()` when called on a
  `sites` object; the argument was previously ignored.

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
* A spatial geometry is now converted once per query rather than once per page,
  and the API's parameter list is fetched once per session rather than on every
  `get_*()` call.
* Paginated queries report their progress in interactive sessions.
* Improved `chronologies()` function to show a data frame instead of the raw format of chronologies data.


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
