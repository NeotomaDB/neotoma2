library(httptest)
# Recorded API responses (and the hand-made parser fixtures) live under
# tests/testthat/fixtures/. Pointing httptest's mock path there keeps every
# `with_mock_api()` recording and replay in one folder instead of scattering a
# `api.neotomadb.org/` tree at the testthat root.
httptest::.mockPaths(testthat::test_path("fixtures"))

# Shared test fixtures, sourced once by testthat before any test file runs.
# Single source of truth for the Brazil polygon used across several spatial
# tests (previously duplicated inline, and in two files mislabelled as
# `europe_json`). Defining it once keeps the queries consistent and avoids
# re-typing the geometry in every file.
brazil_json <- '{"type": "Polygon",
            "coordinates": [[
                [-73.125, -9.102],
                [-56.953, -33.138],
                [-36.563, -7.711],
                [-68.203, 13.923],
                [-73.125, -9.102]
              ]]}'
brazil_sf <- geojsonsf::geojson_sf(brazil_json)

# Guard for live contract tests that hit the API's *spatial* endpoints. These
# are not mocked (their purpose is to prove the live API still works), but the
# spatial POST queries flap, and a timeout should not turn CI red. The probe
# issues a tiny spatial query (loc = brazil, limit = 1) with a short timeout and
# skips the test when it fails, so the test still runs (and catches real
# regressions) whenever the spatial endpoint is healthy. A plain GET is a poor
# probe here: during a flap simple GETs succeed while spatial POSTs fail, which
# is exactly the failure mode these tests hit. Also skips on CRAN, so it
# replaces skip_on_cran() in the tests that use it.
skip_if_api_unreachable <- function(seconds = 12) {
  testthat::skip_on_cran()
  old_t <- Sys.getenv("NEOTOMA_TIMEOUT", unset = NA)
  old_r <- Sys.getenv("NEOTOMA_RETRIES", unset = NA)
  Sys.setenv(NEOTOMA_TIMEOUT = seconds, NEOTOMA_RETRIES = 1)
  on.exit({
    if (is.na(old_t)) Sys.unsetenv("NEOTOMA_TIMEOUT") else
      Sys.setenv(NEOTOMA_TIMEOUT = old_t)
    if (is.na(old_r)) Sys.unsetenv("NEOTOMA_RETRIES") else
      Sys.setenv(NEOTOMA_RETRIES = old_r)
  })
  ok <- tryCatch({
    suppressWarnings(get_datasets(loc = brazil_json[1], limit = 1))
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) testthat::skip("Neotoma API (spatial endpoint) unreachable")
}
