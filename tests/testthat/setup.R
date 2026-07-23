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