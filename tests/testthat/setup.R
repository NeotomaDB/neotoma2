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
