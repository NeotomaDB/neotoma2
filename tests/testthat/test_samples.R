library("testthat")
library("neotoma2")
library("httptest")

context("`samples()` retrieves a data.frame of all data.")

# Only the first test is mocked (small downloads 4716 + 21007). The rest pull
# large downloads (0.9-1 MB each, plus a 13-dataset loop) that are too big to
# ship as fixtures, so they stay live (CI runs them against api-dev).
httptest::with_mock_api({
  test_that("`samples` retrieve df.", {
    skip_on_cran()
    dl <- get_downloads(4716) %>% samples()
    singlechron <- testthat::expect_true({
      dl
      TRUE
    })
    # expect the return is a data frame
    testthat::expect_is(dl, "data.frame")
    testthat::expect_gt(nrow(dl), 0)
    # expect at least one row

    dl2 <- get_downloads(21007) %>% samples()
    testthat::expect_true({
      dl2
      TRUE
    })
    testthat::expect_is(dl2, "data.frame")
    testthat::expect_gt(nrow(dl2), 0)
  })

  # 4716 and 21007 both carry "Syverson-Blois: bounds"; only 4716 has
  # "FAUNMAP 1.1", so asking for it makes 21007 fall back to its default.
  test_that("`samples(chronname=)` flags collection units that lack it.", {
    skip_on_cran()
    both <- c(get_downloads(4716), get_downloads(21007))

    # Without chronname the output is unchanged: no flag column.
    plain <- samples(both)
    testthat::expect_false("chronname_matched" %in% names(plain))

    # A name present everywhere: all matched, and no warning.
    testthat::expect_no_warning(
      same <- samples(both, chronname = "Syverson-Blois: bounds")
    )
    testthat::expect_true(all(same$chronname_matched))
    # Samples without that chronology are dropped, not recycled into extra
    # rows (21007 has samples with no "Syverson-Blois: bounds" age).
    testthat::expect_lte(nrow(same), nrow(plain))
    testthat::expect_false(anyNA(same$chronologyid))

    # A name present in one collection unit only: exactly one warning, that
    # names the dataset, and the flag separates the two.
    warns <- character()
    mixed <- withCallingHandlers(
      samples(both, chronname = "FAUNMAP 1.1"),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    testthat::expect_length(warns, 1)
    testthat::expect_match(warns, "'FAUNMAP 1.1' was not found")
    testthat::expect_match(warns, "21007")
    testthat::expect_false(grepl("4716", warns, fixed = TRUE))
    testthat::expect_true(all(mixed$chronname_matched[mixed$datasetid == 4716]))
    testthat::expect_false(
      any(mixed$chronname_matched[mixed$datasetid == 21007])
    )
    # The fallback rows are the default-chronology rows, same as `plain`.
    testthat::expect_equal(
      mixed$age[mixed$datasetid == 21007],
      plain$age[plain$datasetid == 21007]
    )
  })
}) # end with_mock_api

test_that("Get the samples out of dataset 15692.", {
  skip_on_cran()
  df <- get_downloads(15692) %>% samples()
  testthat::expect_gt(nrow(df), 1)
  testthat::expect_is(df, "data.frame")
})

test_that("ggplot2 on samples", {
  skip_on_cran()
  my_datasets <- get_datasets(40945)
  my_sites <- get_downloads(my_datasets)
  my_counts <- neotoma2::samples(my_sites)
  aa <- my_counts %>%
    dplyr::filter(taxongroup == "Vascular plants") %>%
    group_by(age, ecologicalgroup) %>%
    dplyr::summarize(count = sum(value)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_path(ggplot2::aes(
      x = age,
      y = count,
      color = ecologicalgroup
    ))
  testthat::expect_is(aa, "gg")
})

# Test that no duplicated sampleids exist for a variety of datasetids
# c(48891, 52833 52742) dups
datasetids <- c(
  41625, 46798, 48891, 47771,
  41620, 46689, 52833, 48756,
  52742, 46700, 47608, 46841,
  49238
)
for (i in datasetids) {
  test_that(paste0(
    "Duplicated sampleids for Dataset ID, ",
    i, " don't exist (in the APD)"
  ), {
    skip_on_cran()
    dl <- get_datasets(i) %>%
      get_downloads()
    my_counts <- samples(dl)
    assertthat::assert_that(!any(duplicated(my_counts)))
    counts <- my_counts %>%
      mutate(age_chr = ifelse(is.na(age), "__NA__",
        as.character(age)
      )) %>%
      pivot_wider(
        id_cols = age_chr,
        names_from = variablename,
        values_from = value,
        values_fn = sum,
        values_fill = NA
      ) %>%
      mutate(age = as.numeric(dplyr::na_if(age_chr, "__NA__"))) %>%
      select(-age_chr)

    counts <- counts %>%
      tryCatch(.data,
        error = function(e) e,
        warning = function(w) w
      )
    testthat::expect_false(any(duplicated(my_counts)))
    testthat::expect_false(is(counts, "warning"))
  })
}

test_that("Samples of all sites have the same nrow as the sites combined", {
  skip_on_cran()
  si <- get_sites(limit = 3)
  dl <- get_downloads(si)
  df1 <- nrow(samples(dl[[1]]))
  df2 <- nrow(samples(dl[[2]]))
  df3 <- nrow(samples(dl[[3]]))

  all_df <- nrow(samples(dl))
  testthat::expect_equal(all_df, df1 + df2 + df3)
})
