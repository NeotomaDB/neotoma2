library("testthat")
library("neotoma2")

context("`samples()` retrieves a data.frame of all data.")
test_that("`samples` retrieve df.", {
  skip_on_cran()
  dl <- get_downloads(4716) %>% samples()
  singlechron <- testthat::expect_true({dl; TRUE})
  # expect the return is a data frame
  testthat::expect_is(dl, "data.frame")
  testthat::expect_gt(nrow(dl), 0)
  # expect at least one row
  
  dl2 <- get_downloads(21007) %>% samples()
  testthat::expect_true({dl2; TRUE})
  testthat::expect_is(dl2, "data.frame")
  testthat::expect_gt(nrow(dl2), 0)
})

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
    ggplot2::geom_path(ggplot2::aes(x = age,
                                    y = count,
                                    color = ecologicalgroup))
  testthat::expect_is(aa, "gg")
})

# Test that no duplicated sampleids exist for a variety of datasetids
# c(48891, 52833 52742) dups
datasetids <- c(41625, 46798, 48891, 47771,
                41620, 46689, 52833, 48756, 
                52742, 46700, 47608, 46841, 
                49238)
for (i in datasetids) {
  test_that(paste0("Duplicated sampleids for Dataset ID, ",
                   i, " don't exist (in the APD)"), {
                     skip_on_cran()
                     L <- get_datasets(i) %>%
                       get_downloads()
                     my_counts <- samples(L)
                     assertthat::assert_that(!any(duplicated(my_counts)))
                     counts <- my_counts %>%
                       mutate(age_chr = ifelse(is.na(age), "__NA__",
                                               as.character(age))) %>%
                       pivot_wider(
                         id_cols = age_chr,
                         names_from = variablename,
                         values_from = value,
                         values_fn = sum,
                         values_fill = NA
                       ) %>%
                       mutate(age = as.numeric(dplyr::na_if(age_chr, "__NA__"))) %>%
                       select(-age_chr)
                     
                     counts <- counts%>%
                       tryCatch(.data,
                                error = function(e) e,
                                warning = function(w) w)
                     testthat::expect_false(any(duplicated(my_counts)))
                     testthat::expect_false(is(counts, "warning"))
                   })
}

test_that("Samples of all sites has the same nrow as samples of each site combined", {
  skip_on_cran()
  si <- get_sites(limit=3)
  dl <- get_downloads(si)
  df1 <- nrow(samples(dl[[1]]))
  df2 <- nrow(samples(dl[[2]]))
  df3 <- nrow(samples(dl[[3]]))
  
  all_df <- nrow(samples(dl))
  testthat::expect_equal(all_df, df1 + df2 + df3)
})