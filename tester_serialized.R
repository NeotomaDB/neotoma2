

logfile <- "tests_071726"

sink(logfile, append = TRUE, split = TRUE)

testes = c(
  "tests/testthat/test_add_chronology.R",
  "tests/testthat/test_c.R",
  "tests/testthat/test_chroncontrols.R",
  "tests/testthat/test_chronologies.R",
  "tests/testthat/test_cite_data.R",
  "tests/testthat/test_clean.R",
  "tests/testthat/test_collunits.R",
  "tests/testthat/test_coordinates.R",
  "tests/testthat/test_datasets.R",
  "tests/testthat/test_doi.R",
  #"tests/testthat/test_examples.R",
  "tests/testthat/test_filter.R",
  "tests/testthat/test_get_contacts.R",
  "tests/testthat/test_get_datasets.R",
  "tests/testthat/test_get_documentation.R",
  "tests/testthat/test_get_downloads.R",
  "tests/testthat/test_get_manual.R",
  "tests/testthat/test_get_publications.R",
  "tests/testthat/test_get_sites.R",
  #"tests/testthat/test_get_speleothems.R",
  "tests/testthat/test_get_stats.R",
  "tests/testthat/test_get_table.R",
  "tests/testthat/test_get_taxa.R",
  "tests/testthat/test_get_taxon.R",
  "tests/testthat/test_getids.R",
  "tests/testthat/test_length.R",
  "tests/testthat/test_parse_location.R",
  "tests/testthat/test_pingNeotoma.R",
  "tests/testthat/test_plot.R",
  "tests/testthat/test_plotLeaflet.r",
  "tests/testthat/test_samples.R",
  "tests/testthat/test_set_chronology.R",
  "tests/testthat/test_set_collunit.R",
  "tests/testthat/test_set_contact.R",
  "tests/testthat/test_set_dataset.R",
  "tests/testthat/test_set_default.R",
  "tests/testthat/test_set_publication.R",
  "tests/testthat/test_set_sample.R",
  "tests/testthat/test_set_server.R",
  "tests/testthat/test_set_site.R",
  #"tests/testthat/test_set_speleothem.R",
  "tests/testthat/test_sites.R",
  #"tests/testthat/test_specimens.R",
  #"tests/testthat/test_speleothems.R",
  "tests/testthat/test_summary.R",
  "tests/testthat/test_taxa.R",
  "tests/testthat/test_toWide.R"
)

for (i in testes) {
  print(i)
  source(i)
  Sys.sleep(0.5)
}
sink()