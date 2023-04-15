## ----setOpts, include = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(sf)
library(geojsonsf)
library(dplyr)
library(neotoma2)

## ----getSiteBySiteID----------------------------------------------------------
# Search for site by a single numeric ID:
alex <- get_sites(24)
alex

# Search for sites with multiple IDs using c():
multiple_sites <- get_sites(c(24, 47))
multiple_sites

## ----getsitename--------------------------------------------------------------
alex <- get_sites(sitename = "Alexander Lake")
alex

## ----sitewithwildcardname-----------------------------------------------------
alex <- get_sites(sitename = 'Alex%')
alex

## ----agebounds, eval=FALSE----------------------------------------------------
#  # Note, we are using the `all_data = TRUE` flag here to avoid the default limit of 25 records, discussed below.
#  # Because these queries are searching through every record they are slow and and are not
#  # run in knitting this vignette.
#  get_sites(ageof = 8200, all_data = TRUE) %>% length()
#  get_sites(ageyounger = 5000, ageolder = 8000, all_data = TRUE) %>% length()
#  get_sites(minage = 5000, maxage = 8000, all_data = TRUE) %>% length()

## ----extractElement-----------------------------------------------------------
alex <- get_sites(sitename = "Alexander Lake")
alex[[1]]$siteid

## ----showallNamesSite---------------------------------------------------------
names(alex[[1]])

# Modify a value using $<- assignment:
alex[[1]]$area
alex[[1]]$area <- 100
alex[[1]]$area

# Modify a value using [<- assignment:
alex[[1]]["area"] <- 30
# alex[[1]][7] <- 30  This fails because the `Notes` field expects a character string.

## ----setsitefunction----------------------------------------------------------
my_site <- set_site(sitename = "My Lake", 
                    geography = st_sf(a = 3, st_sfc(st_point(1:2))), 
                    description = "my lake", 
                    altitude = 30)
my_site

## ----addtosites---------------------------------------------------------------
# Add a new site that's been edited using set_site()
longer_alex <- c(alex, my_site)
# Or replace an element within the existing list of sites
# with the newly created site.
longer_alex[[2]] <- my_site

# Or append to the `sites` list with assignment:
longer_alex[[3]] <- my_site

## ----getdatasetsbyid----------------------------------------------------------
# Getting datasets by ID
my_datasets <- get_datasets(c(5, 10, 15, 20))
my_datasets

## ----getdatasetsbytype--------------------------------------------------------
# Getting datasets by type
my_pollen_datasets <- get_datasets(datasettype = "pollen", limit = 25)
my_pollen_datasets

## ----all_data, eval=FALSE-----------------------------------------------------
#  allSites_dt <- get_sites(datasettype = "diatom")
#  allSites_dt_all <- get_sites(datasettype = "diatom", all_data = TRUE)
#  
#  # Because we used the `all_data = TRUE` flag, there will be more sites
#  # in allSites_dt_all, because it represents all sites containing diatom datasets.
#  length(allSites_dt_all) > length(allSites_dt)

## ----boundingBox--------------------------------------------------------------
brazil <- '{"type": "Polygon", 
            "coordinates": [[
                [-73.125, -9.102],
                [-56.953, -33.138],
                [-36.563, -7.711],
                [-68.203, 13.923],
                [-73.125, -9.102]
              ]]}'

# We can make the geojson a spatial object if we want to use the
# functionality of the `sf` package.
brazil_sf <- geojsonsf::geojson_sf(brazil)

brazil_datasets <- get_datasets(loc = brazil_sf)
brazil_datasets

## ----leafletBrazil------------------------------------------------------------
plotLeaflet(brazil_datasets)

## ----filterBrazil-------------------------------------------------------------
brazil_dates <- neotoma2::filter(brazil_datasets,
  datasettype == "geochronologic")

# or:

brazil_dates <- brazil_datasets %>%
  neotoma2::filter(datasettype == "geochronologic")

# With boolean operators:

brazil_space <- brazil_datasets %>% neotoma2::filter(lat > -18 & lat < -16)

## ----filterAndShowTaxa--------------------------------------------------------
brazil <- '{"type": "Polygon", 
            "coordinates": [[
                [-73.125, -9.102],
                [-56.953, -33.138],
                [-36.563, -7.711],
                [-68.203, 13.923],
                [-73.125, -9.102]
              ]]}'

# We can make the geojson a spatial object if we want to use the
# functionality of the `sf` package.
brazil_sf <- geojsonsf::geojson_sf(brazil)

brazil_records <- get_datasets(loc = brazil_sf) %>%
  neotoma2::filter(datasettype == "pollen" & age_range_young <= 1000 & age_range_old >= 10000) %>%
  get_downloads(verbose = FALSE)

count_by_site <- samples(brazil_records) %>%
  dplyr::filter(elementtype == "pollen" & units == "NISP") %>%
  group_by(siteid, variablename) %>%
  summarise(n = n()) %>%
  group_by(variablename) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

## ----pubsbyid-----------------------------------------------------------------
one <- get_publications(12)
two <- get_publications(c(12, 14))

## ----showSinglePub------------------------------------------------------------
two[[2]]

## ----fulltestPubSearch--------------------------------------------------------
michPubs <- get_publications(search = "Michigan", limit = 2)

## ----nonsenseSearch-----------------------------------------------------------
noise <- get_publications(search = "Canada Banada Nanada", limit = 5)

## ----getSecondPub-------------------------------------------------------------
two[[1]]

## ----subsetPubs---------------------------------------------------------------
# Select publications with Neotoma Publication IDs 1 - 10.
pubArray <- get_publications(1:10)
# Select the first five publications:
subPub <- pubArray[[1:5]]
subPub

## ----setNewPub----------------------------------------------------------------
new_pub <- set_publications(
  articletitle = "Myrtle Lake: a late- and post-glacial pollen diagram from northern Minnesota",
  journal = "Canadian Journal of Botany",
  volume = 46)

## ----setPubValue--------------------------------------------------------------
new_pub@pages <- "1397-1410"

