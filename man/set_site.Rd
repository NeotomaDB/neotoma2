% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_site.R
\name{set_site}
\alias{set_site}
\title{set Site Information for Fossil Sites}
\usage{
set_site(
  sitename = NA_character_,
  coordinates = st_as_sf(st_sfc()),
  description = NA_character_,
  notes = NA_character_,
  collunits = new("collunits"),
  altitude = NA_integer_
)
}
\arguments{
\item{sitename}{actual site name}

\item{coordinates}{sf object}

\item{description}{description of site}

\item{notes}{additional information of the site}

\item{collunits}{collection units in the site}

\item{altitude}{altitude/elevation of the site}

\item{siteid}{site unique identificator if available}
}
\description{
set Site Information for Fossil Sites
}
\examples{
\dontrun{
x = st_as_sf(st_sfc(st_point(c(5,5))))
my_site <- set_site(sitename="My Lake", coordinates = x,
                    description = "my lake", altitude = 30)
                    }
}
