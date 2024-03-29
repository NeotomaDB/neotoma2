% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_body.R
\name{parsebody}
\alias{parsebody}
\title{parse_body}
\usage{
parsebody(x, all_data, ...)
}
\arguments{
\item{x}{The HTTP path for the particular API call.}

\item{all_data}{recovers all_data parameter to decide how to handle downloads
lists that would result in a 414 error.}

\item{...}{Any query parameters passed from the function calling}
}
\value{
\code{JSON} object to parse as a body in a HTTP request
}
\description{
An internal helper function to parse the body of POST API requests
}
\author{
Socorro Dominguez
}
\keyword{internal}
