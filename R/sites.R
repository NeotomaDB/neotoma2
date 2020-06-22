#' An S4 class for multi-site information from the Neotoma Paleoecology Database.
#' @include site.R
#' @importFrom sf sf

sites <- setClass("sites",
  representation(sites = "site"))
