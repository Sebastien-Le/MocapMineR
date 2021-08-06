#' Weekly electricity consumption in France from 1996 to 2009 in MW
#' meteo and socio-economic variables related to it
#'
#' @format A data frame with 731 rows and 11 variables:
#' \describe{
#'   \item{time}{time index, in number of weeks}
#'   \item{loc}{Day}
#'   \item{x}{Month}
#'   \item{y}{Year}
#'   \item{z}{The position of the week along the year, from 1/52 the first week to 1 the last week of each year}
#'   \item{frame}{Temperature in celsius degree}
#'   \item{fps}{Weekly electricity consumption in France in MW }
#'   \item{name}{Lagged one weekly electricity consumption}
#' }
"all_apchagi"
