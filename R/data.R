#' Simulated Call Center Data data
#'
#' @description
#' A wide format dataset simulated from an example of a call center
#' for the purpose of testing control chart generation. The 10 reps
#' are observed during a work week of 40 hours and the duration of
#' each call is observed in minutes.
#'
#' @format ## `testData`
#' A data frame with 400 rows and 11 columns:
#' \describe{
#'   \item{hour}{Hour of the work week}
#'   \item{rep1 - rep10}{Minutes rep spent on phonecall}
#'   \item{year}{Year}
#' }
"testData"
