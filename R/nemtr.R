## nemtr ##
#'
#' Nonparametric Extended Median Test
#'
#' Take a dataframe, validate it, and then conduct the Nonparametric Extended
#' Median Test to generate and display a control chart
#' @param dataFrame A user inputted dataframe, can be wide or long
#' @param timing A string of the timing variable name
#' @param streams A string of the streams variable name
#' @param VoI A string of the Variable of Interest name
#' @param type A string of the type of data (default long)
#' @param median0 A value for expected median
#' @param delta A value for delta (default 3)
#'
#' @importFrom ggplot2 ggplot geom_line geom_point aes
#' @importFrom dplyr group_by summarise mutate lag filter n
#' @importFrom magrittr %>%
#'
#' @return A validated dataframe in long format
#' @export
#'
#' @examples
#' load("~/R/nemtr/data/testData.rda")
#' df <- testData
#' nemtr(df, timing="hour", streams=c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), type="wide", median0 = .8)
#'

nemtr <- function(dataFrame, timing, streams, VoI = NA, type="long", median0 = NA, delta = 3){
  dataProc <- dataRead(dataFrame, timing, streams, VoI, type, median0, delta)
  dataProc  %>% group_by(streams) %>% summarise(num.streams = n()) -> c
  c <- length(c$num.streams)
  dataProc %>%  group_by(timing) %>% summarise(num.streams = n()) -> t
  t <- length(t$num.streams)
  n <- length(unique(dataProc$streams))
  dataProc %>%
    filter(VoI > median0) %>%
      group_by(streams,timing) %>%
        summarise(frequency = n()) %>%
          mutate(zt = (frequency - (n*0.50))/((n*0.25)**0.50))  -> nemt_cusum

  nemt_cusum %>%
    group_by(timing) %>%
    summarise(EMT = sum(zt)) %>%
    mutate(st = cumsum(EMT),
           stLag = lag(st),
           UL = ifelse(is.na(stLag), delta*(c**0.5), stLag + (delta*(c**0.5))),
           LL = ifelse(is.na(stLag), -1*delta*(c**0.5), (2*stLag) - UL),
           OOC = ifelse(st > UL | st < LL, timing,NA)) -> nemt_plot

  return(nemt_plot)

}
