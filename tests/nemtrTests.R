#' @importFrom testthat test_that expect_equal
#' @importFrom tidyr pivot_longer
#'
#'


# if(!require(testthat)){
#   install.packages("testthat")
#   library(testthat)
# }
#
#
# if(!require(tidyr)){
#   install.packages("tidyr")
#   library(tidyr)
# }
#
# if(!require(tidyverse)){
#   install.packages("tidyverse")
#   library(tidyverse)
# }

load("~/R/nemtr/data/testData.rda")
load("~/R/nemtr/data/testDataLong.rda")
load("~/R/nemtr/data/expOutput.rda")
load("~/R/nemtr/data/nemt_plot.rda")

df <- testData
dfL <- testDataLong

testthat::test_that("nemtr works", {
    source("~/R/nemtr/R/dataRead.R")
    source("~/R/nemtr/R/nemtr.R")
    testthat::expect_equal(nemt_plot, nemtr(testData, timing="hour", streams=c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), type="wide", median0 = 2))
})
