## ReadR Tests ##

if(!require(testthat)){
  intall.packages("testthat")
  library(testthat)
}

if(!require(tidyr)){
  intall.packages("tidyr")
  library(tidyr)
}

x <- read.csv("./testData.csv")
xl <- pivot_longer(x, cols = c("rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9", "rep10"), names_to = "rep", values_to = "time")
xlshort <- xl[c(-145),]
write.csv(xl, "./longTestData.csv")
xlerrorSNA <- read.csv("./longTestDataErrorStreamMissing.csv")
xlerrorTNA <- read.csv("./longTestDataErrorMissingTime.csv")
xlerrorTCH <- read.csv("./longTestDataErrorCharTime.csv")
xlerrorHNA <- read.csv("./longTestDataErrorMissingHour.csv")
xlerrorHCH <- read.csv("./longTestDataErrorCharHour.csv")


## function(dataFrame, timing, streams, VoI = NA, type="long", median0 = NA, delta = 3) ##


test_that("dataRead program works", {
    source("./R/dataRead.R")
    expect_message(dataRead(x, timing=x$hour, streams=c(x$rep1,x$rep2,x$rep3), type="wide"), "Data is wide")      # Check for wide data
    expect_error(dataRead(x, streams=c(x$rep1,x$rep2,x$rep3), type="wide"), "Input dataframe needs timing variable")            # Check missing time variable
    expect_error(dataRead(x, x$hour, type="wide"), "Input dataframe needs stream identifier variable")          # Check for stream identifier
    expect_error(dataRead(xlshort, xlshort$time, xlshort$hour, streams=xlshort$rep, type="long"), "Input dataframe needs equal sample count")       # Check for equal sample count in long data
    expect_error(dataRead(x, x$hour, streams=(list(x$rep1, x$rep10[-1])), type="wide"), "Input dataframe needs equal sample count")             # Check for equal sample count in wide data
    #expect_error(dataRead(xlerror1, xlerror1$hour, streams=xlerror1$rep, xlerror1$time, type="long"), "Input variable of interest is non-numeric or has missing values")  #Missing value test
    expect_error(dataRead(xlerrorSNA, xlerrorSNA$hour, streams=xlerrorSNA$rep, xlerrorSNA$time, type="long"), "Input stream variable has missing values")  #Char value test
    expect_error(dataRead(xlerrorHNA, xlerrorHNA$hour, streams=xlerrorHNA$rep, xlerrorHNA$time, type="long"), "Input time variable has missing values")  #Missing value test ########
    expect_error(dataRead(xlerrorHCH, xlerrorHCH$hour, streams=xlerrorHCH$rep, xlerrorHCH$time, type="long"), "Input time variable is non-numeric")  #Char value test ########
    expect_error(dataRead(xlerrorTNA, xlerrorTNA$hour, streams=xlerrorTNA$rep, xlerrorTNA$time, type="long"), "Input variable of interest has missing values")  #Missing value test Target
    expect_error(dataRead(xlerrorTCH, xlerrorTCH$hour, streams=xlerrorTCH$rep, xlerrorTCH$time, type="long"), "Input variable of interest is non-numeric")  #Char value test Target
    expect_error(dataRead(xl, xl$time, xl$hour, streams=xl$rep, type="long"), "Target median is missing")             # Check for target median
    expect_error(dataRead(xl, xl$time, xl$hour, streams=xl$rep, type="long", median0 = "hello"), "Inputted target median is non-numeric")             # Check for target median numeric
    expect_error(dataRead(xl, xl$time, xl$hour, streams=xl$rep, type="long", median0 = 3), "Please input a target median less than 1")             # Check for target median less than 1
    expect_error(dataRead(xl, xl$time, xl$hour, streams=xl$rep, type="long", median0 = -1), "Please input a target median greater than 0")             # Check for target median greater than 0
    expect_error(dataRead(xl, xl$time, xl$hour, streams=xl$rep, type="long", median0 = .8, delta = "hello"), "Inputted target delta is non-numeric")             # Check for delta numeric
    expect_equal(x, dataRead(x, x$hour, streams=c(x$rep1,x$rep2,x$rep3), type="wide"))             # Check for wide output dataframe
    expect_equal(xl, dataRead(xl, xl$time, xl$hour, streams=xl$rep, type="long", median0 = .8))             # Check for long output dataframe
})
dataRead(xlshort, xlshort$hour, streams=xlshort$rep, type="long")
