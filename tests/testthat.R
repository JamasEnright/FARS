library(testthat)
library(FARS)

expect_that(fars_read("accident_2015.csv.bz2"), is_a("tbl"))
