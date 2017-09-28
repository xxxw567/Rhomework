library(testthat)
library(Rhomework)
test_that("expect_that work successfully",
          expect_that(make_filename(2017), equals("accident_2017.csv.bz2"))
)
