library(testthat)
library(preprivalry)

test_that("read_riv returns a list", {
  expect_true(is.list(read_rivdata('','RivalryGratings',1,1)))
})

test_that("extract_key returns a list", {
  expect_true(is.data.frame(extract_key(read_rivdata('','RivalryGratings',1,1))))
})

test_that("extract_exp returns a list", {
  expect_true(is.data.frame(extract_exp(read_rivdata('','RivalryGratings',1,1))))
})

test_that("preprocessing_run returns a list", {
  expect_true(is.list(preprocessing_run('','RivalryGratings',1,1)))
})

test_that("preprocessing_session returns a list", {
  expect_true(is.list(preprocessing_session('','RivalryGratings',1,1)))
})

test_that("preprocessing_subject returns a list", {
  expect_true(is.list(preprocessing_subject('','RivalryGratings',1,1)))
})

test_that("reorganize_as_table returns a data.frame", {
  expect_true(is.data.frame(reorganize_as_table(preprocessing_subject('','RivalryGratings',1,1))))
})
