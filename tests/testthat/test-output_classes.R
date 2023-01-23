library(testthat)
library(preprivalry)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

setwd("..")
directory   <- paste0(getwd(),"/data") # the folder which contains the data
experiments <- c("Gratings","Images","GratingImage")
subjects    <- c("s001","s002","s003")
sessions    <- 1:3
runs        <- 1:3

test_that("clean_keyevents returns a data.frame", {expect_true(is.data.frame(
  clean_keyevents(data.frame(idUp = c(114,114,114,115,115),
                      timeUp = c(0.1,0.13,0.18,0.3,0.5),
                      nameUp = c("RightArrow","RightArrow","RightArrow","LeftArrow","LeftArrow"),
                      idDown = c(114,114,114,115,115),
                      timeDown = c(0,0.11,0.16,0.25,0.45),
                      nameDown = c("RightArrow","RightArrow","RightArrow","LeftArrow","LeftArrow")),
    2)
))})

test_that("create_nan_trial_data returns a list", {expect_true(is.list(
  create_nan_trialdata(c(114,115))

))})

test_that("create_transitionkey returns a data.frame", {expect_true(is.data.frame(
  create_transitionkey(data.frame(idUp = c(114,114,114,115,115),
                                  timeUp = c(0.1,0.13,0.18,0.3,0.5),
                                  nameUp = c("RightArrow","RightArrow","RightArrow","LeftArrow","LeftArrow"),
                                  idDown = c(114,114,114,115,115),
                                  timeDown = c(0,0.11,0.16,0.25,0.45),
                                  nameDown = c("RightArrow","RightArrow","RightArrow","LeftArrow","LeftArrow"))), zero_point  = 0

))})

test_that("descriptive_eye returns matrix", {expect_true(is.matrix(
  descriptive_eye(preprocessing_run(directory,experiments[1],subjects[1],sessions[1]))
))})

test_that("descriptive_trial returns matrix", {expect_true(is.matrix(
  descriptive_trial(preprocessing_run(directory,experiments[1],subjects[1],sessions[1]))
))})

test_that("extract_exp returns data.frame", {expect_true(is.data.frame(
  extract_exp(read_rivdata(directory,experiments[1],subjects[1],sessions[1]))
))})

test_that("extract_key returns data.frame", {expect_true(is.data.frame(
  extract_key(read_rivdata(directory,experiments[1],subjects[1],sessions[1]))
))})

exp<-extract_exp(read_rivdata(directory,experiments[1],subjects[1],sessions[1]))
key<-extract_key(read_rivdata(directory,experiments[1],subjects[1],sessions[1]))
test_that("extract_trialkey returns a list", {
  expect_true(is.data.frame(extract_trialkey(key,exp[1,])))
})

test_that("eye_info returns list", {expect_true(is.list(
  eye_info(preprocessing_run(directory,experiments[1],subjects[1],sessions[1]))
))})

test_that("preprocessing_run returns a list", {
  expect_true(is.list(preprocessing_run(directory,experiments[1],subjects[1],sessions[1])))
})

test_that("preprocessing_session returns a list", {
  expect_true(is.list(preprocessing_session(directory,experiments[1],subjects[1],sessions[1])))
})

test_that("preprocessing_subject returns a list", {
  expect_true(is.list(preprocessing_subject(directory,experiments[1],subjects[1],sessions[3])))
})

test_that("read_riv returns a list", {
  expect_true(is.list(read_rivdata(directory,experiments[1],subjects[1],sessions[1])))
})

test_that("remove_irrelevant_keyevents returns data.frame", {expect_true(is.data.frame(
  remove_irrelevant_keyevents(data.frame(idUp = c(114,114,114,115,115),
                                         timeUp = c(0.1,0.13,0.18,0.3,0.5),
                                         nameUp = c("RightArrow","RightArrow","RightArrow","LeftArrow","LeftArrow"),
                                         idDown = c(114,114,114,115,115),
                                         timeDown = c(0,0.11,0.16,0.25,0.45),
                                         nameDown = c("RightArrow","RightArrow","RightArrow","LeftArrow","LeftArrow")),
                              c(114,115))
))})

test_that("reorganize_as_table returns a data.frame", {
  expect_true(is.data.frame(reorganize_as_table(preprocessing_subject(directory,experiments[1],subjects[1],sessions[3]))))
})

test_that("replay_score returns numeric", {expect_true(is.numeric(
  replay_score(rescale_replay_data(directory, experiments[1], subjects[1], sessions[3]))
))})

test_that("rescale_replay_data returns data.frame", {expect_true(is.data.frame(
  rescale_replay_data(directory, experiments[1], subjects[1], sessions[3])
))})

test_that("rivalry.eye_dominance returns numeric", {expect_true(is.numeric(
  rivalry.eye_dominance(preprocessing_run(directory,experiments[1],subjects[1],sessions[1]))
))})
