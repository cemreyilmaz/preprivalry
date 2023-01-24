library(testthat)
library(preprivalry)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
a <- c()
b <- c()
a[[1]] <- data.frame(key = c(114, 114, 114, 114, 114),
                                 onset = c(8.42, 10.73, 15.51, 20.23, 35.53),
                                 duration = c(1.91, 0.63, 0.55, 2.56, 1.36))
a[[2]] <- data.frame(key = c(115, 115, 115),
                                 onset = c(12.44, 17.47, 31.47),
                                 duration = c(0.85, 0.97, 3.20))
a[[3]] <- data.frame(key = c(0, 0, 0, 0, 0, 0, 0, 0),
                                 onset = c(10.33, 11.37, 13.30, 16.06, 18.44, 22.80, 34.68, 36.89),
                                 duration = c(0.40, 1.07, 2.20, 1.40, 1.79, 8.67, 0.84, 2.21))
b[[1]] <- data.frame(key = c(115, 115, 115, 115),
                                 onset = c(149.12, 153.22, 158.71, 163.13),
                                 duration = c(1.67, 1.76, 0.67, 3.67))
b[[2]] <- data.frame(key = c(114, 114, 114, 114),
                                 onset = c(155.81, 161.28, 167.93, 169.90),
                                 duration = c(2.52, 0.62, 1.01, 0.54))
b[[3]] <- data.frame(key = c(0, 0, 0, 0, 0, 0, 0),
                                 onset = c(150.80, 154.98, 158.34, 159.38, 161.91, 166.81, 168.94),
                                 duration = c(2.41, 0.83, 0.36, 1.89, 1.22, 1.12, 0.95))
run_data <- list(a,b)

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
                                  nameDown = c("RightArrow","RightArrow","RightArrow","LeftArrow","LeftArrow")), zero_point = 0)

))})

test_that("descriptive_eye returns matrix", {expect_true(is.matrix(
  descriptive_eye(run_data)
))})

test_that("descriptive_trial returns matrix", {expect_true(is.matrix(
  descriptive_trial(run_data)
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
  eye_info(run_data)
))})

test_that("preprocessing_run returns a list", {
  expect_true(is.list(run_data))
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
  rivalry.eye_dominance(run_data)
))})
