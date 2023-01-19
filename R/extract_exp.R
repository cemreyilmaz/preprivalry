# ---------------------------------------------------------------------------- #
#' Extracting the details of an experimental run
#'
#' This function rearranges the binocular rivalry data which is the output of
#' \link{read_rivdata}, and creates a data frame including the start and end time
#' of trials.
#'
#' @param rivdata list -- the raw data
#'
#' @return data.frame -- the start and end time of trials
#'
#' @export
#'
#' @examples
#' \dontrun{
#' exp <- extract_exp(read_rivdata('~/preprivalry/tests','RivalryGratings','s001','session1'))
#' }
extract_exp <- function(rivdata){
  trialStartTime <- t(rivdata[["log"]][[5]][[1]]) # trialStartTime
  trialEndTime   <- t(rivdata[["log"]][[5]][[2]]) # trialEndTime
  exp <- data.frame(trialStartTime,trialEndTime)
}
# ---------------------------------------------------------------------------- #
