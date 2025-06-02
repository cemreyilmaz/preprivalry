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
  log_data <- rivdata[["log"]]
  y <- 1:length(log_data)
  x <- y[rownames(log_data) == "exp"]
  trialStartTime <- t(log_data[[x]][[1]]) # trialStartTime
  trialEndTime   <- t(log_data[[x]][[2]]) # trialEndTime
  exp <- data.frame(trialStartTime,trialEndTime)
}
# ---------------------------------------------------------------------------- #
