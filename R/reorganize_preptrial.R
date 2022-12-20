# ---------------------------------------------------------------------------- #
#' Reorganizing the preprocessed data of the given trial
#'
#' This function merges and sorts the key-events in a trial. The output is ready
#' to be analyzed.
#'
#' @param key data.frame -- contains the preprocessed key-events of dominant
#'     percepts in a trial
#' @param trial matrix -- contains trial info
#'
#' @return data.frame -- contains the preprocessed key-events of dominant
#'     percepts and transition phases in a trial
#' @export
#'
#' @examples
#' \dontrun{
#' reorganize_preptrial(trial_key,trial)
#' }
reorganize_preptrial <- function(key,trial){
  key <- data.frame(
    idDown   = c(key$idDown, numeric(length(key$timeUp)-1)),
    timeDown = c(key$timeDown, key$timeUp[1:(length(key$timeUp)-1)])- as.numeric(unlist(trial[1])),
    idUp   = c(key$idUp, numeric(length(key$timeUp)-1)),
    timeUp   = c(key$timeUp, key$timeDown[2:length(key$timeDown)])- as.numeric(unlist(trial[1])))
  key <- key[order(key$timeDown),]
}
# ---------------------------------------------------------------------------- #
