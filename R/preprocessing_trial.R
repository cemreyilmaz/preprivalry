# ---------------------------------------------------------------------------- #
#' Preprocessing of key events in a trial
#'
#' This function preprocesses the key events in a given trial.
#' See also \link{extract_trialkey}
#'
#' @param trial_key data.frame -- contains the key-events in given trial
#' @param trial data.frame -- contains trial info
#' @param percept_keys matrix -- contains the keycodes for each percept
#'
#' @return list -- contains the onset and duration info for each percept by keycode
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessing_trial(trial_key,trial,percept_keys)
#' }
preprocessing_trial <- function(trial_key,trial,percept_keys){
  trial_data <- list()
  percept_keys <- as.matrix(unlist(unique(trial_key['idDown'])))
  for(p in 1:length(percept_keys)){
    theKey <- percept_keys[p]
    perceptStartTime <- trial_key[trial_key['idDown']==theKey,]
    perceptStartTime <- perceptStartTime$timeDown
    perceptEndTime   <- trial_key[trial_key['idUp']==theKey,]
    perceptEndTime   <- perceptEndTime$timeUp
    trial_data[[p]]  <- data.frame(key  = theKey,
                                   onset = perceptStartTime - as.numeric(unlist(trial[1])),
                                   duration = c(perceptEndTime - perceptStartTime))
  }
  return(trial_data)
}
# ---------------------------------------------------------------------------- #
