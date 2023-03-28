# ---------------------------------------------------------------------------- #
#' Preprocessing of key events in a trial
#'
#' This function preprocesses the key events in a given trial.
#' See also \link{extract_trialkey}
#'
#' @param trial_key data.frame -- contains the key-events in given trial
#' @param zero_point numeric -- time stamp of the start time
#' @param percept_keys matrix -- contains the keycodes for each percept
#'
#' @return list -- contains the onset and duration info for each percept by keycode
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessing_trial(trial_key,zero_point,percept_keys)
#' }
preprocessing_trial <- function(trial_key,zero_point,percept_keys){
  trial_data <- list()
  percept_keys <- as.matrix(unlist(unique(trial_key['idDown'])))
  if(length(percept_keys)==0){
    trial_data[[p]]  <- data.frame(key  = NaN,
                                   onset = NaN,
                                   duration = NaN)
  } else {
    for(p in 1:length(percept_keys)){
      theKey <- percept_keys[p]
      perceptStartTime <- trial_key[trial_key['idDown']==theKey,]
      perceptStartTime <- perceptStartTime$timeDown
      perceptEndTime   <- trial_key[trial_key['idUp']==theKey,]
      perceptEndTime   <- perceptEndTime$timeUp
      trial_data[[p]]  <- data.frame(key  = theKey,
                                     onset = perceptStartTime - zero_point,
                                     duration = c(perceptEndTime - perceptStartTime))
    }
  }
  return(trial_data)
}
# ---------------------------------------------------------------------------- #
