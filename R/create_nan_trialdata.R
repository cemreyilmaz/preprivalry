# ---------------------------------------------------------------------------- #
#' Creating a fake trial data with nan values
#' This function creates a list similar to the output of \link{preprocessing_trial}
#' but only with nan values. This is for the trials with no key event was recorded.
#'
#' @param percept_keys matrix -- contains the keycodes for each percept
#'
#' @return list -- contains the onset and duration info for each percept by keycode
#' @export
#'
#' @examples
#' \dontrun{create_nan_trialdata(percept_keys)}
create_nan_trialdata <- function(percept_keys){
  trial_data <- list()
  for(p in 1:length(percept_keys)){
    theKey <- percept_keys[p]
    perceptStartTime <- NaN
    perceptEndTime   <- NaN
    trial_data[[p]]  <- data.frame(key  = theKey,
                                   onset = perceptStartTime,
                                   duration = perceptEndTime - perceptStartTime)
  }
  trial_data[[length(trial_data)+1]] <- data.frame(key  = 0,
                                                   onset = perceptStartTime,
                                                   duration = perceptEndTime - perceptStartTime)
  return(trial_data)
}
# ---------------------------------------------------------------------------- #
