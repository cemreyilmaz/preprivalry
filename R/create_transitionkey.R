# ---------------------------------------------------------------------------- #
#' Creating key events for transition phase
#'
#' This function creates a series of key event with a key code 0 for the duration
#' in which participant don't press any key. Such periods are counted as
#' transition phases in binocular rivalry experiment. This approach is valid
#' only when the participant is instructed to press and hold a key during every
#' dominant percept.
#'
#' @note This function can be used only if the experiment includes
#'     a 'press-and-hold' type of binocular rivalry paradigm.
#' @note The output is only the key events for transition. It must be defined
#'     as a new field in key list or as a new variable.
#'
#' @param key data.frame -- contains the preprocessed key-events in a trial
#' @param zero_point numeric -- time stamp of the start time
#'
#' @return data.frame -- contains the onset and duration of each transition
#'     period with a label of key-code 0
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_transitionkey(trial_key)
#' }
create_transitionkey <- function(key,zero_point){
  data <- data.frame(key   = 0,
                     onset = key$timeUp[1:(length(key$timeUp)-1)] - zero_point),
                     duration = key$timeDown[2:length(key$timeDown)] - key$timeUp[1:(length(key$timeUp)-1)])
}
# ---------------------------------------------------------------------------- #
