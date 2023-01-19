# ---------------------------------------------------------------------------- #
#' Extracting the key events only in the given trial
#'
#' This function extracts the key events from the given trial. It uses the time
#' information to define whether or not the key event belongs to the given trial.
#' See also \link{extract_key}
#'
#' @note The function changes the class of "trial" if it is not numeric.
#' @param key data.frame -- the data related to the key-events
#' @param trial matrix -- trial start and end time
#'
#' @return data.frame -- the data related to the key-events in the given trial
#' @export
#'
#' @examples
#' \dontrun{
#' key <- extract_key(read_rivdata('~/preprivalry/tests','RivalryGratings','s001','session1'))
#' exp <- extract_exp(read_rivdata('~/preprivalry/tests','RivalryGratings','s001','session1'))
#' key <- extract_trialkey(key,exp[1,])
#' }
extract_trialkey <- function(key,trial){
  # the key-events only in this trial
  if(!is.numeric(trial)){
    trial <- as.numeric(unlist(trial))
  }
  id_up <- key$idUp
  time_up <- key$timeUp
  name_up <- key$nameUp
  id_down <- key$idDown
  time_down <- key$timeDown
  name_down <- key$nameDown
  keep_up <- time_up > trial[1] & time_up < trial[2]
  keep_down <- time_down > trial[1] & time_down < trial[2]

  id_up <- id_up[keep_up]
  time_up <- time_up[keep_up]
  name_up <- name_up[keep_up]
  id_down <- id_down[keep_down]
  time_down <- time_down[keep_down]
  name_down <- name_down[keep_down]

  for(i in 1:2){
    if(length(id_up) < length(id_down)){
      id_up <- c(id_up, NaN)
      time_up <- c(time_up, NaN)
      name_up <- c(name_up, NaN)
    }
    if(length(id_up) > length(id_down)){
      id_down <- c(id_down, NaN)
      time_down <- c(time_down, NaN)
      name_down <- c(name_down, NaN)
    }
  }

  key <- data.frame(idUp = id_up, timeUp = time_up, nameUp = name_up,
                    idDown = id_down, timeDown = time_down, nameDown = name_down)
}
# ---------------------------------------------------------------------------- #
