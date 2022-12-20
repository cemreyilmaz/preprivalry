# ---------------------------------------------------------------------------- #
#' Cleaning key events
#'
#' This functions aims to remove the unexpected key events including key released
#' which was saved at the beginning of experiment without the key press, key
#' press at the end of experiment without its release, and key events at the
#' beginning with different key-codes.
#'
#' @param key a dataframe containing info about key events
#' @param iteration a numeric input for the number of iteration of this cleaning process
#'
#' @return key a dataframe containing the organized key events
#' @export
#'
#' @examples
#' \dontrun{
#' clean_keyevents(trial_key,2)
#' }
clean_keyevents <- function(key,iteration){
  id_up <- key$idUp
  time_up <- key$timeUp
  name_up <- key$nameUp
  id_down <- key$idDown
  time_down <- key$timeDown
  name_down <- key$nameDown
  tryCatch(
    {
      # cleaning
      for(i in 1:iteration){
        # if keyUp comes before first keyDown
        # => delete first release
        if(time_up[1] < time_down[1]){
          id_up   <- id_up[2:length(id_up)]
          time_up   <- time_up[2:length(time_up)]
          name_up   <- name_up[2:length(name_up)]
        }
        # if keyUp and keyDown are not of the same percept at the beginning
        # => delete first key event
        if(id_up[1] != id_down[1]){
          id_up <- id_up[2:length(id_up)]
          time_up <- time_up[2:length(time_up)]
          name_up <- name_up[2:length(name_up)]
          id_down <- id_down[2:length(id_down)]
          time_down <- time_down[2:length(time_down)]
          name_down <- name_down[2:length(name_down)]
        }
        # if last keyDown comes after last keyUp
        # => delete last press
        if(time_down[length(time_down)] > time_up[length(time_up)]){
          id_down <- id_down[1:(length(id_down)-1)]
          time_down <- time_down[1:(length(time_down)-1)]
          name_down <- name_down[1:(length(name_down)-1)]
        }
        # if keyUp and keyDown are not of the same percept at the end
        # => delete last key event
        if(id_up[length(id_up)] != id_down[length(id_down)]){
          id_up <- id_up[1:(length(id_up)-1)]
          time_up <- time_up[1:(length(time_up)-1)]
          name_up <- name_up[1:(length(name_up)-1)]
          id_down <- id_down[1:(length(id_down)-1)]
          time_down <- time_down[1:(length(time_down)-1)]
          name_down <- name_down[1:(length(name_down)-1)]
        }
      } # end
      # check if there is a mismatch between the length of arrays
      if(length(id_down) < length(id_up)){
        if(time_down[1]<time_up[1]){ # remove last release
          id_up <- id_up[1:(length(id_up)-1)]
          time_up <- time_up[1:(length(time_up)-1)]
          name_up <- name_up[1:(length(name_up)-1)]
        }
        if(time_down[1]>time_up[1]){ # remove first release
          id_up <- id_up[2:length(id_up)]
          time_up <- time_up[2:length(time_up)]
          name_up <- name_up[2:length(name_up)]
        }
      }
      if(length(id_down) > length(id_up)){
        if(time_down[1]<time_up[1]){ # remove last press
          id_down <- id_down[1:(length(id_down)-1)]
          time_down <- time_down[1:(length(time_down)-1)]
          name_down <- name_down[1:(length(name_down)-1)]
        }
        if(time_down[1]>time_up[1]){ # remove first press
          id_down <- id_down[2:length(id_down)]
          time_down <- time_down[2:length(time_down)]
          name_down <- name_down[2:length(name_down)]
        }
      }
    },
    error=function(cond){
      message("No keys are found!")
    }
  )
  # re-define "key"
  key <- data.frame(idDown = id_down, timeDown = time_down, nameDown = name_down,
                    idUp = id_up, timeUp = time_up, nameUp = name_up)
  return(key)
}
# ---------------------------------------------------------------------------- #
