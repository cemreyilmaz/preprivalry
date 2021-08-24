# ---------------------------------------------------------------------------- #
#  The behavioral data are cleaned and preprocessed.
# ---------------------------------------------------------------------------- #
#' Cleaning key events
#'
#' This functions aims to remove the unexpected key events including key released
#' which was saved at the beginning of experiment without the key press, key
#' press at the end of experiment without its release, and key events at the
#' beginning with different key-codes.
#'
#' @param key a dataframe containing info about key events
#' @param percept_keys matrix -- contains the keycodes for each percept
#' @param iteration a numeric input for the number of iteration of this cleaning process
#'
#' @return key a dataframe containing the organized key events
#' @export
#'
#' @examples
#' \dontrun{
#' clean_keyevents(trial_key,2)
#' }
clean_keyevents <- function(key,percept_keys,iteration){
  # first, remove irrelevant key events
  key$idUp[!(key$idUp == percept_keys[1] | key$idUp == percept_keys[2])] <- NaN
  key$timeUp[!(key$idUp == percept_keys[1] | key$idUp == percept_keys[2])] <- NaN
  key$nameUp[!(key$idUp == percept_keys[1] | key$idUp == percept_keys[2])] <- NaN
  key$idDown[!(key$idDown == percept_keys[1] | key$idDown == percept_keys[2])] <- NaN
  key$timeDown[!(key$idDown == percept_keys[1] | key$idDown == percept_keys[2])] <- NaN
  key$nameDown[!(key$idDown == percept_keys[1] | key$idDown == percept_keys[2])] <- NaN
  id_down <- key$idDown[!is.na(key['idDown'])]
  time_down <- key$timeDown[!is.na(key['idDown'])]
  name_down <- key$nameDown[!is.na(key['idDown'])]
  id_up <- key$idUp[!is.na(key['idUp'])]
  time_up <- key$timeUp[!is.na(key['idUp'])]
  name_up <- key$nameUp[!is.na(key['idUp'])]
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
  key <- data.frame(idDown = id_down, timeDown = time_down, nameDown = name_down,
                    idUp = id_up, timeUp = time_up, nameUp = name_up)
  # -------------------------------------------------------------------------- #
  for(i in 1:iteration){
    # if keyUp comes before first keyDown
    # => delete first release
    if(key$timeUp[1] < key$timeDown[1]){
      key$idUp   <- c(key$idUp[2:length(key$idUp)], NaN)
      key$timeUp <- c(key$timeUp[2:length(key$timeUp)], NaN)
      key$nameUp <- c(key$nameUp[2:length(key$nameUp)], NaN)
    }
    # if keyUp and keyDown are not of the same percept at the beginning
    # => delete first key event
    length_key <- dim(key)
    length_key <- length_key[1]
    if(key$idUp[1] != key$idDown[1]){
      key <- key[2:length_key,]
    }
    # if last keyDown comes after last keyUp
    # => delete last press
    if(key$timeDown[length(key$timeDown[!is.na(key$timeDown)])] > key$timeUp[length(key$timeUp[!is.na(key$timeUp)])]){
      key$idDown   <- c(key$idDown[1:(length(key$idDown)-1)],NaN)
      key$timeDown <- c(key$timeDown[1:(length(key$timeDown)-1)],NaN)
      key$nameDown <- c(key$nameDown[1:(length(key$nameDown)-1)],NaN)
    }
    # if keyUp and keyDown are not of the same percept at the end
    # => delete last key event
    length_key <- dim(key)
    length_key <- length_key[1] -1
    if(key$idUp[length(key$idUp[!is.na(key$idUp)])] != key$idDown[length(key$idDown[!is.na(key$idDown)])]){
      key$idUp[length(key$idUp[!is.na(key$idUp)])] <- NaN
      key$timeUp[length(key$timeUp[!is.na(key$timeUp)])] <- NaN
      key$nameUp[length(key$tnameUp[!is.na(key$nameUp)])] <- NaN
      key$idDown[length(key$idDown[!is.na(key$idDown)])] <- NaN
      key$timeDown[length(key$timeDown[!is.na(key$timeDown)])] <- NaN
      key$nameDown[length(key$nameDown[!is.na(key$nameDown)])] <- NaN
    }
    id_down <- key$idDown[!is.na(key['idDown'])]
    time_down <- key$timeDown[!is.na(key['idDown'])]
    name_down <- key$nameDown[!is.na(key['idDown'])]
    id_up <- key$idUp[!is.na(key['idUp'])]
    time_up <- key$timeUp[!is.na(key['idUp'])]
    name_up <- key$nameUp[!is.na(key['idUp'])]
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
    key <- data.frame(idDown = id_down, timeDown = time_down, nameDown = name_down,
                      idUp = id_up, timeUp = time_up, nameUp = name_up)
  }
  return(key)
}
# ---------------------------------------------------------------------------- #
