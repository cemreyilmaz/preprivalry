# ---------------------------------------------------------------------------- #
#' Remove irrelevant key events
#' This function removes the keys which are not defined as percept keys.
#'
#' @param key dataframe -- containing info about key events
#' @param percept_keys matrix -- contains the keycodes for each percept
#'
#' @return dataframe containing the organized key events
#' @export
#'
#' @examples
#' \dontrun{
#' remove_irrelevant_keyevents(trial_key,percept_keys)}
remove_irrelevant_keyevents <- function(key,percept_keys){
  id_down <- key$idDown[!is.na(key['idDown'])]
  time_down <- key$timeDown[!is.na(key['idDown'])]
  name_down <- key$nameDown[!is.na(key['idDown'])]
  id_up <- key$idUp[!is.na(key['idUp'])]
  time_up <- key$timeUp[!is.na(key['idUp'])]
  name_up <- key$nameUp[!is.na(key['idUp'])]
  # first, remove irrelevant key events
  keep_down <- (id_down == percept_keys[1] | id_down == percept_keys[2])
  id_down <- id_down[keep_down]
  time_down <- time_down[keep_down]
  name_down <- name_down[keep_down]
  keep_up <- (id_up == percept_keys[1] | id_up == percept_keys[2])
  id_up <- id_up[keep_up]
  time_up <- time_up[keep_up]
  name_up <- name_up[keep_up]
  for(i in 1:2){
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
  }
  key <- data.frame(idDown = id_down, timeDown = time_down, nameDown = name_down,
                    idUp = id_up, timeUp = time_up, nameUp = name_up)
  return(key)
}
