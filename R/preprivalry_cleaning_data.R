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
  for(i in 1:iteration){
    # if keyUp comes before first keyDown
    # => delete first release
    if(key$timeUp[1] < key$timeDown[1]){
      key$idUp   <- c(key$idUp[2:(length(key$idUp-1))],NaN)
      key$timeUp <- c(key$timeUp[2:(length(key$timeUp-1))],NaN)
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
    }
    # if keyUp and keyDown are not of the same percept at the end
    # => delete last key event
    length_key <- dim(key)
    length_key <- length_key[1] -1
    if(key$idUp[length(key$idUp[!is.na(key$idUp)])] != key$idDown[length(key$idDown[!is.na(key$idDown)])]){
      key$idUp[length(key$idUp[!is.na(key$idUp)])] <- NaN
      key$timeUp[length(key$timeUp[!is.na(key$timeUp)])] <- NaN
      key$idDown[length(key$idDown[!is.na(key$idDown)])] <- NaN
      key$timeDown[length(key$timeDown[!is.na(key$timeDown)])] <- NaN
    }
    key <- key[!is.na(key['idDown']),]
    key <- key[!is.na(key['timeDown']),]
    key <- key[!is.na(key['idUp']),]
    key <- key[!is.na(key['timeUp']),]
    return(key)
  }
}
# ---------------------------------------------------------------------------- #
