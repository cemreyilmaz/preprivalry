# ---------------------------------------------------------------------------- #
#' Extracting the data of key events
#'
#' This function rearranges the binocular rivalry data which is the output of
#' \link{read_rivdata}, and creates a data frame including the key name, key code
#' and time for key-presses and key-releases.
#'
#' @param rivdata list -- the raw data
#'
#' @return data.frame -- the data of only the key-events
#'
#' @export
#'
#' @examples
#' \dontrun{
#' key <- extract_key(read_rivdata('~/preprivalry/tests','RivalryGratings','s001','session1'))
#' }
extract_key <- function(rivdata){
  pKeys      <- rivdata[["log"]][[4]][[1]]
  pKeyNames  <- rivdata[["log"]][[4]][[2]]
  idDown     <- rivdata[["log"]][[4]][[3]] # key-press id
  nameDown   <- c()
  nameDown[idDown == pKeys[1,1]] <- pKeyNames[[1]][[1]]
  nameDown[idDown == pKeys[1,2]] <- pKeyNames[[2]][[1]]
  timeDown   <- rivdata[["log"]][[4]][[4]] # key-press time
  idUp       <- rivdata[["log"]][[4]][[5]] # key-release id
  nameUp     <- c()
  nameUp[idUp == pKeys[1,1]] <- pKeyNames[[1]][[1]]
  nameUp[idUp == pKeys[1,2]] <- pKeyNames[[2]][[1]]
  timeUp     <- rivdata[["log"]][[4]][[6]] # key-release time
  if(length(idUp) != length(idDown)){
    a <- max(c(length(idUp), length(idDown)))
    idUp[length(idUp):a]   <- NaN
    timeUp[length(idUp):a] <- NaN
    nameUp[length(idUp):a] <- NaN
    idDown[length(idDown):a]   <- NaN
    timeDown[length(idDown):a] <- NaN
    nameDown[length(idDown):a] <- NaN
  }
  key <- data.frame(nameUp,idUp,timeUp,nameDown,idDown,timeDown)
}
# ---------------------------------------------------------------------------- #
