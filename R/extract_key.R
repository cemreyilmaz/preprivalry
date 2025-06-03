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
  log_data   <- rivdata[["log"]]
  y <- 1:length(log_data)
  x <- y[rownames(log_data) == "key"]
  key_data   <- log_data[[x]]
  pKeys      <- key_data[[1]]
  pKeyNames  <- key_data[[2]]
  idDown     <- key_data[[3]] # key-press id
  nameDown   <- c()
  nameDown[idDown == pKeys[1,1]] <- pKeyNames[[1]][[1]]
  nameDown[idDown == pKeys[1,2]] <- pKeyNames[[2]][[1]]
  timeDown   <- key_data[[4]] # key-press time
  idUp       <- key_data[[5]] # key-release id
  nameUp     <- c()
  nameUp[idUp == pKeys[1,1]] <- pKeyNames[[1]][[1]]
  nameUp[idUp == pKeys[1,2]] <- pKeyNames[[2]][[1]]
  timeUp     <- key_data[[6]] # key-release time
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
