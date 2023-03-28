# ---------------------------------------------------------------------------- #
#' Preprocessing the subject data
#'
#' This function performs the preprocessing for each run of the given number of
#' session for a subject. It uses \link{preprocessing_session} function
#' for each session.
#'
#' @note If the participant is given as a numeric variable, the function adds to
#'     the beginning of participant code a letter 's' and changes the variable
#'     into character.
#' @param directory character -- the directory that contains all the data
#' @param expList list -- contains all the possible experiment name as characters
#' @param participant character or numeric - the subject id e.g. 's001' or simply
#'     the number of subject
#' @param sessions numeric array -- the number of sessions to be preprocessed in total
#'
#' @return list -- contains the preprocessed data of every sessions
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessing_subject('~/preprivalry/tests',c('RivalryGrating','RivalryImages'),1,1:2)
#' }
preprocessing_subject <- function(directory,expList,participant,sessions){
  data <- list()
  if(is.numeric(participant)){
    participant <- paste('s',sprintf('%03d', participant),sep = '')
  }
  for(session_no in sessions){
    session_data <- preprivalry::preprocessing_session(directory,expList,participant,session_no)
    data[[session_no]] <- session_data
  }
  rivdata <- preprivalry::read_rivdata(directory,expList[1],participant,'session1')
  data[['percept_keys']] <- rbind(rivdata[["log"]][[4]][[1]],
                                  c(rivdata[["log"]][[4]][[2]][[1]][[1]],rivdata[["log"]][[4]][[2]][[2]][[1]]))
  data[['subject']] <- participant
  return(data)
}
# ---------------------------------------------------------------------------- #
