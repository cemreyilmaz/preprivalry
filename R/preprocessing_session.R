# ---------------------------------------------------------------------------- #
#' Preprocessing the session data of a subject
#'
#' This function performs a preprocessing workflow for each run of the given
#' session for the given subject. It uses \link{preprocessing_run} function for
#' each run.
#'
#' @note If the participant and/or session_no is given as a numeric variable,
#'     the function adds to the beginning of participant code a letter 's' and
#'     changes the variable into character.
#'
#' @param directory character -- the directory that contains all the data
#' @param expList list -- contains all the possible experiment name as characters
#' @param participant character or numeric -- the subject id e.g. 's001' or simply
#'     the number of subject
#' @param session_no character or numeric -- the session number as numeric or in
#'     the format of 'session1'
#'
#' @return list -- contains the preprocessed data of every runs
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessing_session('~/preprivalry/tests',c('RivalryGrating','RivalryImages'),1,2)
#' }
preprocessing_session <- function(directory,expList,participant,session_no){
  if(is.numeric(participant)){
    participant <- paste('s',sprintf('%03d', participant),sep = '')
  }
  if(is.numeric(session_no)){
    session <- paste('session', session_no, sep = '')
  }
  data <- list()
  for(expNo in 1:length(expList)){
    expType  <- expList[expNo]
    run_data <- preprivalry::preprocessing_run(directory,expType,participant,session)
    run_data <- preprivalry::eye_info(run_data)
    data[[expNo]] <- run_data
  }
  return(data)
}
# ---------------------------------------------------------------------------- #
