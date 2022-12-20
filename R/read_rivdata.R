# ---------------------------------------------------------------------------- #
#' Creating file name and reading it
#'
#' This is a simple function to create the file name of data collected on matlab.
#' Then, it checks if there is any debugged version and uses the debugged. It
#' reads the mat file by using \link{readMat}.
#'
#' @note I use the following file name structure:
#'     'ExperimentName_SubjectID_SessionNo ...'. This function searches for
#'     such a file name.
#' @note It returns into a warning message if the file cannot be read for any reason.
#' @note If the data file is stored in the current directory, you don't need to specify
#'     the path. Then, use d <- '' or don't define d.
#' @note If the participant and/or session_no is given as a numeric variable,
#'     the function adds to the beginning of participant code a letter 's' and
#'     changes the variable into character.
#'
#' @importFrom R.matlab readMat
#'
#' @param directory character -- the directory in which the data are stored
#' @param expType character -- the experiment name of the current run
#' @param participant character or numeric -- the subject id e.g. 's001' or simply
#'     the number of subject
#' @param session character or numeric -- the session number as numeric or in
#'     the format of 'session1'
#'
#' @return list -- all the information about the current run and the key-event
#'     data collected in this run
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- riv_data('~/preprivalry/tests','RivalryGratings','s001','session1')
#' }
read_rivdata <- function(directory,expType,participant,session){
  if(directory=='' | missing(directory)){
    directory <- getwd()
  }
  if(is.numeric(participant)){
    participant <- paste('s',sprintf('%03d', participant),sep = '')
  }
  if(is.numeric(session)){
    session <- paste('session', session, sep = '')
  }
  fullPath   <- dir(directory, full.names=T, pattern=paste(expType, '_', participant, '_', session, sep = ''))
  if(length(fullPath)>1){
    fullPath <- fullPath[substr(fullPath,nchar(fullPath)-11,nchar(fullPath)) == 'debugged.mat']
  }
  tryCatch({
    data  <- R.matlab::readMat(fullPath)
  }, error = function(e){
    warning('The file cannot be read!')
    return(NULL)
  })
}
# ---------------------------------------------------------------------------- #
