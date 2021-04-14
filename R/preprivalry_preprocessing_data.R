# ---------------------------------------------------------------------------- #
#  The data is collected as matlab .mat file in our project. Therefore, we will
#  read and convert it to preprocess it as our first step. Then, organize it.
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
#' @param sessions numeric -- the number of sessions to be preprocessed in total
#' @param which_output numeric -- to define you the type of output
#'     a data-type output(1) or a table-version of output (2)
#'
#' @return list -- contains the preprocessed data of every sessions
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessing_subject('~/preprivalry/tests',c('RivalryGrating','RivalryImages'),1,2)
#' }
preprocessing_subject <- function(directory,expList,participant,sessions,which_output){
  data <- list()
  if(missing(which_output)){
    which_output <- 2
  }
  if(is.numeric(participant)){
    participant <- paste('s',sprintf('%03d', participant),sep = '')
  }
  for(session_no in 1:sessions){
    session_data <- preprocessing_session(directory,expList,participant,session_no,which_output)
    data[[session_no]] <- session_data
  }
  rivdata <- read_rivdata(directory,expList[1],participant,'session1')
  data[['percept_keys']] <- rbind(rivdata[["log"]][[4]][[1]],
                                  c(rivdata[["log"]][[4]][[2]][[1]][[1]],rivdata[["log"]][[4]][[2]][[2]][[1]]))
  data[['subject']] <- participant
  return(data)
}
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
#' @param which_output numeric -- to define you the type of output
#'     a data-type output(1) or a table-version of output (2)
#'
#' @return list -- contains the preprocessed data of every runs
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessing_session('~/preprivalry/tests',c('RivalryGrating','RivalryImages'),1,2)
#' }
preprocessing_session <- function(directory,expList,participant,session_no,which_output){
  if(is.numeric(participant)){
    participant <- paste('s',sprintf('%03d', participant),sep = '')
  }
  if(is.numeric(session_no)){
    session <- paste('session', session_no, sep = '')
  }
  if(missing(which_output)){
    which_output <- 2
  }
  data <- list()
  for(expNo in 1:length(expList)){
    expType  <- expList[expNo]
    run_data <- preprocessing_run(directory,expType,participant,session,which_output)
    data[[expNo]] <- run_data
  }
  return(data)
}
# ---------------------------------------------------------------------------- #
#' Preprocessing the run data of a session of a subject
#'
#' This function performs the preprocessing for the given run of the given
#' session for the given subject. First, it reads the data. Then, it extracts
#' experiment info and key events. Then, it extracts and cleans the data for
#' separate trials in a run. The preprocessing of the trial data is performed,
#' and a series of key events is created for transition phases. The output
#' is ready to be analyzed.
#' See also:
#' \link{read_rivdata} \link{extract_exp} \link{extract_key}
#' \link{extract_trialkey} \link{clean_keyevents} \link{preprocessing_trial}
#' \link{create_transitionkey} \link{reorganize_prepdata}
#'
#' @note If the participant and/or session_no is given as a numeric variable,
#'     the function adds to the beginning of participant code a letter 's' and
#'     changes the variable into character.
#'
#' @import dplyr
#'
#' @param directory character -- the directory that contains all the data
#' @param expType character -- the experiment name
#' @param participant character or numeric -- the subject id e.g. 's001' or simply
#'     the number of subject
#' @param session character or numeric -- the session number as numeric or in
#'     the format of 'session1'
#' @param which_output numeric -- to define you the type of output
#'     a data-type output(1) or a table-version of output (2)
#'
#' @return list -- contains the preprocessed data of given experimental block
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessing_run('~/preprivalry/tests','RivalryGrating',1,2)
#' }}
preprocessing_run <- function(directory,expType,participant,session,which_output){
  if(is.numeric(participant)){
    participant <- paste('s',sprintf('%03d', participant),sep = '')
  }
  if(is.numeric(session)){
    session <- paste('session', session, sep = '')
  }
  if(missing(which_output)){
    which_output <- 2
  }
  rivdata <- read_rivdata(directory,expType,participant,session)
  exp     <- extract_exp(rivdata)
  exp_key <- extract_key(rivdata)
  percept_keys <- rivdata[["log"]][[4]][[1]]
  key_data     <- list()
  all_percept  <- list()
  for(t in 1:length(exp)){
    trial      <- exp[t,]
    trial_key  <- extract_trialkey(exp_key,trial)
    trial_key  <- clean_keyevents(trial_key,2)
    d <- preprocessing_trial(trial_key,trial,percept_keys)
    d[[length(d)+1]] <- create_transitionkey(trial_key,trial)
    key_data[[t]]    <- d
    all_percept[[t]] <- reorganize_prepdata(trial_key,trial)
  }
  if(which_output==1){
    # output is key_data
    return(key_data)
  } else if(which_output==2){
    return(all_percept)
  } else{
    warning('No output is defined!')
    return(NULL)
  }
}
# ---------------------------------------------------------------------------- #
