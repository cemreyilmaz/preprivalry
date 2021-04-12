# ---------------------------------------------------------------------------- #
#  The data is collected as matlab .mat file in our project. Therefore, we will
#  read and convert it to preprocess it as our first step. Then, organize it.
# ---------------------------------------------------------------------------- #
#' Preprocessing the subject data
#'
#' This function performs the preprocessing for each run of the given number of
#' session for the given subject. It uses \link{preprocessing_session} function
#' for each session.
#'
#' @note If the participant is given as a numeric variable, the function adds to
#'     the beginning of participant code a letter 's' and changes the variable
#'     into character.
#' @param directory the directory that contains all the data
#' @param expList a list containing all the possible experiment name as characters
#' @param participant the subject id e.g. 's001' or simply the number of subject
#' @param sessions the number of sessions to be preprocessed in total
#'
#' @return data a list containing the preprocessed data of every sessions
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessing_subject('~/preprivalry/tests',c('RivalryGrating','RivalryImages'),1,2)
#' }
preprocessing_subject <- function(directory,expList,participant,sessions){
  data <- list()
  for(session_no in 1:sessions){
    if(is.numeric(participant)){
      participant <- paste('s',sprintf('%03d', participant),sep = '')
    }
    session_data <- preprocessing_session(directory,expList,participant,session_no)
    data[[session_no]] <- session_data
  }
  return(data)
}
# ---------------------------------------------------------------------------- #
#' Preprocessing the session data of given subject
#'
#' This function performs a preprocessing workflow for each run of the given
#' session for the given subject. It uses \link{preprocessing_run} function for
#' each run.
#'
#' @note If the participant and/or session_no is given as a numeric variable,
#'     the function adds to the beginning of participant code a letter 's' and
#'     changes the variable into character.
#'
#' @param directory the directory that contains all the data
#' @param expList a list containing all the possible experiment name as characters
#' @param participant the subject id e.g. 's001' or simply the number of subject
#' @param session_no the session number as numeric or in the format of 'session1'
#'
#' @return data a list containing the preprocessed data of every runs
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
    run_data <- preprocessing_run(directory,expType,participant,session,2)
    data[[expNo]] <- run_data
  }
  return(data)
}
# ---------------------------------------------------------------------------- #
#' Preprocessing the run data of given session of given subject
#'
#' This function performs the preprocessing for the given run of the given
#' session for the given subject. First, it uses \link{read_rivdata} function to
#' read the data. Then, it extracts experiment info and key events by using
#' \link{extract_exp} and \link{extract_key}. Then, it extracts and cleans
#' the data for separate trials in a run by using \link{extract_trialkey} and
#' \link{clean_keyevents}. The preprocessing of the trial data is performed by
#' \link{preprocessing_trial}. Then, a series of key events is created for
#' transition phases with \link{create_transitionkey}. The function
#' \link{reorganize_prepdata} creates the output ready to be analyzed.
#'
#' @note If the participant and/or session_no is given as a numeric variable,
#'     the function adds to the beginning of participant code a letter 's' and
#'     changes the variable into character.
#'
#' @import dplyr
#'
#' @param directory the directory that contains all the data
#' @param expType the experiment name as characters
#' @param participant the subject id e.g. 's001' or simply the number of subject
#' @param session the session number as numeric or in the format of 'session1'
#' @param which_output to define you want a table-version of output (2) or a data-type output(1)
#' @return a list containing the preprocessed data of given experimental block
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
