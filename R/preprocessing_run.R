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
#' \link{create_transitionkey}
#'
#' @note If the participant and/or session_no is given as a numeric variable,
#'     the function adds to the beginning of participant code a letter 's' and
#'     changes the variable into character.
#'
#' @param directory character -- the directory that contains all the data
#' @param expType character -- the experiment name
#' @param participant character or numeric -- the subject id e.g. 's001' or simply
#'     the number of subject
#' @param session character or numeric -- the session number as numeric or in
#'     the format of 'session1'
#'
#' @return list -- contains the preprocessed data of given experimental block
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessing_run('~/preprivalry/tests','RivalryGrating',1,2)
#' }
preprocessing_run <- function(directory,expType,participant,session){
  if(is.numeric(participant)){
    participant <- paste('s',sprintf('%03d', participant),sep = '')
  }
  if(is.numeric(session)){
    session <- paste('session', session, sep = '')
  }
  rivdata <- read_rivdata(directory,expType,participant,session)
  exp     <- extract_exp(rivdata)
  exp_key <- extract_key(rivdata)
  percept_keys <- as.numeric(unlist(rivdata[["log"]][[4]][[1]]))
  key_data     <- list()
  for(trl in 1:length(exp)){
    trial      <- exp[trl,]
    trial_key  <- extract_trialkey(exp_key,trial)
    if(dim(trial_key)[1] > 1){
      trial_key  <- remove_irrelevant_keyevents(trial_key,percept_keys)
      trial_key  <- clean_keyevents(trial_key,2)
      d <- preprocessing_trial(trial_key,as.numeric(exp[1,1]),percept_keys)
      d[[length(d)+1]] <- create_transitionkey(trial_key,as.numeric(exp[1,1]))
      key_data[[trl]]    <- d
    }
    else{
      key_data[[trl]]    <- create_nan_trialdata(percept_keys)
    }
  }
  return(key_data)
}
# ---------------------------------------------------------------------------- #
