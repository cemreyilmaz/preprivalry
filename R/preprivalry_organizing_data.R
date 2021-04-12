# ---------------------------------------------------------------------------- #
#  The data are read and tidied up for preprocessing.
# ---------------------------------------------------------------------------- #
#' Creating file name and reading it
#'
#' This is a simple function to create the file name of data collected on matlab
#' by using \link{dir} function. Then, it checks if there is any debugged version
#' and uses the debugged. It reads the mat file by using \link{readMat}.
#'
#' @note I use the following file name structure:
#'     'ExperimentName_SubjectID_SessionNo_ ...'. This function searches for
#'     such a file name.
#' @note It returns into a warning message if the file cannot be read for any reason.
#'
#' @import R.matlab
#'
#' @param d the directory in which the data are stored
#' @param e the experiment name of the current run
#' @param p the participant id in the format of 's001'
#' @param s the session number in the format of 'session1'
#'
#' @return data a list -- all the information about the current run and
#'     the key-press data collected in this run
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- riv_data('~/preprivalry/tests','RivalryGratings','s001','session1')
#' }
read_rivdata <- function(d,e,p,s){
  fullPath   <- dir(d, full.names=T, pattern=paste(e, '_', p, '_', s, sep = ''))
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
#' Extracting the data of key events
#'
#' This function rearranges the binocular rivalry data which is the output of
#' \link{read_rivdata}, and creates a data frame including the key name, key code
#' and time for key-presses and key-releases.
#'
#' @param rivdata a list -- the binocular rivalry data
#'
#' @return key a dataframe -- the data related to the key-events
#'
#' @export
#'
#' @examples
#' \dontrun{
#' key <- extract_key(riv_data('~/preprivalry/tests','RivalryGratings','s001','session1'))
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
#' Extracting the details of experimental run
#'
#' This function rearranges the binocular rivalry data which is the output of
#' \link{read_rivdata}, and creates a data frame including the start and end time
#' of trials.
#'
#' @param rivdata a list -- the binocular rivalry data
#'
#' @return exp a data frame -- the start and end time of trials
#'
#' @export
#'
#' @examples
#' \dontrun{
#' exp <- extract_exp(riv_data('~/preprivalry/tests','RivalryGratings','s001','session1'))
#' }
extract_exp <- function(rivdata){
  trialStartTime <- t(rivdata[["log"]][[5]][[1]]) # trialStartTime
  trialEndTime   <- t(rivdata[["log"]][[5]][[2]]) # trialEndTime
  exp <- data.frame(trialStartTime,trialEndTime)
}
# ---------------------------------------------------------------------------- #
#' Extracting the key events only in the given trial
#'
#' This function extracts the key events from the given trial. It uses the time
#' information to define whether or not the key event belongs to the given trial.
#' The input data is the output of \link{extract_key} function.
#'
#' @note The function changes the class of "trial" if it is not numeric.
#' @param key a dataframe -- the data related to the key-events
#' @param trial trial start and end time
#'
#' @return key a dataframe -- the data related to the key-events in the given trial
#' @export
#'
#' @examples
#' \dontrun{
#' key <- extract_key(riv_data('~/preprivalry/tests','RivalryGratings','s001','session1'))
#' exp <- extract_exp(riv_data('~/preprivalry/tests','RivalryGratings','s001','session1'))
#' key <- extract_trialkey(key,exp[1,])
#' }
extract_trialkey <- function(key,trial){
  # the key-events only in this trial
  if(!is.numeric(trial)){
    trial <- as.numeric(unlist(trial))
  }
  key     <- key[key$timeUp > trial[1] &
                 key$timeUp < trial[2] &
                 key$timeDown > trial[1] &
                 key$timeDown < trial[2],]
}
# ---------------------------------------------------------------------------- #
#' Preprocessing of key events in a trial
#'
#' This function preprocesses the key events in a given trial.
#'
#' @param trial_key a dataframe containing the key-events in given trial
#' @param trial a dataframe containing trial info
#' @param percept_keys an array containing the keycodes for each percept
#'
#' @import dplyr
#'
#' @return trial_data a list containing the onset and duration info for
#'     each percept by keycode
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessing_trial(trial_key,trial,percept_keys)
#' }
preprocessing_trial <- function(trial_key,trial,percept_keys){
  trial_data <- list()
  for(p in 1:length(percept_keys)){
    theKey <- percept_keys[p]
    perceptStartTime <- trial_key[trial_key['idDown']==theKey,]
    perceptStartTime <- dplyr::select(perceptStartTime,timeDown)
    perceptStartTime <- as.numeric(unlist(perceptStartTime))
    perceptEndTime   <- trial_key[trial_key['idUp']==theKey,]
    perceptEndTime   <- dplyr::select(perceptEndTime,timeUp)
    perceptEndTime   <- as.numeric(unlist(perceptEndTime))
    trial_data[[p]]  <- data.frame(key  = theKey,
                                  onset = perceptStartTime - as.numeric(unlist(trial[1])),
                                  duration = c(perceptEndTime - perceptStartTime))
  }
  return(trial_data)
}
# ---------------------------------------------------------------------------- #
#' Creating key events for transition phase
#'
#' This function creates a series of key event with a key code 0 for the duration
#' in which participant don't press any key. Such periods are counted as
#' transition phases in binocular rivalry experiment. This approach is valid
#' only when the participant is instructed to press and hold a key during every
#' dominant percept.
#'
#' @note This function can be used only if the experiment includes
#'     a 'press-and-hold' type of binocular rivalry paradigm.
#' @note The output is only the key events for transition. It must be defined
#'     as a new field in key list or as a new variable.
#'
#' @param key a data frame containing the preprocessed key-events in a trial
#' @param trial a dataframe containing trial info
#'
#' @return data a data frame containing the onset and duration of each transition
#'     period with a label of key-code 0
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_transitionkey(trial_key)
#' }
create_transitionkey <- function(key,trial){
data <- data.frame(key   = 0,
                   onset = key$timeUp[1:(length(key$timeUp)-1)] - as.numeric(unlist(trial[1])),
                   duration = key$timeDown[2:length(key$timeDown)] - key$timeUp[1:(length(key$timeUp)-1)])
}
# ---------------------------------------------------------------------------- #
#' Reorganizing the preprocessed data of the given trial
#'
#' This function merges and sorts the key-events in a trial. The output is ready
#' to be analyzed.
#'
#' @param key a data frame containing the preprocessed key-events of dominant
#'     percepts in a trial
#' @param trial a data frame containing trial info
#'
#' @return key a data frame containing the preprocessed key-events of dominant
#'     percepts and transition phases in a trial
#' @export
#'
#' @examples
#' \dontrun{
#' reorganize_prepdata(trial_key,trial)
#' }
reorganize_prepdata <- function(key,trial){
  key <- data.frame(
    idDown   = c(key$idDown, numeric(length(key$timeUp)-1)),
    timeDown = c(key$timeDown, key$timeUp[1:(length(key$timeUp)-1)])- as.numeric(unlist(trial[1])),
    timeUp   = c(key$timeUp, key$timeDown[2:length(key$timeDown)])- as.numeric(unlist(trial[1])))
  key <- key[order(key$timeDown),]
}
# ---------------------------------------------------------------------------- #
#' Saving the preprocessed data as csv
#'
#' This function reorganizes the preprocessed data and saves it as csv file in
#' the defined path.
#'
#' @param data preprocessed data which is the output of preprocessing functions
#' @param percept_keys key codes and names which are instructed in the experiment
#' @param subjID subject id to be used as label in the csv file
#'
#' @importFrom utils write.csv
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' reorganize_as_csv(data,percept_keys,subjectID)
#' }
reorganize_as_table <- function(data,percept_keys,subjID){
id <- c()
timeUp   <- c()
timeDown <- c()
session  <- c()
run      <- c()
trial    <- c()
keyname  <- c()
for(i in 1:length(data)){
  for(j in 1:length(data[[i]])){
    for(k in length(data[[i]][[j]])){
      tmp <- data[[i]][[j]][[k]]
      id  <- c(id, tmp$idDown)
      timeUp   <- c(timeUp, tmp$timeUp)
      timeDown <- c(timeDown, tmp$timeDown)
      trial <- c(trial,(numeric(length(tmp$timeDown)) + j))
      run <- c(run,(numeric(length(tmp$timeDown)) + j))
      session  <- c(session, (numeric(length(tmp$timeDown)) + i))
    }
  }
}
subject <- rep(subjID,length(timeDown))
keyname[id == percept_keys[1,1]] <- percept_keys[2,1]
keyname[id == percept_keys[1,2]] <- percept_keys[2,2]
keyname[id == 0] <- 'Transition'
table <- data.frame(subject,session,run,keyname,id,timeDown,timeUp)
}
# ---------------------------------------------------------------------------- #
