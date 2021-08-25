# ---------------------------------------------------------------------------- #
#  The data are read and tidied up for preprocessing.
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
#' Extracting the details of an experimental run
#'
#' This function rearranges the binocular rivalry data which is the output of
#' \link{read_rivdata}, and creates a data frame including the start and end time
#' of trials.
#'
#' @param rivdata list -- the raw data
#'
#' @return data.frame -- the start and end time of trials
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
#' See also \link{extract_key}
#'
#' @note The function changes the class of "trial" if it is not numeric.
#' @param key data.frame -- the data related to the key-events
#' @param trial matrix -- trial start and end time
#'
#' @return data.frame -- the data related to the key-events in the given trial
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
  id_up <- key$idUp
  time_up <- key$timeUp
  name_up <- key$nameUp
  id_down <- key$idDown
  time_down <- key$timeDown
  name_down <- key$nameDown
  keep_up <- time_up > trial[1] & time_up < trial[2]
  keep_down <- time_down > trial[1] & time_down < trial[2]

  id_up <- id_up[keep_up]
  time_up <- time_up[keep_up]
  name_up <- name_up[keep_up]
  id_down <- id_down[keep_down]
  time_down <- time_down[keep_down]
  name_down <- name_down[keep_down]

  for(i in 1:2){
    if(length(id_up) < length(id_down)){
      id_up <- c(id_up, NaN)
      time_up <- c(time_up, NaN)
      name_up <- c(name_up, NaN)
    }
    if(length(id_up) > length(id_down)){
      id_down <- c(id_down, NaN)
      time_down <- c(time_down, NaN)
      name_down <- c(name_down, NaN)
    }
  }

  key <- data.frame(idUp = id_up, timeUp = time_up, nameUp = name_up,
                    idDown = id_down, timeDown = time_down, nameDown = name_down)
}
# ---------------------------------------------------------------------------- #
#' Preprocessing of key events in a trial
#'
#' This function preprocesses the key events in a given trial.
#' See also \link{extract_trialkey}
#'
#' @param trial_key data.frame -- contains the key-events in given trial
#' @param trial data.frame -- contains trial info
#' @param percept_keys matrix -- contains the keycodes for each percept
#'
#' @return list -- contains the onset and duration info for each percept by keycode
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessing_trial(trial_key,trial,percept_keys)
#' }
preprocessing_trial <- function(trial_key,trial,percept_keys){
  trial_data <- list()
  percept_keys <- as.matrix(unlist(unique(trial_key['idDown'])))
  for(p in 1:length(percept_keys)){
    theKey <- percept_keys[p]
    perceptStartTime <- trial_key[trial_key['idDown']==theKey,]
    perceptStartTime <- perceptStartTime$timeDown
    perceptEndTime   <- trial_key[trial_key['idUp']==theKey,]
    perceptEndTime   <- perceptEndTime$timeUp
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
#' @param key data.frame -- contains the preprocessed key-events in a trial
#' @param trial matrix -- contains trial info
#'
#' @return data.frame -- contains the onset and duration of each transition
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
#' @param key data.frame -- contains the preprocessed key-events of dominant
#'     percepts in a trial
#' @param trial matrix -- contains trial info
#'
#' @return data.frame -- contains the preprocessed key-events of dominant
#'     percepts and transition phases in a trial
#' @export
#'
#' @examples
#' \dontrun{
#' reorganize_preptrial(trial_key,trial)
#' }
reorganize_preptrial <- function(key,trial){
  key <- data.frame(
    idDown   = c(key$idDown, numeric(length(key$timeUp)-1)),
    timeDown = c(key$timeDown, key$timeUp[1:(length(key$timeUp)-1)])- as.numeric(unlist(trial[1])),
    idUp   = c(key$idUp, numeric(length(key$timeUp)-1)),
    timeUp   = c(key$timeUp, key$timeDown[2:length(key$timeDown)])- as.numeric(unlist(trial[1])))
  key <- key[order(key$timeDown),]
}
# ---------------------------------------------------------------------------- #
#' Saving the preprocessed data as csv
#'
#' This function reorganizes the preprocessed data of a subject and saves it as
#' table structure.You can save the output as a csv file.
#'
#' @param data list -- preprocessed data which is the output of preprocessing functions
#'
#' @importFrom utils write.csv
#'
#' @return data.frame -- preprocessed data reorganized as table
#' @export
#'
#' @examples
#' \dontrun{
#' reorganize_as_table(data,percept_keys,subjectID)
#' }
reorganize_as_table <- function(data){
  percept_keys <- data[['percept_keys']]
  subjID <- data[['subject']]
  data <- data[1:(length(data)-2)]
  id <- c()
  eye <- c()
  timeUp   <- c()
  timeDown <- c()
  session  <- c()
  run      <- c()
  trial    <- c()
  keyname  <- c()
  for(i in 1:length(data)){# each session
    for(j in 1:length(data[[i]])){# each run
      for(k in 1:length(data[[i]][[j]])){# each trial
        tmp <- data[[i]][[j]][[k]]
        tmp_id <- c()
        tmp_timeDown <- c()
        tmp_timeUp <- c()
        tmp_eye <- c()
        for(l in 1:length(tmp)){# each percept key
          curr_tmp <- tmp[[l]]
          tmp_id  <- c(tmp_id, curr_tmp$key)
          tmp_eye  <- c(tmp_eye, curr_tmp$eye_info)
          tmp_timeDown <- c(tmp_timeDown, curr_tmp$onset)
          tmp_timeUp <- c(tmp_timeUp, curr_tmp$onset + curr_tmp$duration)
        }
        index_order <- order(tmp_timeDown)
        id <- c(id, tmp_id[index_order])
        eye <- c(eye, tmp_eye[index_order])
        timeUp <- c(timeUp, tmp_timeUp[index_order])
        timeDown <- c(timeDown, tmp_timeDown[index_order])
        trial <- c(trial,(numeric(length(tmp_timeUp)) + k))
        run <- c(run,(numeric(length(tmp_timeUp)) + j))
        session  <- c(session, (numeric(length(tmp_timeUp)) + i))
      }
    }
  }
  subject <- rep(subjID,length(timeDown))
  keyname[id == percept_keys[1,1]] <- percept_keys[2,1]
  keyname[id == percept_keys[1,2]] <- percept_keys[2,2]
  keyname[id == 0] <- 'Transition'
  duration <- timeUp - timeDown
  table <- data.frame(subject,session,run,trial,keyname,id,eye,timeDown,timeUp,duration)
}
# ---------------------------------------------------------------------------- #
#' Creating a fake trial data with nan values
#' This function creates a list similar to the output of \link{preprocessing_trial}
#' but only with nan values. This is for the trials with no key event was recorded.
#'
#' @param percept_keys matrix -- contains the keycodes for each percept
#'
#' @return list -- contains the onset and duration info for each percept by keycode
#' @export
#'
#' @examples
#' \dontrun{create_nan_trialdata(percept_keys)}
create_nan_trialdata <- function(percept_keys){
  trial_data <- list()
  percept_keys <- as.matrix(unlist(unique(trial_key['idDown'])))
  for(p in 1:length(percept_keys)){
    theKey <- percept_keys[p]
    perceptStartTime <- NaN
    perceptEndTime   <- NaN
    trial_data[[p]]  <- data.frame(key  = theKey,
                                   onset = perceptStartTime,
                                   duration = perceptEndTime - perceptStartTime)
  }
  return(trial_data)
}
# ---------------------------------------------------------------------------- #
