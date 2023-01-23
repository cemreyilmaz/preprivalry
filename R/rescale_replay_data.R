# ---------------------------------------------------------------------------- #
#' Projection of data and presented stimulus info onto a new time series
#' This functions aims to equalize the length of data and stimulus info by
#' projecting both of them onto a new time series. The step size is defined as
#' the minimum time difference in data and stimulus presentation. Then, the
#' percept key ids are assigned to indicate either the subject response or the
#' stimulus presented at that time point.
#'
#' @param directory character -- the directory that contains all the data
#' @param expType character -- the experiment name
#' @param participant character or numeric -- the subject id e.g. 's001' or simply
#'     the number of subject
#' @param session character or numeric -- the session number of replay as numeric
#'     or in the format of 'session1'
#'
#' @return data.frame -- scaled data to a new time series
#'
#' @importFrom data.table data.table
#' @importFrom data.table setattr
#' @importFrom data.table setkey
#' @export
#'
#' @examples
#' \dontrun{
#' rescale_replay_data(directory,"Gratings",1,3)}
rescale_replay_data <- function(directory,expType,participant,session){
  # check inputs
  if(is.numeric(participant)){
    participant <- paste('s',sprintf('%03d', participant),sep = '')
  }
  if(is.numeric(session)){
    session <- paste('session', session, sep = '')
  }
  # read data
  rivdata <- read_rivdata(directory,expType,participant,session)
  # extract stimulus info
  a <- rivdata[['stimulus']][[length(rivdata[['stimulus']])]]
  if(grepl("Dots", expType, fixed=TRUE)){
    percept_peaks <- a[[length(a)-1]]
    percept_onsets <- a[[length(a)-2]]
  }else{
    percept_peaks <- a[[length(a)]]
    percept_onsets <- a[[length(a)-1]]
  }
  # extract exp info
  exp     <- extract_exp(rivdata)
  # extract key events
  exp_key <- extract_key(rivdata)
  # extract percept keys
  percept_keys <- as.numeric(unlist(rivdata[["log"]][[4]][[1]]))
  a <- exp_key$timeDown-exp_key$timeUp
  for(i in length(percept_onsets)){
    a <- c(a,(diff(percept_onsets[[i]][[1]][,1])))
  }
  d <- round(min(abs(a[!is.na(a)])), digits = 2)
  if(d==0){
    d <- round(min(abs(a[!is.na(a)])), digits = 3)
  }
  # go trial-by-trial
  output <- data.frame()
  for(t in 1:length(exp)){
    trial      <- exp[t,]
    trial_key  <- extract_trialkey(exp_key,trial)
    if(dim(trial_key)[1] > 1){ # if any key event was recorded
      trial_key     <- remove_irrelevant_keyevents(trial_key,percept_keys)
      trial_onsets  <- percept_onsets[[t]][[1]]
      trial_onsets[trial_onsets[,2] == -1,2] <- 115
      trial_onsets[trial_onsets[,2] == 1,2]  <- 114
      trial_onsets[,1] <- round(trial_onsets[,1], digits = 2)
      time_series   <- seq(from = 0, to = (trial$trialEndTime-trial$trialStartTime), by = d)
      dt <- data.table::data.table(time_series, val = time_series) # you'll see why val is needed in a sec
      data.table::setattr(dt, "sorted", "time_series")  # let data.table know that w is sorted
      data.table::setkey(dt, time_series) # sorts the data
      down_time <- round(trial_key$timeDown - trial$trialStartTime, digits = 2)
      up_time   <- round(trial_key$timeUp - trial$trialStartTime, digits = 2)
      id_resp   <- matrix(0,length(time_series),1)
      for(i in 1:length(down_time)){
        a <- dt[J(down_time[i]), roll = "nearest"]
        b <- dt[J(up_time[i]), roll = "nearest"]
        id_resp[(match(a$val,time_series)):(match(b$val,time_series))] <- trial_key$idDown[i]
      }
      id_stim   <- matrix(0,length(time_series),1)
      for(i in 1:(length(trial_onsets[,1])-1)){
        a <- dt[J(trial_onsets[i,1]), roll = "nearest"]
        b <- dt[J(trial_onsets[(i+1),1]), roll = "nearest"]
        id_stim[(match(a$val,time_series)):(match(b$val,time_series))] <- trial_onsets[i,2]
      }
      trial_output <- data.frame(time         = time_series,
                                 stimulus     = id_stim,
                                 response_key = id_resp)
      output <- rbind(output, trial_output)
    }
  }
  return(output)
}
# ---------------------------------------------------------------------------- #
