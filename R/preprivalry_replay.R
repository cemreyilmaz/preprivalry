# ---------------------------------------------------------------------------- #
# We need to check the key events in replay session (if we have) to check
# response reliability of participants. If needed, the keyevents might be swapped
# or the participant might be completely excluded from the analysis.
# ---------------------------------------------------------------------------- #
#' Visualization of presented stimuli and subject response across time per trial
#' This function extracts the data for each trial in the given run and visualize
#' the participants key events as \link{plot_perceptdurations} does. It also adds
#' the presented stimulus types throughout the corresponding trial in order to
#' compare them. It results in a figure including the plots for each trial
#' in the given experimental run.
#'
#' @param directory character -- the directory that contains all the data
#' @param expType character -- the experiment name
#' @param participant character or numeric -- the subject id e.g. 's001' or simply
#'     the number of subject
#' @param session character or numeric -- the session number as numeric or in
#'     the format of 'session1'
#' @param plot_flag logical -- plot or not the resulting graph
#'
#' @return ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' replay_visualize_run(directory,"Gratings",1,3)}
replay_visualize_run <- function(directory,expType,participant,session,plot_flag){
  # check inputs
  if(is.numeric(participant)){
    participant <- paste('s',sprintf('%03d', participant),sep = '')
  }
  if(is.numeric(session)){
    session <- paste('session', session, sep = '')
  }
  if(missing(plot_flag)){
    plot_flag <- 1
  }
  # read data
  rivdata <- preprivalry::read_rivdata(directory,expType,participant,session)
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
  exp     <- preprivalry::extract_exp(rivdata)
  # extract key events
  exp_key <- preprivalry::extract_key(rivdata)
  # extract percept keys
  percept_keys <- sort(as.numeric(unlist(rivdata[["log"]][[4]][[1]])))
  # go trial-by-trial
  k <- list()
  for(t in 1:length(exp)){
    trial      <- exp[t,]
    trial_key  <- preprivalry::extract_trialkey(exp_key,trial)
    if(dim(trial_key)[1] > 1){ # if any key event was recorded
      trial_key  <- preprivalry::remove_irrelevant_keyevents(trial_key,percept_keys)
      trial_onsets <- percept_onsets[[t]][[1]]
      # prepare the data of response
      plot_resp <- data.frame(time = (c(trial_key$timeDown,trial_key$timeUp) - trial$trialStartTime),
                              y = rep("Response",2*dim(trial_key)[1]),
                              linenumber = c(1:dim(trial_key)[1],1:dim(trial_key)[1]),
                              id = c(trial_key$idDown,trial_key$idDown))
      plot_resp$id[plot_resp$id == 0] <- "Transition"
      plot_resp$id[plot_resp$id == percept_keys[1]] <- "LeftArrow"
      plot_resp$id[plot_resp$id == percept_keys[2]] <- "RightArrow"
      plot_resp$cat <- "Response"
      # prepare the data of stimulus
      plot_stim <- data.frame(time = unlist(c(trial_onsets[,1],
                                                trial_onsets[2:dim(trial_onsets)[1],1],
                                                (trial[2]-trial[1]))),
                              y = rep("Stimulus",2*dim(trial_onsets)[1]),
                              linenumber = (c(1:dim(trial_onsets)[1],1:dim(trial_onsets)[1]) + plot_resp$linenumber[length(plot_resp$linenumber)]),
                              id = c(trial_onsets[,2],trial_onsets[,2]))
      plot_stim$id[plot_stim$id == 0] <- "Transition"
      plot_stim$id[plot_stim$id == -1] <- "RightArrow"
      plot_stim$id[plot_stim$id == 1] <- "LeftArrow"
      plot_stim$cat <- "Stimulus"
      # combine data for plot
      plot_data <- rbind(plot_resp,plot_stim)
      # let's plot
      k[[t]]<- ggplot2::ggplot(plot_data, ggplot2::aes(color = id)) +
        ggplot2::geom_line(ggplot2::aes(x = time, y = y, group = linenumber), size = 5) +
        ggplot2::scale_color_manual(values=c('#3390FF','#FF5733', '#FFFC33', "33FF5E", "#FFFC22")) +
        ggplot2::labs(y = "")
    }
  }
  p <- ggpubr::ggarrange(k[[1]],k[[2]], ncol = 1, nrow = 2)
  p <- ggpubr::annotate_figure(p, top = ggpubr::text_grob(paste0(participant, " - ", expType, ": Presented Stimulus vs Subject Response"),
                                        color = "black", face = "bold", size = 14))
  p
  return(p)
}
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
#' @param session character or numeric -- the session number as numeric or in
#'     the format of 'session1'
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
  rivdata <- preprivalry::read_rivdata(directory,expType,participant,session)
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
  exp     <- preprivalry::extract_exp(rivdata)
  # extract key events
  exp_key <- preprivalry::extract_key(rivdata)
  # extract percept keys
  percept_keys <- as.numeric(unlist(rivdata[["log"]][[4]][[1]]))
  a <- exp_key$timeDown-exp_key$timeUp
  for(i in length(percept_onsets)){
    a <- c(a,(diff(percept_onsets[[i]][[1]][,1])))
  }
  d <- round(min(abs(a[!is.na(a)])), digits = 2)
  # go trial-by-trial
  output <- data.frame()
  for(t in 1:length(exp)){
    trial      <- exp[t,]
    trial_key  <- preprivalry::extract_trialkey(exp_key,trial)
    if(dim(trial_key)[1] > 1){ # if any key event was recorded
      trial_key     <- preprivalry::remove_irrelevant_keyevents(trial_key,percept_keys)
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
#' Check the significance of linear regression
#' This functions performs a linear regression and checks the p-value. It prints
#' a message about the reliability of subject response by checking the p-value
#' for alpha 0.01.
#'
#' @param data data.frame -- scaled data to a new time series
#'
#' @return lm -- linear model of data
#'
#' @importFrom stats lm
#' @importFrom stats pf
#' @export
#'
#' @examples
#' \dontrun{
#' check_lm_significance(rescale_replay_data(directory,"Gratings",1,3))}
check_lm_significance <- function(data){
  linearMod <- stats::lm(response_key ~ stimulus, data)
  f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
  model_p <- stats::pf(f[1], f[2], f[3], lower=FALSE)
  if(model_p > 0.01){
    message("Subject response seems irrelevant from the presented stimuli.")
  }
  else{
    message("You can rely on this subject.")
  }
  return(linearMod)
}
# ---------------------------------------------------------------------------- #
