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
#' replay_visualize_run(directory,expType,participant,3)}
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
  percept_keys <- as.numeric(unlist(rivdata[["log"]][[4]][[1]]))
  # go trial-by-trial
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
      plot_resp$id[plot_resp$id == 115] <- "LeftArrow"
      plot_resp$id[plot_resp$id == 114] <- "RightArrow"
      plot_resp$cat <- "Response"
      # prepare the data of stimulus
      plot_stim <- data.frame(time = unlist(c(trial_onsets[,1],
                                                trial_onsets[2:dim(trial_onsets)[1],1],
                                                (trial[2]-trial[1]))),
                              y = rep("Stimulus",2*dim(trial_onsets)[1]),
                              linenumber = (c(1:dim(trial_onsets)[1],1:dim(trial_onsets)[1]) + plot_resp$linenumber[length(plot_resp$linenumber)]),
                              id = c(trial_onsets[,2],trial_onsets[,2]))
      plot_stim$id[plot_stim$id == 0] <- "Transition"
      plot_stim$id[plot_stim$id == -1] <- "LeftArrow"
      plot_stim$id[plot_stim$id == 1] <- "RightArrow"
      plot_stim$cat <- "Stimulus"
      # combine data for plot
      plot_data <- rbind(plot_resp,plot_stim)
      # let's plot
      k[[t]]<- ggplot2::ggplot(plot_data, ggplot2::aes(color = id)) +
        ggplot2::geom_line(ggplot2::aes(x = time, y = y, group = linenumber), size = 5) +
        ggplot2::scale_color_manual(values=c('#3390FF','#FF5733', '#FFFC33', "33FF5E")) +
        ggplot2::labs(y = "")
    }
  }
  p <- ggpubr::ggarrange(k[[1]],k[[2]], ncol = 1, nrow = 2)
  p <- ggpubr::annotate_figure(p, top = ggpubr::text_grob(paste0(participant, " - ", expType, ": Presented Stimulus vs Subject Response"),
                                        color = "black", face = "bold", size = 14))
  p
  return(p)
}
