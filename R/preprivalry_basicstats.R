# ---------------------------------------------------------------------------- #
# Basic stats for binocular rivalry
# ---------------------------------------------------------------------------- #
#' Descriptive statistics of a trial
#'
#' @param data data.frame -- contains key events of a trial
#'
#' @return matrix -- contains median, mean, standard deviation, and sample size
#'    calculated for each unique percept key in the given data
#'
#' @importFrom stats median sd
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- table(id,timeDown,timeUp,duration)
#' descriptive_trial <- function(data)
#' }
descriptive_trial <- function(data){
  keys <- unique(data[["id"]])
  durations <- data[["duration"]]
  percept_key <- c()
  trial_stat <- c()
  for(k in 1:length(keys)){
          percept_key <- c(percept_key,keys[k])
          curr_dur <- durations[data[["id"]]==keys[k]]
          trial_stat <- rbind(trial_stat,
                             c(stats::median(curr_dur),mean(curr_dur),
                               stats::sd(curr_dur),sum(curr_dur),length(curr_dur)))
  }
  curr_dur <- durations[data[["id"]] > 0] # only dominant percepts
  trial_stat <- rbind(trial_stat,
                      c(stats::median(curr_dur),mean(curr_dur),
                        stats::sd(curr_dur),sum(curr_dur),length(curr_dur)))
  if(is.element("eye_info",colnames(data))){# if there is eye info
    eyes <- unique(data[["eye_info"]])
    eyes <- eyes[eyes > 0]
    for(j in 1:length(eyes)){
      curr_dur <- durations[data[["eye_info"]] == eyes[j]]
      trial_stat <- rbind(trial_stat,
                          c(stats::median(curr_dur),mean(curr_dur),
                            stats::sd(curr_dur),sum(curr_dur),length(curr_dur)))
    }
  }
  curr_dur <- durations # overall
  trial_stat <- rbind(trial_stat,
                      c(stats::median(curr_dur),mean(curr_dur),
                        stats::sd(curr_dur),sum(curr_dur),length(curr_dur)))
  row_names_output <- c(paste('percept_',keys,sep=''),'dominant_percepts')
  if(is.element("eye_info",colnames(data))){
    row_names_output <- c(row_names_output,paste('eye_',eyes,sep=''))
  }
  row_names_output <- c(row_names_output,'overall')
  rownames(trial_stat) <- row_names_output
  colnames(trial_stat) <- c('median','mean','std','total_duration','N')
  return(trial_stat)
}
# ---------------------------------------------------------------------------- #
#' Adding eye info for each dominant percept
#'
#' This function defines which eye was being presented with the dominant percept
#' for every data point. It assumes that the locations of two images were
#' interchanged from one trial to another by default.
#'
#' @param data data.frame -- binocular rivalry data
#' @param eye_trials numeric -- trial labels for different presentations
#' @param trial_sequence numeric -- trial labels for the actual experiment
#'
#' @return data.frame -- binocular rivalry data added with eye info
#' @export
#'
#' @examples
#' \dontrun{
#' data <- table(trial,id,timeDown,timeUp)
#' output <- eye_dominance(data,c(1,2),c(1,2))}
eye_dominance <- function(data,eye_trials,trial_sequence){
  if(missing(eye_trials)){
    eye_trials <- c(1,2)
  }
  if(missing(trial_sequence)){
    trial_sequence <- c(1,2)
  }
  keys <- unique(data[["id"]])
  keys <- keys[keys > 0] # no mixed perception here
  trials <- unique(data[["trial"]])
  data[["eye_info"]] <- c()
  for(t in trials){
    if(length(keys)==1){
      warning('Eye dominance could not be determined because only one percept was reported!')
    } else{
      eye_vector <- numeric(dim(data[data$trial == t,])[1])
      if(is.element(trial_sequence[t],eye_trials[1])){
        eye_vector[data[["id"]]==keys[1]] <- 1
        eye_vector[data[["id"]]==keys[2]] <- 2
      } else if(is.element(trial_sequence[t],eye_trials[2])){
        eye_vector[data[["id"]]==keys[1]] <- 2
        eye_vector[data[["id"]]==keys[2]] <- 1
      } else{
        warning('Undefined eye information in this trial!')
      }
    }
    data[["eye_info"]][data$trial == t] <- eye_vector
  }
  return(data)
}
# ---------------------------------------------------------------------------- #
