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
#' @importFrom stats median sd mad
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- table(id,timeDown,timeUp,duration)
#' stats <- descriptive_trial(data)
#' }
descriptive_trial <- function(data){
  if(!is.data.frame(data)){
    data <- preprivalry::reorganize_as_table(data)
  }
  keys <- unique(data[["id"]])
  durations <- data[["duration"]]
  percept_key <- c()
  trial_stat <- c()
  for(k in 1:length(keys)){
          percept_key <- c(percept_key,keys[k])
          curr_dur <- durations[data[["id"]]==keys[k]]
          trial_stat <- rbind(trial_stat,
                             c(stats::median(curr_dur),stats::mad(curr_dur),
                               mean(curr_dur),stats::sd(curr_dur),
                               sum(curr_dur),length(curr_dur)))
  }
  curr_dur <- durations[data[["id"]] > 0] # only dominant percepts
  trial_stat <- rbind(trial_stat,
                      c(stats::median(curr_dur),stats::mad(curr_dur),
                        mean(curr_dur),stats::sd(curr_dur),
                        sum(curr_dur),length(curr_dur)))
  if(is.element("eye_info",colnames(data))){# if there is eye info
    eyes <- unique(data[["eye_info"]])
    eyes <- eyes[eyes > 0]
    for(j in 1:length(eyes)){
      curr_dur <- durations[data[["eye_info"]] == eyes[j]]
      trial_stat <- rbind(trial_stat,
                          c(stats::median(curr_dur),stats::mad(curr_dur),
                            mean(curr_dur),stats::sd(curr_dur),
                            sum(curr_dur),length(curr_dur)))
    }
  }
  curr_dur <- durations # overall
  trial_stat <- rbind(trial_stat,
                      c(stats::median(curr_dur),stats::mad(curr_dur),
                        mean(curr_dur),stats::sd(curr_dur),
                        sum(curr_dur),length(curr_dur)))
  row_names_output <- c(paste('percept_',keys,sep=''),'dominant_percepts')
  if(is.element("eye_info",colnames(data))){
    row_names_output <- c(row_names_output,paste('eye_',eyes,sep=''))
  }
  row_names_output <- c(row_names_output,'overall')
  rownames(trial_stat) <- row_names_output
  colnames(trial_stat) <- c('median','mad','mean','std','total_duration','N')
  return(trial_stat)
}
# ---------------------------------------------------------------------------- #
#' Descriptive statistics of a trial for each eye
#'
#' @param data data.frame -- contains key events of a trial
#'
#' @return matrix -- contains median, mean, standard deviation, and sample size
#'    calculated for each unique percept key in the given data
#'
#' @importFrom stats median sd mad
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- table(id,timeDown,timeUp,duration)
#' stats <- descriptive_eye(data)
#' }
descriptive_eye <- function(data){
  if(!is.data.frame(data)){
    data <- preprivalry::reorganize_as_table(data)
  }
  eyes <- unique(data[["eye"]])
  durations <- data[["duration"]]
  percept_eye <- c()
  eye_stat <- c()
  for(k in 1:length(eyes)){
    percept_eye <- c(percept_eye,eyes[k])
    curr_dur <- durations[data[["eye"]]==eyes[k]]
    eye_stat <- rbind(eye_stat,
                        c(stats::median(curr_dur),stats::mad(curr_dur),
                          mean(curr_dur),stats::sd(curr_dur),
                          sum(curr_dur),length(curr_dur)))
  }
  curr_dur <- durations[data[["id"]] > 0] # only dominant percepts
  eye_stat <- rbind(eye_stat,
                      c(stats::median(curr_dur),stats::mad(curr_dur),
                        mean(curr_dur),stats::sd(curr_dur),
                        sum(curr_dur),length(curr_dur)))
  if(is.element("eye_info",colnames(data))){# if there is eye info
    eyes <- unique(data[["eye_info"]])
    eyes <- eyes[eyes > 0]
    for(j in 1:length(eyes)){
      curr_dur <- durations[data[["eye_info"]] == eyes[j]]
      eye_stat <- rbind(eye_stat,
                          c(stats::median(curr_dur),stats::mad(curr_dur),
                            mean(curr_dur),stats::sd(curr_dur),
                            sum(curr_dur),length(curr_dur)))
    }
  }
  curr_dur <- durations # overall
  eye_stat <- rbind(eye_stat,
                      c(stats::median(curr_dur),stats::mad(curr_dur),
                        mean(curr_dur),stats::sd(curr_dur),
                        sum(curr_dur),length(curr_dur)))
  row_names_output <- c(paste('eye_',eyes,sep=''),'dominant_eyes')
  if(is.element("eye_info",colnames(data))){
    row_names_output <- c(row_names_output,paste('eye_',eyes,sep=''))
  }
  row_names_output <- c(row_names_output,'overall')
  rownames(eye_stat) <- row_names_output
  colnames(eye_stat) <- c('median','mad','mean','std','total_duration','N')
  return(eye_stat)
}
# ---------------------------------------------------------------------------- #
#' Adding eye info for each dominant percept
#'
#' This function defines which eye was being presented with the dominant percept
#' for every data point. It assumes that the locations of two images were
#' interchanged from one trial to another by default.
#'
#' @param key_data list -- output of run preprocessing
#' @param eye_trials numeric -- trial labels for different presentations
#' @param trial_sequence numeric -- trial labels for the actual experiment
#'
#' @return list -- binocular rivalry key_data added with eye info
#' @export
#'
#' @examples
#' \dontrun{
#' key_data <- preprocessing_run(directory,expType,participant,session)
#' output <- eye_dominance(key_data,c(1,2),c(1,2))}
eye_dominance <- function(key_data,eye_trials,trial_sequence){
  if(missing(eye_trials)){
    eye_trials <- c(1,2)
  }
  if(missing(trial_sequence)){
    trial_sequence <- c(1,2)
  }
  for(t in 1:length(key_data)){
    curr_data <- key_data[[t]]
    percept_keys <- c()
    for(k in 1:length(curr_data)){
      percept_keys[k] <- as.numeric(unlist(unique(curr_data[[k]]['key'])))
    }
    percept_keys <- sort(percept_keys)
    for(k in 1:length(curr_data)){
      keys <- curr_data[[k]]['key']
      eye_vector <- numeric(dim(keys)[1])
      if(is.element(trial_sequence[t],eye_trials[1])){
        eye_vector[keys==percept_keys[3]] <- 'RightEye' # right arrow
        eye_vector[keys==percept_keys[2]] <- 'LeftEye' # left arrow
      } else if(is.element(trial_sequence[t],eye_trials[2])){
        eye_vector[keys==percept_keys[3]] <- 'LeftEye' # right arrow
        eye_vector[keys==percept_keys[2]] <- 'RightEye' # left arrow
      } else{
        warning('Undefined eye information in this trial!')
      }
      eye_vector[keys==percept_keys[1]] <- 'Transition'
      key_data[[t]][[k]]['eye_info'] <- eye_vector
    }
  }
  return(key_data)
}
# ---------------------------------------------------------------------------- #
