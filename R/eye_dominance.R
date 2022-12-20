# ---------------------------------------------------------------------------- #
#' Adding eye info for each trial
#'
#' This function defines which eye was being presented with the dominant percept
#' for every data point. It creates a vector and add this vector carrying the eye
#' info to the data that is given as input. It assumes that the locations of two
#' images were interchanged from one trial to another by default.
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
