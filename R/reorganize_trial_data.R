#' Reorganize trial data as table
#'
#' @param data list
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{reorganize_trial_data(trial_data)}
reorganize_trial_data <- function(data){
  id <- c()
  eye <- c()
  timeUp   <- c()
  timeDown <- c()
  session  <- c()
  run      <- c()
  trial    <- c()
  keyname  <- c()
  for(l in 1:length(data)){# each percept key
    curr_data <- data[[l]]
    id  <- c(id, curr_data$key)
    eye  <- c(eye, curr_data$eye_info)
    timeDown <- c(timeDown, curr_data$onset)
    timeUp <- c(timeUp, curr_data$onset + curr_data$duration)
  }
  index_order <- order(timeDown)
  id <- id[index_order]
  eye <- eye[index_order]
  timeUp <- timeUp[index_order]
  timeDown <- timeDown[index_order]
  trial <- c(trial,(numeric(length(timeUp)) + k))
  run <- c(run,(numeric(length(timeUp)) + j))
  session  <- c(session, (numeric(length(timeUp)) + i))

  subject <- rep(subjID,length(timeDown))
  keyname[id == percept_keys[1,1]] <- percept_keys[2,1]
  keyname[id == percept_keys[1,2]] <- percept_keys[2,2]
  keyname[id == 0] <- 'Transition'
  duration <- timeUp - timeDown
  table <- data.frame(run,trial,keyname,id,eye,timeDown,timeUp,duration)

  return(table)
}
