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
  eye_info <- c()
  timeUp   <- c()
  timeDown <- c()
  session  <- c()
  run      <- c()
  trial    <- c()
  for(l in 1:length(data)){# each percept key
    curr_data <- data[[l]]
    id  <- c(id, curr_data$key)
    eye_info  <- c(eye_info, curr_data$eye_info)
    timeDown <- c(timeDown, curr_data$onset)
    timeUp <- c(timeUp, curr_data$onset + curr_data$duration)
  }
  index_order <- order(timeDown)
  id <- id[index_order]
  if(is.null(eye_info)){
    eye_info <-(rep("NA",length(id)))
  }else{
    eye_info <- eye_info[index_order]
  }
  timeUp <- timeUp[index_order]
  timeDown <- timeDown[index_order]
  trial <- c(trial,(numeric(length(timeUp))))
  run <- c(run,(numeric(length(timeUp))))
  session  <- c(session, (numeric(length(timeUp))))

  duration <- timeUp - timeDown
  table <- data.frame(session,run,trial,id,eye_info,timeDown,timeUp)

  return(table)
}
