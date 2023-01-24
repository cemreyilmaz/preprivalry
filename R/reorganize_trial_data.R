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
  for(l in 1:length(data)){# each percept key
    curr_data <- data[[l]]
    id  <- c(id, curr_data$key)
    eye  <- c(eye, curr_data$eye_info)
    timeDown <- c(timeDown, curr_data$onset)
    timeUp <- c(timeUp, curr_data$onset + curr_data$duration)
  }
  index_order <- order(timeDown)
  id <- id[index_order]
  if(is.null(eye)){
    eye <-(rep("NA",length(id)))
  }else{
    eye <- eye[index_order]
  }
  timeUp <- timeUp[index_order]
  timeDown <- timeDown[index_order]
  trial <- c(trial,(numeric(length(timeUp))))
  run <- c(run,(numeric(length(timeUp))))
  session  <- c(session, (numeric(length(timeUp))))

  duration <- timeUp - timeDown
  table <- data.frame(session,run,trial,id,eye,timeDown,timeUp)

  return(table)
}
