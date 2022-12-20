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
