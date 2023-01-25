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
  if(!is.null(data$percept_keys)){
    # then, it is subject data with additional fields. we need to remove those
    # additional fields before reorganizing it
    percept_keys <- data[['percept_keys']]
    subjID <- data[['subject']]
    data <- data[1:(length(data)-2)]
  }
  output <- data.frame(session = c(), run = c(), trial = c(), id = c(),
                       eye = c(), timeDown = c(), timeUp = c(), duration = c())

  if(is.data.frame(data)){ # trial data
    tmp         <- reorganize_trial_data(data)
    tmp$run     <- tmp$run+1
    tmp$trial   <- tmp$trial+1
    tmp$session <- tmp$session+1
    output      <- rbind(output,tmp)
  }else{ for(i in 1:length(data)){
      data1 <- data[[i]]
      if(is.data.frame(data1[[1]])){ # run data
        tmp <- reorganize_trial_data(data1)
        tmp$run     <- tmp$run+1
        tmp$trial   <- tmp$trial+i
        tmp$session <- tmp$session+1
        output      <- rbind(output,tmp)
      }else{ for(j in 1:length(data1)){
          data2 <- data1[[j]]
          if(is.data.frame(data2[[1]])){ # session data
            tmp <- reorganize_trial_data(data2)
            tmp$run     <- tmp$run+j
            tmp$trial   <- tmp$trial+i
            tmp$session <- tmp$session+1
            output      <- rbind(output,tmp)
          }else{
            for(k in 1:length(data2)){
              data3 <- data2[[k]]
              if(is.data.frame(data3[[1]])){ # subject data
                tmp <- reorganize_trial_data(data3)
                tmp$run     <- tmp$run+j
                tmp$trial   <- tmp$trial+i
                tmp$session <- tmp$session+k
                output      <- rbind(output,tmp)
              }
            }
          }
        }
      }
    }
  }
  subject <- rep(subjID,length(output$timeDown))
  keyname <- rep(percept_keys[2,1],length(output$id))
  keyname[output$id == percept_keys[1,2]] <- percept_keys[2,2]
  keyname[output$id == 0] <- 'Transition'
  duration <- output$timeUp - output$timeDown
  session <- output$session
  run <- output$run
  trial <- output$trial
  id <- output$id
  eye_info <- output$eye_info
  timeDown <- output$timeDown
  timeUp <-output$timeUp
  table <- data.frame(subject,session,run,trial,keyname,
                      id,eye_info,timeDown,timeUp,duration)
}
# ---------------------------------------------------------------------------- #
