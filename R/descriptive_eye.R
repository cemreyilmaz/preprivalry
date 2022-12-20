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
