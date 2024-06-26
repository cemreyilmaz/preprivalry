# ---------------------------------------------------------------------------- #
#' Descriptive statistics of a trial for each eye
#'
#' @param data data.frame -- contains key events of a trial
#'
#' @return matrix -- contains median, mean, standard deviation, and sample size
#'    calculated for each unique percept key in the given data
#'
#' @importFrom stats median sd mad na.omit
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data  <- preprocessing_run(directory,exp_type,participant_id,session_no)
#' stats <- descriptive_eye(data)
#' }
descriptive_eye <- function(data){
  if(!is.data.frame(data)){
    data <- reorganize_as_table(data)
  }
  eyes <- unique(data[["eye_info"]])
  durations <- data[["duration"]]
  percept_eye <- c()
  eye_stat <- c()
  for(k in 1:length(eyes)){
    percept_eye <- c(percept_eye,eyes[k])
    curr_dur <- durations[data[["eye_info"]]==eyes[k]]
    eye_stat <- rbind(eye_stat,
                      c(stats::median(stats::na.omit(curr_dur)),stats::mad(stats::na.omit(curr_dur)),
                        mean(stats::na.omit(curr_dur)),stats::sd(stats::na.omit(curr_dur)),
                        sum(stats::na.omit(curr_dur)),length(stats::na.omit(curr_dur))))
  }
  curr_dur <- durations[data[["id"]] > 0] # only dominant percepts
  eye_stat <- rbind(eye_stat,
                    c(stats::median(stats::na.omit(curr_dur)),stats::mad(stats::na.omit(curr_dur)),
                      mean(stats::na.omit(curr_dur)),stats::sd(stats::na.omit(curr_dur)),
                      sum(stats::na.omit(curr_dur)),length(stats::na.omit(curr_dur))))

  curr_dur <- durations # overall
  eye_stat <- rbind(eye_stat,
                      c(stats::median(stats::na.omit(curr_dur)),stats::mad(stats::na.omit(curr_dur)),
                        mean(stats::na.omit(curr_dur)),stats::sd(stats::na.omit(curr_dur)),
                        sum(stats::na.omit(curr_dur)),length(stats::na.omit(curr_dur))))
  row_names_output <- c(eyes,'dominant_eyes','overall')
  rownames(eye_stat) <- row_names_output
  colnames(eye_stat) <- c('median','mad','mean','std','total_duration','N')
  return(eye_stat)
}
# ---------------------------------------------------------------------------- #
