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
                        c(stats::median(stats::na.omit(curr_dur)),stats::mad(stats::na.omit(curr_dur)),
                          mean(stats::na.omit(curr_dur)),stats::sd(stats::na.omit(curr_dur)),
                          sum(stats::na.omit(curr_dur)),length(stats::na.omit(curr_dur))))
  }
  curr_dur <- durations[data[["id"]] > 0] # only dominant percepts
  trial_stat <- rbind(trial_stat,
                      c(stats::median(stats::na.omit(curr_dur)),stats::mad(stats::na.omit(curr_dur)),
                        mean(stats::na.omit(curr_dur)),stats::sd(stats::na.omit(curr_dur)),
                        sum(stats::na.omit(curr_dur)),length(stats::na.omit(curr_dur))))
  curr_dur <- durations # overall
  trial_stat <- rbind(trial_stat,
                      c(stats::median(stats::na.omit(curr_dur)),stats::mad(stats::na.omit(curr_dur)),
                        mean(stats::na.omit(curr_dur)),stats::sd(stats::na.omit(curr_dur)),
                        sum(stats::na.omit(curr_dur)),length(stats::na.omit(curr_dur))))
  row_names_output <- c(paste('percept_',keys,sep=''),'dominant_percepts','overall')
  rownames(trial_stat) <- row_names_output
  colnames(trial_stat) <- c('median','mad','mean','std','total_duration','N')
  return(trial_stat)
}
# ---------------------------------------------------------------------------- #
