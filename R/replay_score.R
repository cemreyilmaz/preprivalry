#' Replay Score
#'
#' This function calculates the ratio of correct responses of a given subject in
#' a given run as their replay score. The input data must be the scaled data to a
#' new time series by \link{rescale_replay_data()}
#'
#' @param data data.frame -- scaled data
#'
#' @return numeric -- replay score
#' @export
#'
#' @examples \dontrun{
#' data<- preprivalry::rescale_replay_data(directory, experiment, subject, 3)
#' replay_score(data)}
replay_score <- function(data){
  b               <- rep(0, length(data[,1]))
  b[data[,2]==data[,3]] <- 1
  score           <- sum(b)/length(b)
  for(shift in 1:length(data[,1])){
    shifted_response <- c(data[2:length(data[,3]),3],0)
    data                <- cbind(data[,1],data[,2],shifted_response)
    b                <- rep(0, length(data[,1]))
    b[data[,2]==data[,3]]  <- 1
    if(sum(b)/length(b) > score){
      score <- sum(b)/length(b)
    } else if(sum(b)/length(b) < score){
      break
    }
  }
  message(paste("shifted by",shift,"rows"))
  return(score)
}


