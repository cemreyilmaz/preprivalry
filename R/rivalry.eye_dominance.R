#' Eye Dominance based on ongoing rivalry task
#'
#' This function calculates the eye dominance based on a rivalry data.
#'
#' for more details see: https://doi.org/10.1167/18.6.6
#'
#' @param data data.frame -- contains key events of a trial
#'
#' @return numeric -- eye dominance as percentage
#' @export
#'
#' @example \dontrun{
#' data  <- preprocessing_run(directory,exp_type,participant_id,session_no)
#' dominance <- rivalry.eye.dominance(data)
#' }
rivalry.eye_dominance <- function(data){
  stat_eye  <- preprivalry::descriptive_eye(data)
  stat_eye  <- curr_descriptives[order(row.names(stat_eye)),]
  dominance <- stat_eye[2,1]/(stat_eye[2,1]+stat_eye[3,1])
  return(dominance)
}
