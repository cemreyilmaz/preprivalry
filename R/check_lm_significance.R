# ---------------------------------------------------------------------------- #
#' Check the significance of linear regression
#' This functions performs a linear regression and checks the p-value. It prints
#' a message about the reliability of subject response by checking the p-value
#' for alpha 0.01.
#'
#' @param data data.frame -- scaled data to a new time series
#'
#' @return lm -- linear model of data
#'
#' @importFrom stats lm
#' @importFrom stats pf
#' @export
#'
#' @examples
#' \dontrun{
#' check_lm_significance(rescale_replay_data(directory,"Gratings",1,3))}
check_lm_significance <- function(data){
  linearMod <- stats::lm(response_key ~ stimulus, data)
  f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
  model_p <- stats::pf(f[1], f[2], f[3], lower=FALSE)
  if(model_p > 0.01){
    message("Subject response seems irrelevant from the presented stimuli.")
  }
  else{
    message("You can rely on this subject.")
  }
  return(linearMod)
}
# ---------------------------------------------------------------------------- #
