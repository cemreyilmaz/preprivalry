# ---------------------------------------------------------------------------- #
#' Draw histogram of percept durations
#'
#' @param data list or dataframe -- It contains all the information about the
#'     experimental sessions. It can be also the data of one run or one trial
#'     without defining the other parameters.
#' @param sessionNo numeric -- the session of which data you want to visual.
#' @param runNo numeric -- the experimental run of which data you want to visual.
#' @param trialNo numeric -- the trial of which data you want to visual.
#'
#' @import dplyr graphics
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' data <- table(keyname,timeDown,timeUp,duration)
#' hist_perceptdurations(data,sessionNo,runNo,trialNo)}
hist_perceptdurations <- function(data,sessionNo,runNo,trialNo){
  if(!is.data.frame(data)){
    data <- preprivalry::reorganize_as_table(data)
  }
  # filter the data by the given parameters
  if(!missing(sessionNo)){
    if(missing(runNo)){
      warning('You need to select an experimental run at least!')
    }
    else{
      if(missing(trialNo)){
        data <- dplyr::filter(data,session==sessionNo,run==runNo)
        limits <- c(0,260)
      }
      else{
        data <- dplyr::filter(data,session==sessionNo,run==runNo,trial==trialNo)
        limits <- c(0,120)
      }
    }
  }else{
    warning('You need to select a sesssion and an experimental run at least!')
  }
  # plot the filtered data
  y <- unique(as.numeric(unlist(data['id'])))
  colors = c('red','black','blue')
  p <- par(mfrow=c(1,3)) # set the plotting area into a 1*3 array
  for(i in 1:3){
    x <- dplyr::filter(data, id==y[i])
    x <- dplyr::filter(data, duration <= 8)
    graphics::hist(as.numeric(unlist(x['duration'])),
                   breaks=0:8,
                   main=paste('Percept of Key', y[i]),
                   xlab='Duration (sec)',
                   ylab='Frequency',
                   col=colors[i],
                   freq=FALSE)
  }
  return(p)
}
# ---------------------------------------------------------------------------- #
