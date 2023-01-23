# ---------------------------------------------------------------------------- #
#' Plot the duration of each percept and transition
#'
#' @param data list or dataframe -- It contains all the information about the
#'     experimental sessions. It can be also the data of one run or one trial
#'     without defining the other parameters.
#' @param sessionNo numeric -- the session of which data you want to visual.
#' @param runNo numeric -- the experimental run of which data you want to visual.
#' @param trialNo numeric -- the trial of which data you want to visual.
#'
#' @import graphics dplyr
#' @return ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' data <- table(keyname,timeDown,timeUp,duration)
#' plot_perceptdurations(data,sessionNo,runNo,trialNo)}
plot_perceptdurations <- function(data,sessionNo,runNo,trialNo){
  if(!is.data.frame(data)){
    data <- reorganize_as_table(data)
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
  y <- unique(unlist(data['keyname']))
  colors = rbind(c('RightArrow','Transition','LeftArrow'),c('red','black','blue'))
  for(i in 1:length(y)){
    x <- dplyr::filter(data, keyname==y[i])
    if(i==1){
      graphics::plot(rbind(as.numeric(unlist(x[1,]['timeDown'])),as.numeric(unlist(x[1,]['timeUp']))),
                     c(i,i), type='l', xlim=limits, ylim=c(0,4), col=colors[2,colors[1,]==y[i]], yaxt="n",
                     main='Sorted Percepts', ylab='', xlab='Time (sec)')
      for(j in 2:dim(x)[1]){
        graphics::lines(rbind(as.numeric(unlist(x[j,]['timeDown'])),as.numeric(unlist(x[j,]['timeUp']))),
                        c(i,i), type='l',col=colors[2,colors[1,]==y[i]])
      }
    }
    else{
      for(j in 1:dim(x)[1]){
        graphics::lines(rbind(as.numeric(unlist(x[j,]['timeDown'])),as.numeric(unlist(x[j,]['timeUp']))),
                        c(i,i), type='l', col=colors[2,colors[1,]==y[i]])
      }
    }
  }
  # rename yticks
  axis(side=2, at=1:3, labels = FALSE)
  p <- graphics::text(y=1:length(y),par('usr')[3],labels = paste(y,'   '),
                 pos = 2, srt=30, xpd = TRUE)
  return(p)
}
# ---------------------------------------------------------------------------- #
