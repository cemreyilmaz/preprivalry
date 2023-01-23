# ---------------------------------------------------------------------------- #
#' Plot all the key events
#'
#' @param data list or dataframe -- It contains all the information about the
#'     experimental sessions. It can be also the data of one run or one trial
#'     without defining the other parameters.
#' @param sessionNo numeric -- the session of which data you want to visual.
#' @param runNo numeric -- the experimental run of which data you want to visual.
#' @param trialNo numeric -- the trial of which data you want to visual.
#'
#' @import ggplot2 dplyr
#'
#' @return ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' data <- table(keyname,timeDown,timeUp,duration)
#' plot_keyevents(data,sessionNo,runNo,trialNo)}
plot_keyevents <- function(data,sessionNo,runNo,trialNo){
  # if data is given as list, convert it into table
  if(!is.data.frame(data)){
    data <- preprivalry::reorganize_as_table(data)
  }
  # filter the data by the given parameters
  if(!missing(sessionNo)){
    if(missing(runNo)){
      data <- dplyr::filter(data,id!=0,session==sessionNo)
    }
    else{
      if(missing(trialNo)){
        data <- dplyr::filter(data,id!=0,session==sessionNo,run==runNo)
      }
      else{
        data <- dplyr::filter(data,id!=0,session==sessionNo,run==runNo,trial==trialNo)
      }
    }
  }
  # plot the filtered data
  data$keyname <- as.factor(data$keyname)
  p <- ggplot2::ggplot(as.data.frame(data)) +
    # first, key-presses
    ggplot2::geom_point(ggplot2::aes(x=timeDown, y=keyname, colour=as.factor('Key Press')),size=.5,
                        stat = "identity", alpha=0.7) +
    # then, key-releases
    ggplot2::geom_point(ggplot2::aes(x=timeUp, y=keyname, colour=as.factor('Key Release')),size=.5,
                        stat = "identity", alpha=0.7) +
    ggplot2::scale_colour_manual(values=c('Key Press'='deepskyblue', 'Key Release'='orangered')) +
    ggplot2::labs(title='Key Events', x='Time (sec)', y=NULL, col=NULL) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position="top")
  p
  return(p)
}
# ---------------------------------------------------------------------------- #
