# ---------------------------------------------------------------------------- #
#' Barplot as a statistical summary of given data
#'
#' @param data list or dataframe -- It contains all the information about the
#'     experimental sessions. It can be also the data of one run or one trial
#'     without defining the other parameters.
#' @param sessionNo numeric -- the session of which data you want to visual.
#' @param runNo numeric -- the experimental run of which data you want to visual.
#' @param trialNo numeric -- the trial of which data you want to visual.
#'
#' @import ggplot2 ggpubr
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' data <- table(keyname,timeDown,timeUp,duration)
#' bar_stats(data,sessionNo,runNo,trialNo)}
bar_stats <- function(data,sessionNo,runNo,trialNo){
  if(!is.data.frame(data)){
    data <- reorganize_as_table(data)
  }
  # filter the data by the given parameters
  if(!missing(sessionNo)){
    if(missing(runNo)){
      data <- dplyr::filter(data,session==sessionNo)
      title <- paste('Statistics from Session',sessionNo)
    }
    else{
      if(missing(trialNo)){
        data <- dplyr::filter(data,session==sessionNo,run==runNo)
        title <- paste('Statistics from Session',sessionNo,'Run',runNo)
      }
      else{
        data <- dplyr::filter(data,session==sessionNo,run==runNo,trial==trialNo)
        title <- paste('Statistics from Session',sessionNo,'Run',runNo,'Trial',trialNo)
      }
    }
  }else{
    warning('You need to select a sesssion at least!')
  }
  # prepare for the plots
  stats_riv <- preprivalry::descriptive_trial(data)
  stats_eye <- preprivalry::descriptive_eye(data)
  par(mfrow=c(2,3))
  # ---------- keys ---------- #
  percept_keys <- sort(as.array(unlist(unique(data['keyname']))))
  labels_key <- factor(c(percept_keys,'Both Key', 'Overall'), levels = c(percept_keys,'Both Key', 'Overall'))
  # 1, median(percept_keys) w/ errorbars of median absolute deviation
  k1 <- ggplot2::ggplot(as.data.frame(stats_riv)) +
    ggplot2::geom_bar(ggplot2::aes(x=labels_key, y=median),
                      stat="identity", fill="skyblue", alpha=0.7) +
    ggplot2::geom_errorbar( ggplot2::aes(x=labels_key,ymin=median-stats_riv[,'mad'],
                            ymax=median+stats_riv[,'mad']),
                            width=0.4, colour="deepskyblue", alpha=0.9, size=1.3) +
    ggplot2::labs(title='Median Percept', x=NULL, y='Duration (sec)') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  # 2, mean(percept_keys) w/ errorbars of standard error
  se = stats_riv[,'std'] / sqrt(stats_riv[,'N'])
  k2 <- ggplot2::ggplot(as.data.frame(stats_riv)) +
    ggplot2::geom_bar(ggplot2::aes(x=labels_key, y=mean),
                      stat="identity", fill="skyblue", alpha=0.7) +
    ggplot2::geom_errorbar( ggplot2::aes(x=labels_key,ymin=mean-stats_riv[,'mad'],
                                         ymax=mean+stats_riv[,'mad']),
                            width=0.4, colour="deepskyblue", alpha=0.9, size=1.3) +
    ggplot2::labs(title='Mean Percept', x=NULL, y='Duration (sec)') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  # 3, sum(percept_keys) no errorbar naturally
  k3 <- ggplot2::ggplot(as.data.frame(stats_riv)) +
    ggplot2::geom_bar(ggplot2::aes(x=labels_key, y=total_duration),
                      stat="identity", fill="skyblue", alpha=0.7) +
    ggplot2::labs(title='Total Percept', x=NULL, y='Duration (sec)') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  # ---------- eyes ---------- #
  percept_eyes <- sort(as.array(unlist(unique(data['eye']))))
  labels_eye <- factor(c(percept_eyes,'Both Eye', 'Overall'),levels=factor(c(percept_eyes,'Both Eye', 'Overall')))
  # 1, median(percept_eyes) w/ errorbars of median absolute deviation
  e1 <- ggplot2::ggplot(as.data.frame(stats_eye)) +
    ggplot2::geom_bar(ggplot2::aes(x=labels_eye, y=median),
                      stat="identity", fill="skyblue", alpha=0.7) +
    ggplot2::geom_errorbar( ggplot2::aes(x=labels_eye,ymin=median-stats_eye[,'mad'],
                                         ymax=median+stats_eye[,'mad']),
                            width=0.4, colour="deepskyblue", alpha=0.9, size=1.3) +
    ggplot2::labs(title='Median Eye', x=NULL, y='Duration (sec)') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  # 2, mean(percept_eyes) w/ errorbars of standard error
  se = stats_eye[,'std'] / sqrt(stats_eye[,'N'])
  e2 <- ggplot2::ggplot(as.data.frame(stats_eye)) +
    ggplot2::geom_bar(ggplot2::aes(x=labels_eye, y=mean),
                      stat="identity", fill="skyblue", alpha=0.7) +
    ggplot2::geom_errorbar( ggplot2::aes(x=labels_eye,ymin=mean-stats_eye[,'mad'],
                                         ymax=mean+stats_eye[,'mad']),
                            width=0.4, colour="deepskyblue", alpha=0.9, size=1.3) +
    ggplot2::labs(title='Mean Eye', x=NULL, y='Duration (sec)') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  # 3, sum(percept_eyes) no errorbar naturally
  e3 <- ggplot2::ggplot(as.data.frame(stats_eye)) +
    ggplot2::geom_bar(ggplot2::aes(x=labels_eye, y=total_duration),
                      stat="identity", fill="skyblue", alpha=0.7) +
    ggplot2::labs(title='Total Eye', x=NULL, y='Duration (sec)') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  p <- ggpubr::ggarrange(k1,k2,k3,e1,e2,e3, ncol = 3, nrow = 2)
  p
}
