# Visualisation of preprocessed data
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
#' @import ggplot2
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
}
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
#' @import graphics
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' data <- table(keyname,timeDown,timeUp,duration)
#' plot_perceptdurations(data,sessionNo,runNo,trialNo)}
plot_perceptdurations <- function(data,sessionNo,runNo,trialNo){
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
  y <- unique(unlist(data['keyname']))
  colors = rbind(c('RightArrow','Transition','LeftArrow'),c('red','black','blue'))
  for(i in 1:length(y)){
    x <- dplyr::filter(data, keyname==y[i])
    if(i==1){
      p <- graphics::plot(rbind(as.numeric(unlist(x[1,]['timeDown'])),as.numeric(unlist(x[1,]['timeUp']))),
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
  graphics::text(y=1:length(y),par('usr')[3],labels = paste(y,'   '),
                 pos = 2, xpd = TRUE)
  return(NULL)
}
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
  par(mfrow=c(1,3)) # set the plotting area into a 1*3 array
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
  return(NULL)
}
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
#' @return
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


}
