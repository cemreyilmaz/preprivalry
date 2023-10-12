# ---------------------------------------------------------------------------- #
# replay score
# ---------------------------------------------------------------------------- #
#' Replay Score calculated as event-based
#'
#' @param data list
#' @param expType character
#'
#' @return numeric
#' @export
#'
#' @examples \dontrun{
#' eventbased_replay(read_rivdata(sourcefold,expType,subj_id,3),expType)}
eventbased_replay <- function(data, expType="None"){
  # extract stimulus info
  a <- data[['stimulus']][[length(data[['stimulus']])]]
  # extract transparency vector
  if(expType=="Dots"){
    transperancies <- a[3][[1]]
  }else{
    transparencies <- a[5][[1]]
  }
  # extract exp info
  exp     <- extract_exp(data)
  # extract key events
  exp_key <- extract_key(data)
  # extract percept keys
  percept_keys <- as.numeric(unlist(data[["log"]][[4]][[1]]))
  # 75% transparency is considered as full stimulus, others transition
  scores <- c()
  labs_data <- c()
  for(t in 1:length(exp)){
    trial_transperancy <- transparencies[[t]][[1]]
    # binarize transparency vector
    trial_transperancy[abs(trial_transperancy) < 0.75] <- 0
    trial_transperancy[trial_transperancy > 0.75] <- 1
    trial_transperancy[trial_transperancy < -0.75] <- -1
    # remove events that took less then 500 ms (30 frames)
    i0 <- 1
    for(i in 1:(length(trial_transperancy)-1)){
      if(trial_transperancy[i]!=trial_transperancy[i+1]){
        if((i-i0 < 30)){
          if(i0!=1){
            trial_transperancy[i0:i] <- rep(trial_transperancy[(i0-1)],(i-i0+1))
          }
        }
        i0=i+1
      }
    }
    timeframe <- 1:length(trial_transperancy)

    trial      <- exp[t,]
    trial_key  <- extract_trialkey(exp_key,trial)
    if(dim(trial_key)[1] > 1){ # if any key event was recorded
      trial_key     <- remove_irrelevant_keyevents(trial_key,percept_keys)
      down_time <- round(trial_key$timeDown - trial$trialStartTime, digits = 2)
      up_time   <- round(trial_key$timeUp - trial$trialStartTime, digits = 2)
      key_events <- c()
      key_onsets <- c()
      for(i in 1:length(trial_key$timeDown)){
        key_events <- c(key_events, trial_key$idDown[i], 0)
        key_onsets <- c(key_onsets, down_time[i], up_time[i])
      }
      key_events[key_events==114] <- 1 # left
      key_events[key_events==115] <- -1 # right
      # scale the key events onto timeframe
      key_onsets <- key_onsets * 60 # in frames
      i0 <- 1
      keys <- rep(0, length(timeframe))
      for(i in 1:length(key_onsets)){
        keys[i0:key_onsets[i]] <- key_events[i]
        i0 <- key_onsets[i]+1
      }
      i0 <- 1
      resp_array <- c()
      for(i in 1:(length(timeframe)-1)){
        if(trial_transperancy[i]!=trial_transperancy[i+1]){
          event <- trial_transperancy[i]
          resp <- keys[i0:i]
          if(sum(resp==event) > 30){
            resp_array <- c(resp_array, 1)
          } else{
            resp_array <- c(resp_array, 0)
          }
        }
      }
    }
  scores <- c(scores, mean(resp_array))
  labs_data <- c(labs_data, paste0("trial",t))
  }
  scores <- c(scores, mean(scores))
  labs_data <- c(labs_data, "all")
  output <- mean(scores)
  return(output)
}
