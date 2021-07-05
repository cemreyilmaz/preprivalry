
<!-- README.md is generated from README.Rmd. Please edit that file -->

# preprivalry

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/cemreyilmaz/preprivalry.svg?branch=master)](https://travis-ci.com/cemreyilmaz/preprivalry)
<!-- badges: end -->

The goal of preprivalry is the preprocessing of binocular rivalry data
which are collected via PsychToolbox in MATLAB environment in the
project by NeuroVision research group in the University of Graz. A
rivalry data-set can be preprocessed by using this package if it is
organized as required.

## Installation

You can install the released version of preprivalry from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("preprivalry")
```

## Data structure

Since our data were collected on MATLAB, this package includes some
functions to reorganize the data.

#### MATLAB data

Our .mat file includes a MATLAB structure named as log. Besides the
other variable, its forth component is a struct containing the key
events and fifth component is a struct containing the experiment info,
as the following:

    raw_data
      --raw_data.key
        --raw_data.key.perceptKeys  # key codes of instructed keys  [key_1, key_2]
        --raw_data.key.perceptNames # key names of instructed keys  {key_1, key_2}
        --raw_data.key.idDown       # key codes of pressed keys     [nx1 double]
        --raw_data.key.timeDown     # time points for pressed keys  [nx1 double]
        --raw_data.key.idUp         # key codes of released keys    [nx1 double]
        --raw_data.key.timeUp       # time points for released keys [nx1 double]
      --raw_data.exp
        --raw_data.exp.trialStartTime # [trial_1, trial_2]
        --raw_data.exp.trialEndTime   # [trial_1, trial_2]

We saved separate .mat files for each experimental run by naming the
file as: ExperimentName\_s\#\#\#\_session\#\_run\#\_date.mat

## Example

The subject data can be preprocessed by using only one function:

``` r
library(preprivalry)
## basic example for preprocessing of subject data
directory <- paste(getwd(), '/tests', sep='')
exp_list <- c('RivalryGratings','RivalryImages')
subj <- 's001'
ses <- 1
data <- preprocessing_subject(directory,exp_list,subj,ses)
```

The output can be saved also as an excel file after organizing as a
table:

``` r
library(preprivalry)
## basic example for saving as csv
directory <- paste(getwd(), '/tests',sep='')
output_file <- paste(directory, '/data.csv', sep='')
exp_list <- c('RivalryGratings','RivalryImages')
subj <- 's001'
ses <- 1
raw_data <- read_rivdata(directory,exp_list[1],subj,'session1')
percept_keys <- rbind(raw_data[["log"]][[4]][[1]],
                      c(raw_data[["log"]][[4]][[2]][[1]][[1]],raw_data[["log"]][[4]][[2]][[2]][[1]]))
data <- preprocessing_subject(directory,exp_list,subj,ses)
data <- reorganize_as_table(data)
```

Check the function file preprivalry\_preprocessing\_data.R to see more
details and to customize the work flow.

## Visualization

After preprocessing, the data can be visualized to check the key events,
percept duration, and eye dominance. There is a set of visualization
functions in the package. You can use those functions to visualize the
data in a specified trial, run, and/or session.

### Example

The input for the visualization functions is the same: data, session\#,
run\#, tiral\#. You can visualize the data for a run or a trial.
Althought you do not have to, we recommend to specify at least session
and run since the output would be little messy and not useful for
interpretation.

``` r
library(preprivalry)
## basic example for preprocessing of subject data
directory <- paste(getwd(), '/tests', sep='')
exp_list <- c('RivalryGratings','RivalryImages')
subj <- 's001'
ses <- 1
data <- preprocessing_subject(directory,exp_list,subj,ses)
plot_keyevents(data,1,1,1)
```

<img src="man/figures/README-vis-example-1.png" width="100%" />

``` r
plot_perceptdurations(data,1,1,1)
```

<img src="man/figures/README-vis-example-2.png" width="100%" />

``` r
hist_perceptdurations(data,1,1,1)
```

<img src="man/figures/README-vis-example-3.png" width="100%" />

``` r
bar_stats(data,1,1,1)
```

<img src="man/figures/README-vis-example-4.png" width="100%" />
