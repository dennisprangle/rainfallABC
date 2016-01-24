#' Overall mean rainfall
#'
#' @param x Numeric vector of rainfall totals
#'
#' @return Mean rainfall
#' 
#' @export
mean_overall = function(x, ...) c(mean = mean(x))

#' Proportion of dry days
#'
#' @param x Numeric vector of rainfall totals
#' @param cutoff If cutoff>0, dry day defined as day where rain<cutoff, otherwise dry day defined as day where rain=0
#' 
#' @return Proportion of dry days
#' @export
prop_dry = function(x, cutoff=0) {
    if(cutoff==0) return(c(prop_dry = mean(x==0)))
    else return(c(prop_dry = mean(x<cutoff)))
}

#' Overall rainfall variance
#'
#' @param x Numeric vector of rainfall totals
#' 
#' @return Rainfall variance
#' @export
var_overall = function(x, ...) c(var = var(x))

#' Rainfall autocovariance
#'
#' @param x Numeric vector of rainfall totals
#' 
#' @return Lag 1 autocovariance
#' @export
cov_overall = function(x, ...) c(cov = cov(x[-length(x)], x[-1]))

#' Rainfall autocorrelation
#'
#' @param x Numeric vector of rainfall totals
#' 
#' @return Lag 1 autocorrelation
#' @export
cor_overall = function(x, ...) c(cor = cor(x[-length(x)], x[-1]))

#' Maximum rainfall
#'
#' @param x Numeric vector of rainfall totals
#' 
#' @return Maximum rainfall
#' @export
max_rain = function(x, ...) c(max = max(x))

#' Mean rainfall on wet days
#'
#' @param x Numeric vector of rainfall totals
#' @param cutoff If cutoff>0, rain day defined as day where rain>=cutoff, otherwise rain day defined as day where rain>0
#' 
#' @return Mean of rainfall on rain days
#' @export
mean_wet = function(x, cutoff=0) {
    if(cutoff==0) return(c(mean_wet = mean(x[x>0])))
    else return(c(mean_wet = mean(x[x>=cutoff])))
}

#' Variance of rainfall on wet days
#'
#' @param x Numeric vector of rainfall totals
#' @param cutoff If cutoff>0, rain day defined as day where rain>=cutoff, otherwise rain day defined as day where rain>0
#' 
#' @return Variance of rainfall on rain days
#' @export
var_wet = function(x, cutoff=0) {
    if(cutoff==0) return(c(var_wet = var(x[x>0])))
    else return(c(var_wet = var(x[x>=cutoff])))
}

#' Summaries related to streaks of consecutive rainy days
#' 
#' @param x Numeric vector of rainfall totals
#' @param cutoff If cutoff>0, rain day defined as day where rain>=cutoff, otherwise rain day defined as day where rain>0
#'
#' @return A vector of \code{mean_dur}, mean streak duration; \code{sd_dur}, standard deviation of streak durations; \code{mean_start_gap}, mean times between start times.
#' Where not meaningfully defined the default values of these are 0, 0 and \code{length(x) + 1}.
#' @export
streaks = function(x, cutoff=0) {    
    streak_durs = numeric(length(x)) ##Durations of streaks (of all rain days)
    streak_starts = numeric(length(x)) ##Start days of streaks
    streak_length = 0
    num_streak = 0
    for (i in 1:length(x)) {
        if(cutoff==0) wet = (x[i]>0)
        else wet = (x[i]>=cutoff)
        if (streak_length==0) {
            if (wet) {
                num_streak = num_streak+1
                streak_length = 1
                streak_starts[num_streak] = i
            }
        } else {
            if (wet) {
                streak_length = streak_length+1
            } else {
                streak_durs[num_streak] = streak_length
                streak_length = 0
            }
        }         
    }
    if (streak_length > 0) {
        streak_durs[num_streak] = streak_length
    }
    streak_starts = streak_starts[1:num_streak]
    streak_durs = streak_durs[1:num_streak]
    mean_dur = 0
    sd_dur = 0
    if (num_streak > 0) {
        mean_dur = mean(streak_durs)
        if (num_streak > 1) {
            sd_dur = sd(streak_durs)
        } 
    }
    if (num_streak > 1) {
        mean_start_gap = mean(diff(streak_starts))
    } else {
        mean_start_gap = length(x) + 1
    }
    c(mean_dur=mean_dur, sd_dur=sd_dur, mean_start_gap=mean_start_gap)
}
