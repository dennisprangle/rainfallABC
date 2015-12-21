#' @export
mean_overall = function(x) c(mean = mean(x))

#' @export
prop_dry = function(x) c(prop_dry = mean(x==0))

#' @export
var_overall = function(x) c(var = var(x))

#' @export
cov_overall = function(x) c(cov = cov(x[-length(x)], x[-1]))

#' @export
cor_overall = function(x) c(cor = cor(x[-length(x)], x[-1]))

#' @export
max_rain = function(x) c(max = max(x))

#' @export
mean_wet = function(x) {
    wetcount = sum(x>0)
    if (wetcount == 0) {
        mw = 0
    } else {
        mw = sum(x)/wetcount
    }
    c(mean_wet = mw)
}

#' @export
streaks = function(x) {    
    streak_durs = c() ##Durations of streaks (of all wet days)
    streak_starts = c() ##Start days of streaks
    streak_length = 0
    for (i in 1:length(x)) {
        wet = (x[i]>0)
        if (streak_length==0) {
            if (wet) {
                streak_length = 1
                streak_starts = c(streak_starts, i)
            }
        } else {
            if (wet) {
                streak_length = streak_length+1
            } else {
                streak_durs = c(streak_durs, streak_length)
                streak_length = 0
            }
        }         
    }
    if (streak_length > 0) {
        streak_durs = c(streak_durs, streak_length)
    }
    mean_dur = 0
    sd_dur = 0
    if (length(streak_durs) > 0) {
        mean_dur = mean(streak_durs)
        if (length(streak_durs) > 1) {
            sd_dur = sd(streak_durs)
        } 
    }
    if (length(streak_starts)>1) {
        mean_start_gap = mean(diff(streak_starts))
    } else {
        mean_start_gap = length(x) + 1
    }
    c(mean_dur=mean_dur, sd_dur=sd_dur, mean_start_gap=mean_start_gap)
}
