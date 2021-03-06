## Creates S3 object representing a storm:
## Args:
## - arrival time of the storm
## - duration should specify the duration of the storm 
##   if Bartlett Lewis process or the number of
##   rain cells in the storm (excluding the one at 
##   onset, if present), if Neymann-Scott process
## - beta, rate of rain cell arrival if Bartlett Lewis
##   process or 1/mean arrival time of rain cells after
##   storm onset if Neymann-Scott process
## - mu_x, mean rain cell depth
## - eta, 1/mean rain cell duration
## - a boolean indicating whether the first rain cell
##   occurs at onset of the storm (the default)
create_storm = function(arrival, duration, beta, mu_x, 
                        eta, bartlett_lewis=TRUE, 
                        cell_at_start=TRUE) {
  if(bartlett_lewis) {
    raincells = simulate_raincells(duration, beta, mu_x, 
                                 eta, cell_at_start)
  } else {
    num_cells = duration
    raincells = simulate_raincells_neymann_scott(num_cells, 
                                 beta, mu_x, eta,
                                 cell_at_start)
  }
  storm = list(arrival = arrival, raincells = raincells)
  class(storm) = "storm"
  return(storm)
}

## Simulates rain cells in a Bartlett Lewis process
## Args: see args list for create_storm
simulate_raincells = function(duration, beta, mu_x, eta, 
                              cell_at_start=TRUE) {
  # Here we assume there is a rain cell immediately at 
  # the start of each storm
  if(cell_at_start) arrival = c(0, sim_poisson_arrivals
                                (beta, duration))
  # Here we don't
  else arrival = sim_poisson_arrivals(beta, duration)
  d = sim_depth_duration(length(arrival), mu_x, eta)
  return(cbind(arrival, d))
}

## Simulates rain cells in a Neymann-Scott process
## Args: see args list for create_storm
simulate_raincells_neymann_scott = function(num_cells, beta, mu_x,
                              eta, cell_at_start=TRUE) {
  # Here we assume there is a rain cell immediately at 
  # the start of each storm
  if(cell_at_start) arrival = c(0, sort(rexp(num_cells, 
                                beta))) 
  # Here we don't
  else arrival = sort(rexp(num_cells, beta))
  d = sim_depth_duration(length(arrival), mu_x, eta)
  return(cbind(arrival, d))
}

## Simulates a Poisson process
## Args:
## - rate of Poisson process
## - L, number of time units over which to simulate
sim_poisson_arrivals = function(rate, L) {
  # Expected number of events in [0,L] is L*rate.
  # Multiply by 2 to make it very likely that the arrivals
  # span the whole interval.
  N = 2*ceiling(L*rate)
  # Simulate interarrival times
  interarrival = rexp(N,rate)
  # Check to make sure the arrivals span the interval. 
  # If not, simulate more interarrival times.
  while(sum(interarrival)<L) {
    interarrival = c(interarrival, rexp(N/2,rate))
  }
  # Compute arrival times
  arrival = cumsum(interarrival)
  # Throw away arrivals after time L
  arrival = arrival[arrival<L]
  return(arrival)
}


## Simulates the depth and duration of rain cells
## Args:
## - n, number of rain cells
## - mu_x and eta: see args list for create_storm
sim_depth_duration = function(n, mu_x, eta) {
  return(data.frame(depth = rexp(n, 1/mu_x), 
                    duration = rexp(n, eta)))
}

## Generic method for storm objects: returns a data 
## frame containing the (absolute) times when the 
## rainfall depth changes during the storm and the 
## size of the change
summary.storm = function(object, ...) {
  arrival = object$raincells$arrival + object$arrival
  depart = arrival + object$raincells$duration
  d = data.frame(time = c(arrival, depart), 
                 change = c(object$raincells$depth, 
                            -object$raincells$depth))
  return(d)
}

## Simulates rainfall process, returning a list
## containing a list of storms and the duration
## of simulation. Note that for clustered processes
## it is assumed that there is a rain cell at onset
## of each storm.
## Args:
## - lambda, rate of storm arrival
## - gamma should specify 1/mean storm duration if
##   Bartlett Lewis process or mu_c, the mean number  
##   of rain cells per storm (excluding the one at 
##   onset), if Neymann-Scott process
## - beta, mu_x, eta: see args list for create_storm
## - dur, number of time units over which to simulate
sim_rain = function(lambda, beta=0, gamma=NA, mu_x, eta, 
                    dur, bartlett_lewis=TRUE) {
  if((beta==0) & is.na(gamma)) cluster=FALSE
  else cluster=TRUE
  # Simulate arrival times of storms
  if(cluster) arrival = sim_poisson_arrivals(lambda, dur)
  else arrival = 0
  # Simulate storm durations if appropriate ...
  num_storm = length(arrival)
  if(num_storm==0) return(NULL)
  if(!cluster) duration = dur
  else if(bartlett_lewis) duration = rexp(num_storm, gamma)
  else {
    mu_c = gamma
    num_cells = rgeom(num_storm, 1/(1+mu_c)) # num_cells >= 0
    duration = num_cells
  }
  # Simulate individual storms
  if(cluster) {
    storms = lapply(1:num_storm, function(i) 
      create_storm(arrival[i], duration[i], beta, mu_x, 
                  eta, bartlett_lewis, TRUE))
  } else {
    storms = lapply(1:num_storm, function(i) 
      create_storm(arrival[i], duration[i], lambda, mu_x,
                   eta, TRUE, FALSE))
  }
  return(list(storms = storms, dur = dur))
}

## Aggregates simulated rainfall over regular, 
## non-overlapping intervals
## Args:
## - interval, size of sub-intervals over which to 
##   aggregate
## - list object returned by sim_rain function
## Returns a list of function closures providing
## access to aggregated data and summaries thereof
compute_summaries = function(interval, storms) {
  # Input a list (storms), apply a function (summary.storm) to 
  # each component and return the output in a data frame (d)
  d = plyr::ldply(storms$storms, summary.storm) 
  # Insert dummy "events" at start of each interval
  seq = seq(0, storms$dur, interval)
  d = rbind(d, cbind(time=seq[-length(seq)], change=0)) 
  # Sort changes in depth chronologically
  d = d[order(d$time),]
  # Calculate time series of total depths
  d$total_depth = cumsum(d$change)
  # Calculate aggregated (e.g. daily) rainfall totals
  d$day = as.numeric(cut(d$time, seq, right=FALSE))
  total = as.numeric(by(d[,c("time", "total_depth")], d$day, 
                        area_step_graph, interval=interval)) # Here we
  # split the data frame d[,c("time", "total_depth")] by  
  # row into data frames subsetted by d$day then apply 
  # area_step_graph to each subset in turn
  prop_dry = function() {
    return(1-mean(total>0))
  }
  mean_rain = function() {
    return(mean(total))
  }
  var_rain = function() {
    return(var(total))
  }
  cov_rain = function() {
    return(cov(total[-length(total)], total[-1]))
  }
  max_rain = function() {
    return(max(total))
  }
  get_total = function() {
    return(total)
  }
  # Return list of function closures
  return(list(get_total=get_total, prop_dry=prop_dry, 
              mean_rain=mean_rain, var_rain=var_rain, 
              cov_rain=cov_rain, max_rain=max_rain))
}

# Calculates the area under the step graph whose left-most
# coordinate is (x=x[1,1], y=x[1,2]) and whose y-coordinates 
# changes to values c(x[-1,2], 0) at x-coordinates 
# c(x[-1,1], x[1,1]+interval) 
area_step_graph = function(x, interval) {
  y = x[,2]
  x = diff(c(x[,1], x[1,1]+interval))
  return(sum(x*y))
}

## Aggregates simulated rainfall over regular, 
## non-overlapping intervals
## Args:
## - interval, size of sub-intervals over which to 
##   aggregate
## - list object returned by sim_rain function
## Returns aggregated data
compute_totals = function(interval, storms) {
  # Input a list (storms), apply a function (summary.storm) to 
  # each component and return the output in a data frame (d)
  d = plyr::ldply(storms$storms, summary.storm) 
  # Insert dummy "events" at start of each interval
  seq = seq(0, storms$dur, interval)
  d = rbind(d, cbind(time=seq[-length(seq)], change=0)) 
  # Sort changes in depth chronologically
  d = d[order(d$time),]
  # Calculate time series of total depths
  d$total_depth = cumsum(d$change)
  # Calculate aggregated (e.g. daily) rainfall totals
  d$day = as.numeric(cut(d$time, seq, right=FALSE))
  total = as.numeric(by(d[,c("time", "total_depth")], d$day, 
                        area_step_graph, interval=interval)) # Here we
  # split the data frame d[,c("time", "total_depth")] by  
  # row into data frames subsetted by d$day then apply 
  # area_step_graph to each subset in turn

  # Return aggregated rainfall
  return(total)
}

#' Simulate a rainfall time series from a simplified model
#' 
#' @param lambda Rate of storm arrival.
#' @param mu_x Mean rain cell depth
#' @param eta 1/mean rain cell duration
#' @param dur Number of time units over which to simulate
#' @param interval How many time units to aggregate into a single
#' observation
#' @param bartlett_lewis If \code{TRUE} (the default) simulate from
#' a Bartlett-Lewis process. Otherwise simulate from a Neymann-Scott
#' process 
#'
#' @export
sim_simple_data = function(lambda, mu_x, eta, dur, interval, bartlett_lewis=TRUE) {
  my_data = sim_rain(lambda=lambda, mu_x=mu_x, eta=eta, dur=dur, bartlett_lewis=bartlett_lewis)
  my_totals = compute_totals(interval, my_data)
  return(my_totals)
}
