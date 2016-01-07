#' Simulate data to be used in ABC
#'
#' Parameters are drawn from the prior distribution, fed in to the model to produce data, which is processed to produce summary statistics. This process is iterated.
#'
#' @param nsims How many simulations to perform.
#' @param prior_function A function with no arguments returing a vector of parameters drawn from the prior. Ideally this vector should be named.
#' @param sim_function A function taking a vector of parameters as an argument and returning a model data object.
#' @param sumstat_functions A list of functions. Each should take a model data object as argument and return a summary statistic (or a vector, see later), which ideally is named. Sometimes it is convenient to use the same calculations for several summaries. In this case a function can return a vector of summaries. Again these should ideally be named.
#' @param nCores Number of parallel cores to use.
#'
#' @return A matrix whose rows are separate simulations. Each row contains the simulations parameters followed by the summaries.
#'
#' @import parallel
#' @importFrom magrittr "%>%"
simABC = function(nsims, prior_function, sim_function, sumstat_functions, nCores=detectCores()) {    
    cl = makeCluster(nCores)
    clusterCall(cl, function() {require(magrittr)})
    clusterExport(cl=cl, varlist=c("prior_function", "sim_function", "sumstat_functions"), envir=environment())    
    do1sim = function(i) {
        theta = do.call(prior_function, list())
        d = do.call(sim_function, list(theta))
        s = lapply(sumstat_functions, function(f) do.call(f, list(d))) %>% unlist
        c(theta, s)
    }
    sims = parSapply(cl, 1:nsims, do1sim) %>% t
    stopCluster(cl)
    sims
}

#' Perform an ABC analysis given one simulated summary
#'
#' Perform an ABC analysis given simulations of a single summary statistic
#'
#' @param stats A vector of simulated summary statistics.
#' @param obs The observed summary statistic.
#' @param dist What distance function to use. Currently choose either "weightedEuclidean" or "rank".
#' @param nacc How many simulations to accept.
#'
#' @return A vector specifying which elements of \code{stats} were accepted.
rejABC_1stat = function(stats, obs, dist="weightedEuclidean", nacc=200) {
    S = stats
    s0 = obs
    if (dist == "rank") {
        diffVec = S - s0
        dist = rank(abs(diffVec)) ##nb could consider a different choice of "ties" argument for "rank"
    } else {
        dist = abs(S - s0)
    }
    order(dist)[1:nacc]
}


#' Perform an ABC analysis given simulations
#'
#' Perform an ABC analysis given simulations
#'
#' @param stats A matrix of simulated summary statistics.
#' @param obs A vector of observed summary statistics.
#' @param subset A numeric vector specifying which summaries should be used.
#' @param dist What distance function to use. Currently choose either "weightedEuclidean" or "rank".
#' @param nacc How many simulations to accept.
#' @param nCores Number of parallel cores to use.
#'
#' @return A vector specifying which rows of \code{stats} were accepted.
#'
#' @import parallel
#' @importFrom magrittr "%>%"
rejABC = function(stats, obs, subset=1:ncol(stats), dist="weightedEuclidean", nacc=200, nCores=detectCores()) {    
    if (length(subset)==1) {
        ##Do case of a single summary statistic separately (otherwise dealing with dropped dimensions is tedious)
        return(rejABC_1stat(stats[,subset], obs[subset], dist=dist, nacc=nacc))
    }
    cl = makeCluster(nCores)
    clusterCall(cl, function() {require(magrittr)})
    S = stats[,subset,drop=FALSE]
    s0 = obs[subset]
    if (dist == "rank") {
        diffMat = parApply(cl, S, 1, function(x) x-s0) %>% t
        rankDiffMat = parApply(cl, diffMat, 2, function(x) rank(abs(x))) ##nb could consider a different choice of "ties" argument for "rank"
        dist = parApply(cl, rankDiffMat, 1, function(x) x^2 %>% sum %>% sqrt)
    } else {
        mean_stats = parApply(cl, S, 2, mean)
        sd_stats = parApply(cl, S, 2, sd)
        ss_norm = parApply(cl, S, 1, function(x) (x-mean_stats)/sd_stats) %>% t
        obs_norm = (s0-mean_stats) / sd_stats
        dist = parApply(cl, ss_norm, 1, function(x) (x-obs_norm)^2 %>% sum %>% sqrt)                
    }
    stopCluster(cl)
    order(dist)[1:nacc]
}

#' Perform an ABC analysis
#'
#' Perform an ABC analysis, including simulation of data
#'
#' @param nsims How many simulations to perform.
#' @param prior_function A function with no arguments returing a vector of parameters drawn from the prior. Ideally this vector should be named.
#' @param sim_function A function taking a vector of parameters as an argument and returning a model data object.
#' @param sumstat_functions A list of functions. Each should take a model data object as argument and return a summary statistic (or a vector, see later), which ideally is named. Sometimes it is convenient to use the same calculations for several summaries. In this case a function can return a vector of summaries. Again these should ideally be named.
#' @param obs_data Observed model data object.
#' @param dist What distance function to use. Currently choose either "weightedEuclidean" or "rank".
#' @param nacc How many simulations to accept.
#' @param output Specify what output to return. Currently choose either "accepted" or "all".
#' @param nCores Number of parallel cores to use.
#'
#' @return A data frame containing parameters and summaries. If \code{output=="accepted"} then only the accepted simulations are returned. Otherwise all simulations are returns and an \code{output=="accepted"} column shows if they were accepted.
#'
#' @examples
#'
#' library(ismev)
#' data(rain)
#' doSim = function(pars) {
#'     rainfallABC::sim_simple_data(pars[1], pars[2], pars[3], 17531, 1) ##TO DO: un-hardcode dur=17531
#' }
#' rprior = function() {
#'     c(lambda = runif(1, 0, 2),
#'       mu_x = runif(1, 0, 50),
#'       eta = runif(1, 0, 10))
#' }
#' abcout = ABC(200, rprior, doSim, list(mean_overall, prop_dry, cor_overall, max_rain), rain, nacc=50)
#' par(mfrow=c(1,3))
#' for (i in 1:3) {
#'     pname = names(abcout)[i]
#'     hist(abcout[,i], main=pname, xlab=pname, freq=FALSE)
#' }
#' 
#' @export
#' @importFrom magrittr "%>%"
ABC = function(nsims, prior_function, sim_function, sumstat_functions, obs_data, dist="weightedEuclidean", nacc=200, output="accepted", nCores=parallel::detectCores()) {
    obs_stats= lapply(sumstat_functions, function(f) do.call(f, list(obs_data))) %>% unlist
    sims = simABC(nsims, prior_function, sim_function, sumstat_functions, nCores)
    npars = ncol(sims) - length(obs_stats)
    stats = sims[, -(1:npars), drop=FALSE]
    which_acc = rejABC(stats, obs_stats, dist=dist, nacc=nacc, nCores=nCores)
    
    if (output=="accepted") {
        out = data.frame(sims[which_acc,,drop=FALSE])
    } else {
        acc = rep(FALSE, nsims)
        acc[which_acc] = TRUE            
        out = data.frame(sims, accepted = acc)
    }
    return(out)
}
