library(rainfallABC)
library(ismev)
data(rain)
dur = length(rain)

doSim = function(pars) {
    rainfallABC::simulation_Bartlett_Lewis(pars[1], pars[2], pars[3], 17531, 1) ##TO DO: dehardcode dur=17531
}

rprior = function() {
    c(lambda = runif(1, 0, 2),
      mu_x = runif(1, 0, 50),
      eta = runif(1, 0, 10))
}

abcout = ABC(1000, rprior, doSim, list(mean_overall, prop_dry, cor_overall, max_rain), rain)

par(mfrow=c(1,3))
for (i in 1:3) {
    pname = names(abcout)[i]
    hist(abcout[,i], main=pname, xlab=pname, freq=FALSE)
}

