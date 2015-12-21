##Test combined operation of different parts of the package
##(nb the test is simply that the code executes without error)

library(ismev)
data(rain)
doSim = function(pars) {
    rainfallABC::simulation_Bartlett_Lewis(pars[1], pars[2], pars[3], 17531, 1) ##TO DO: dehardcode dur=17531
}
rprior = function() {
    c(lambda = runif(1, 0, 2),
      mu_x = runif(1, 0, 50),
      eta = runif(1, 0, 10))
}
abcout = ABC(200, rprior, doSim, list(mean_overall, prop_dry, cor_overall, max_rain), rain)
