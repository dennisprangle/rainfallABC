##A simple ABC test with 2 parameters and summary statistics

rprior = function() {
    c(mu = rnorm(1), sigma = exp(1))
}

rdata = function(pars) {
    rnorm(100, pars[1], pars[2])
}

sum_functions = list(function(x, ...) c(mean=mean(x)), function(x, ...) c(sd=sd(x)))

xobs = rnorm(100, 2, 2)

##simABC
test_that("simABC run and outputs matrix of correct dimension", {
    sims = rainfallABC:::simABC(1000, rprior, rdata, sum_functions)
    expect_true(is.matrix(sims))
    expect_equal(dim(sims), c(1000, 4))
})

##Create sims instead so it's in the global environment
sims = rainfallABC:::simABC(1000, rprior, rdata, sum_functions)

##rejABC_1stat
test_that("rejABC_1stat runs and outputs vector", {
    abcout1 = rainfallABC:::rejABC_1stat(sims[,"mean"], xobs[1], nacc=100)
    abcout2 = rainfallABC:::rejABC_1stat(sims[,"mean"], xobs[1], dist="rank", nacc=100)
    expect_true(is.vector(abcout1))
    expect_true(length(abcout1) == 100)
    expect_true(is.vector(abcout2))
    expect_true(length(abcout2) == 100)
})

##rejABC
test_that("rejABC runs and outputs vector", {
    abcout3 = rainfallABC:::rejABC(sims[,3:4], xobs[3:4], nacc=100)
    abcout4 = rainfallABC:::rejABC(sims[,3:4], xobs[3:4], dist="rank", nacc=100)
    expect_true(is.vector(abcout3))
    expect_true(length(abcout3) == 100)
    expect_true(is.vector(abcout4))
    expect_true(length(abcout4) == 100)
})

##ABC
abcout5 = ABC(1000, rprior, rdata, sum_functions, xobs, nacc=100, output="accepted")
abcout6 = ABC(1000, rprior, rdata, sum_functions, xobs, nacc=100, output="all")
abcout7 = ABC(1000, rprior, rdata, sum_functions, xobs, dist="rank", nacc=100, output="accepted")
abcout8 = ABC(1000, rprior, rdata, sum_functions, xobs, dist="rank", nacc=100, output="all")

test_that("ABC runs and gives correct output", {
    expect_true(is.data.frame(abcout5))
    expect_true(is.data.frame(abcout6))
    expect_true(is.data.frame(abcout7))
    expect_true(is.data.frame(abcout8))
    expect_equal(dim(abcout5), c(100, 4))
    expect_equal(dim(abcout6), c(1000, 5))
    expect_equal(dim(abcout7), c(100, 4))
    expect_equal(dim(abcout8), c(1000, 5))
})
