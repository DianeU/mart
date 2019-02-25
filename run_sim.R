setwd("C:/Users/bsc-default/Dropbox/research/2019-01-29_mutivariate-randomization-test")
source("bsdRand.R")
source("getImbal.R")

# Simulation parameters
r <- 10000
L <- 20000
K <- 4
mti <- 6
N <- 100

# one run is executed like this
R <- bsdRand(N, mti = mti, K = K)



# Unconditional reference set
t1 <- Sys.time()
Omega <- t(sapply(1:L, 
								function(x, N, mti, K) bsdRand(N, mti = mti, K = K),
								N = N,
								mti = mti,
								K = K))
t2 <- Sys.time()
(isBSD <- all(apply(Omega, 1, function(x, mti, K) checkBSDSeq(x, mti, K), mti, K)))
t3 <- Sys.time()

t2-t1; t3-t2


Robs <- bsdRand(N, mti = mti, K = K)
condBsdRand(Robs, K, mti, cond = 2)

# Conditional reference set
t1 <- Sys.time()
Omega_c <- t(sapply(1:L, 
									function(x, Robs, mti, K, cond) condBsdRand(Robs, mti = mti, K = K, cond = cond),
									Robs = Robs,
									mti = mti,
									K = K,
									cond = c(1,4)))
t2 <- Sys.time()
(isBSD <- all(apply(Omega, 1, function(x, mti, K) checkBSDSeq(x, mti, K), mti, K)))
t3 <- Sys.time()

t2-t1; t3-t2

(isCond <- all(apply(Omega_c, 1, function(R, Robs, cond) checkCond(R, Robs, cond), Robs, 2)))

R <- Omega_c[1,]
Robs
checkCond(R, Robs, 2)



