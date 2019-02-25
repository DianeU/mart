checkCond <- function(R, Robs, cond) {
	ind <- sapply(cond, function(j, R, Robs){
		indR <- which(R == j)
		indRobs <- which(Robs == j)
		all.equal(indR, indRobs)
	}, R, Robs)
	all(ind)
}
