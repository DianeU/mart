
Robs <- R
K <- 3
mti <- 2
cond <- 2


condBsdRand <- function(Robs, K, mti, cond){
	stopifnot(round(mti) == mti, mti > 0, cond %in% 1:K)
	N <- length(Robs)
	R <- numeric(N)
	i <- 0
	sumR <- rep(0, K) # Number of subjects per treatment group
	imb <- rep(0, K) # Distance from the target allocation: imb = abs(K* sumR - i)
	free <- setdiff(1:K, cond)
	if(length(free) <= 1){
		warning("There are not enough groups for sampling. Returning Robs")
		return(Robs)
	}
	
	while(i < N) {
		if (Robs[i+1] %in% cond){
			R[i+1] <- Robs[i+1]
		}
		else {
			atMti <- list(which(imb <= -mti), which(imb >= mti))
			sumAtMti <- sapply(atMti, length)
			# case analysis
			# No group at MTI - sample among all groups
			if (sum(sumAtMti) == 0) {
				R[i + 1] <- sample(free, 1)
				# If one group is at the imbalance, we have to choose it next
			} else if (sumAtMti[1] > 0){
				stopifnot(sumAtMti[1] == 1)
				R[i + 1] <- atMti[[1]]
			}
			# Some groups are MTI - sample among those that are "free"
			else if ( sumAtMti[2] > 0 ) {
				notAtMti <- setdiff(free, atMti[[2]])
				if(length(notAtMti) == 1){
					R[i + 1] <- notAtMti
				}
				else{
					R[i + 1] <- sample(notAtMti, 1)
				}
				
			}  else {
				stop("There must be an error somewhere")
			}
		}
		i <- i + 1
		# increment the number of participants in the group that was selected
		sumR[R[i]] <- sumR[R[i]] + 1
		imb <- K*sumR - i
		# Printing for debugging
		#print(i); print(sumR); #print (imb)
	}
	R
}
