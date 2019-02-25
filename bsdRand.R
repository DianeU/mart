
bsdRand <- function(N, b, K = 2) {
  stopifnot(round(N) == N, round(b) == b, b > 0)

  R <- numeric(N) # Randomization sequence R = T
  n <- rep(0, K) # Vector: n = (n_1(i), ...,n_K(i)) 
  D <- rep(0, K) # Vector: D = (D_i1, ...,D_iK) 
  i <- 0 # loop index
  
  while(i < N) {
  	# vector: atMti = (groups at -b, groups at b)
  	atMti <- list(which(D <= -b), which(D >= b))
  	# vector: sumAtMti = (number of groups at -b, number of groups at b)
  	sumAtMti <- sapply(atMti, length)
  	
  	# If no group at -b or b:  sample among all groups
  	if (sum(sumAtMti) == 0) {
  		R[i + 1] <- sample(1:K, 1)
  		# Deterministic allocation if one group is at -b
  	} else if (sumAtMti[1] > 0){
  		# at each point, only one group can be at -b!
  		#stopifnot(sumAtMti[1] == 1) 
  	  if (sumAtMti[1] > 1) {
  	    warning(paste("More than one group is at the lower imbalance boundary. Groups =", 
  	                  paste(atMti[[1]], collapse = ", "), ". Stopping early after", i, " allocations."))
  	    break 
  	  }
  	  # Deterministic allocation if one group is at -b
  	  R[i + 1] <- atMti[[1]]
  	}
  	# If some groups are at b: sample among those that are not at b
  	else { # sumAtMti[2] > 0 
  		notAtMti <- setdiff(1:K, atMti[[2]])
  		# Deterministic allocation if there is only one group <b
  		if(length(notAtMti) == 1){ 
  			R[i + 1] <- notAtMti
  		}
  		else{
  			R[i + 1] <- sample(notAtMti, 1)
  		}
  	} 
  	i <- i + 1
  	# increment the number of participants in the group that was selected
  	n[R[i]] <- n[R[i]] + 1
  	D <- K*n - i
  }
  R
}

