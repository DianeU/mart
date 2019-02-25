# Calculate the number of patients per treatment group after each allocation
#
# @param R randomization sequence as numeric vector
# @param K number of treatment groups
# @value matrix with treatment groups in columns and patient index in rows.
getSum <- function(R, K){
	sapply(1:K, function(j) cumsum(R == j))	
}

# Calculate the imbalance after each allocation
#
# @param R randomization sequence as numeric vector
# @param K number of treatment groups
# @value matrix that contains the imbalance vector after each allocation
getImbal <- function(R, K){
	sumR <- getSum(R, K)
	#matrix(0, nrow= N, ncol= K)- 0:(length(R)-1)
	K*sumR - 1:length(R)
}

# Check whether sequence is valid BSD sequence
#
# @param R randomization sequence as numeric vector
# @param mti maximum tolerated imbalance, scalar number
# @param K number of treatment groups
checkBSDSeq <- function(R, b, K){
	imb <- getImbal(R, K)
	all(abs(range(imb)) <= b )
}

