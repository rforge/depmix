# 
# Maarten Speekenbrink, 23-3-2008
# 

viterbi.fb <-
function(A,B,prior) {
    # returns the most likely state sequence
    nt <- nrow(B)
    ns <- ncol(A)
    delta <- psi <- matrix(nrow=nt,ncol=ns)
    state <- vector(length=nt)
    # initialization
    delta[1,] <- - (log(prior) + log(B[1,]))
    psi[1,] <- 0
    # recursion
    for(i in 2:nt) {
        for(j in 1:ns) {
            delta[i,j] <- min(delta[i-1,] - log(A[,j])) - log(B[i,j])
            k <- which.min(delta[i-1,] - log(A[,j]))
            if(length(k) == 0) k <- 0
            psi[i,j] <- k
        }
    }
    # trace maximum likely state
    state[nt] <- which.min(delta[nt,])
    for(i in (nt-1):1) {
        state[i] <- psi[i+1,state[i+1]]
    }
    return(state)
}

