viterbi <- function(hmm) {
    # returns the most likely state sequence
    nt <- sum(hmm@ntimes)
    lt <- length(hmm@ntimes)
		et <- cumsum(hmm@ntimes)
		bt <- c(1,et[-lt]+1)
		
    ns <- hmm@nstates
    
    delta <- psi <- matrix(nrow=nt,ncol=ns)
    state <- vector(length=nt)
    
    prior <- exp(logDens(hmm@initModel))
    
    A <- hmm@trans
    B <- apply(hmm@logdens,c(1,3),sum)
    
    for(case in 1:lt) {
    # initialization
      delta[bt[case],] <- - (log(prior[case,]) + B[bt[case],])
      psi[bt[case],] <- 0
      # recursion
      for(i in ((bt[case]+1):et[case])) {
          for(j in 1:ns) {
              delta[i,j] <- min(delta[i-1,] - log(A[i,,j])) - B[i,j]
              k <- which.min(delta[i-1,] - log(A[i,,j]))
              if(length(k) == 0) k <- 0
              psi[i,j] <- k
          }
      }
      #trace maximum likely state
      state[et[case]] <- which.min(delta[et[case],])
      for(i in (et[case]-1):bt[case]) {
          state[i] <- psi[i+1,state[i+1]]
      }
    }
    return(state)
}

viterbi.fb <- function(A,B,prior) {
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
    #trace maximum likely state
    state[nt] <- which.min(delta[nt,])
    for(i in (nt-1):1) {
        state[i] <- psi[i+1,state[i+1]]
    }
    return(state)
}