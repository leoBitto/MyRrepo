bern = function(p, nsam = 50, epsilon = 1e-4){
    
    phat = mean(rbinom(nsam,1,p))
    n = 1
    while(abs(p-mean(phat)) > epsilon){
        phat = c(phat, mean(rbinom(nsam,1,p)))
        n = n+1
    }
    
    plot(1:length(phat), cumsum(phat)/(1:n), type="l")
    print(paste("ripetizioni: ", n))
    
}