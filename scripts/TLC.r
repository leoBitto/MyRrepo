TLC <- function(nsam, p, nrepl=1e4){
    phat = NULL
    for(i in 1:nrepl)
        phat = c(phat, mean(rbinom(nsam, 1, p)))
    #hist(phat, breaks = 20, prob=T , col="light gray")
    
    # con plot
    t = table(phat)
    plot(as.numeric(names(t)), t/nrepl, type="h", col="orange")
    #

    x=seq(0,1,length.out=1000)
    mean.sample = mean(phat)
    sd.sample = sqrt( mean.sample*( 1-mean.sample) /nsam )
    lines(x, dnorm(x, mean = mean.sample, sd = sd.sample), col = "navy", lwd = 2)
}

# la hist ha diversi bug

# ma il plot non riesce a scalare in modo 
# opportuno la normale

TLC <- function(nsam, lambda, nrepl=1e4){
    lambda.c = NULL
    for(i in 1:nrepl)
        lambda.c = c(lambda.c, mean(rpois(nsam, lambda)))
    #hist(phat, breaks = 20, prob=T , col="light gray")
    
    # con plot
    t = table(lambda.c)
    plot(as.numeric(names(t)), t/nrepl, type="h", col="orange")
    #

    x=seq(0,1,length.out=1000)
    mean.sample = mean(phat)
    sd.sample = sqrt( mean.sample*( 1-mean.sample) /nsam )
   # lines(x, dnorm(x, mean = mean.sample, sd = sd.sample), col = "navy", lwd = 2)
   curve(dnorm(x, mean = mean.sample, sd = sd.sample), col = "navy", lwd = 2, add=T)
   return(t)
}