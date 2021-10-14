# mu per sigma noto
int.conf1 = function(nobs, mu, sigma2, alpha=0.05, nrepl=1e4){
    
    samp = function(i){
        sam = rnorm(nobs, mean=mu, sd=sqrt(sigma2))
        stima = mean(sam)
        me = qnorm(1-alpha/2)*sqrt(sigma2/nobs)
        return(c(stima-me, stima+me))
    }

    universo = mapply(samp, 1:nrepl)
    universo=t(universo)
    copertura = universo[,1]<mu & mu<universo[,2]
    
    print(sum(copertura)/nrepl)
}

###mu per sigma noto CON FOR
int.conf1 = function(nobs, mu, sigma2, alpha=0.05, nrepl=1e4){
    
    universo = NULL
    for (i in 1:nrepl){
        sam = rnorm(nobs, mean=mu, sd=sqrt(sigma2))
        stima = mean(sam)
        me = qnorm(1-alpha/2)*sqrt(sigma2/nobs)
        universo = rbind(universo, c(stima-me, stima+me))
        print(universo)
    }

    copertura = universo[,1]<mu & mu<universo[,2]
    
    print(sum(copertura)/nrepl)
}

# mu con sigma non noto
int.conf2 = function(nobs, mu, sigma2, alpha=0.05, nrepl=1e4){
    
    samp = function(i){
        sam = rnorm(nobs, mean=mu, sd=sqrt(sigma2))
        stima = mean(sam)
        me = qt(1-alpha/2, df=nobs-1)*sd(sam)/sqrt(nobs)
        return( c(stima-me, stima+me) )
    }

    universo = mapply(samp, 1:nrepl)
    universo=t(universo)
    copertura = universo[,1]<mu & mu<universo[,2]
    
    print(sum(copertura)/nrepl)
}
#intervallo di p
int.conf3 = function(nobs, p, alpha=0.05, nrepl=1e4){

    samp = function(i){
        sam = rbinom(nobs, 1, p)
        phat = mean(sam)
        me = qnorm(1-alpha/2)*sqrt((phat*(1-phat)/nobs))
        return(c(phat-me, phat+me))
    }

    universo = mapply(samp, 1:nrepl)
    universo=t(universo)
    copertura = universo[,1]<p & p<universo[,2]
    
    print(sum(copertura)/nrepl)
}