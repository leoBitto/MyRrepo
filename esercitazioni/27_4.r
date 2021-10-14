#fix hist
TLC <- function (nsam, p, nrepl=1e4){

    sampling <- function(i){
        return (mean(rbinom(nsam,1,p)))
    }
    phat = mapply(sampling, 1:nrepl)


    # calcola i valori medi di un vettore
    # togli a ogni valore del vettore meno il primo
    # ogni valore del vettore apparte l'ultimo
    #scrivi modalita di phat valori di densità
    br = sort(unique(phat))
    #calcola i valori medi
    d = (br[-1]-br[-length(br)])/2
    d = c(d, d[length(d)])

    m.ph = mean(phat)
    xl = min(m.ph, 1-m.ph)
    hist(phat, breaks=c(0, br+d, 1), col="pink", prob=T, xlim=c(m.ph-xl, m.ph+xl))
}

#controlla parentesi
misuraDist = function(lambda, nsam, nrepl=1e4){
    #cakE2h8MmSlcola media campionaria di poisson
    sampling=function(i){
        camp = rpois(nsam, lambda)

        s1 = mean((camp-mean(camp))^2)
        s2 = mean((camp-median(camp))^2)
        return(c(s1,s2))
    }
    m = t(mapply(sampling, 1:nrepl))

    print(abs(lambda - colMeans(m)))

}