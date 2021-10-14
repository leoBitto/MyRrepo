chi = function(n,k,x, nrepl=1e4){
    perc.pop = qchisq(x, n)
    sam = function(i){
        campione = rchisq(k, df=n)
        perc= quantile( campione, x)
    }
    p = mapply(sam, 1:nrepl)
    print(paste("distorsione: ", abs(mean(p) - perc.pop)))
    
}

