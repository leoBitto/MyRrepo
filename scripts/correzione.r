poi = function(lambda, k, nrepl=1e4){
    sd = sqrt(lambda)

    s = function(i){
        sam = rpois(k, lambda)
        stima = mean( abs(sam- mean(sam)))
        return(stima)
    }
    stime = mapply(s, 1:nrepl)
    print(paste("distorsione: ", abs(mean(stime) - sd)))
}