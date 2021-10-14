probchi = function(nsam = 1e4){
    res = NULL
    for(gdl in 2:30){
        x = rchisq(nsam, df=gdl)
        res = c(res, mean(x > 0 & x < 2))
    }
    return(res)
}