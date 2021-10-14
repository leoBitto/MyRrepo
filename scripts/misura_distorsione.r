simulaNorm = function(nobs, media, sd, prv=1e4){
    medie = NULL
    
    for(i in 1:prv){
        sam = rnorm(nobs, media, sd)
        medie = c(medie, mean(sam))
    }
    val.att = mean(medie)
    var.medio = var(medie)
    print(paste("distorsione :", abs(media-val.att)))
    plot(1:prv, medie, col="red", type="l")
    return(c(val.att, var.medio))
}

simulaPois= function(n, lambda, prv=1e4){
    medie = NULL
    
    for(i in 1:prv){
        sam = rpois(n, lambda)
        medie = c(medie, mean(sam))
    }
    val.att = mean(medie)
    var.medio = var(medie)
    print(paste("distorsione :", abs(lambda-val.att)))
    return(c(val.att, var.medio))
}

simulaBin = function(n, p, prv=1e4){
    t0 = Sys.time()
    medie = NULL
    
    for(i in 1:prv){
        sam = rbinom(n, 1, p)
        medie = c(medie, mean(sam))
    }
    val.att = mean(medie)
    var.medio = var(medie)

    print(paste("tempo impiegato con for: ", Sys.time()-t0))
    print(paste("distorsione :", abs(p-val.att)))
    return(c(val.att, var.medio))
}

# si toglie for con mapply

simulaBin2 = function(n, p, prv=1e4){
    t0 = Sys.time()
    # singola prova con binomiale
    # il parametro i serve a indicizzare 
    # i valori di ritorno di singolo.campione
    singolo.campione = function(i){
        sample = rbinom(n,1,p)
        return(mean(sample))
    }
    # con mapply richiedi prv campioni e fanne la media
    medie = mapply(singolo.campione, 1:prv)
    #calcola valore atteso
    val.att = mean(medie)
    #calcola varianza non neccessaria al fine di valutare 
    #distorsione stimatore
    var.medio = var(medie)

    print(paste("tempo impiegato con mapply: ", Sys.time()-t0))
    print(paste("distorsione :", abs(p-val.att)))
    return(c(val.att, var.medio))
}