dist = function(nrepl=1e4, gdl = 3, k=1000){
    if(gdl<3) stop("i gradi di libertà devono essere maggiori di due")
    var.pop = gdl/(gdl-2)
    samples = function(i){
        sam = rt(k, gdl)
        # calcola varianza non corretta
        media.camp = mean(sam)
        diff = sam - media.camp
        diff2 = diff*diff
        s = sum(diff2)
        var.camp.Ncorr = s/(k-1)
        return(var.camp.Ncorr)
    }
    vars = mapply(samples, 1:nrepl)
    print(paste("distorsione: ", mean(vars) - var.pop))
}