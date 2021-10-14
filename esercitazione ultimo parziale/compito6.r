es3 = function(nrepl=1e4){
    
    game = function(i){
        monete = sum(rbinom(12, 1, 0.5))
        dadi = sum(sample(1:6, 2, rep=T))
        win = 0
        if(monete+dadi == 24) win = 10000
        else if(monete>7 & monete!=12 & dadi==12) win = 1000
        else win = monete - dadi

        return(win)
    }
    VA = mapply(game, 1:nrepl)
    return(mean(VA))
    
}