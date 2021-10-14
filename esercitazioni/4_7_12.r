# ES1
# prob tra 0 e 2 di una chi quadro dai 2 ai 30 gdl

prob02 = function(nrepl=1e4){
    p = function(i){
        v = rchisq(nrepl, i)
        v = v[v<2]
        return(length(v)/nrepl)
    }
    e = mapply(p, 2:30)
    names(e)=2:30
    
    return(e)
}
# notiamo che la prob si abbassa
convergenza_chisq = function(dfmax = 10){
    a = F
    for(gl in 2:dfmax){
        if(gl > 1) a = T
        curve(dchisq(x, gl), from=0, to=2, ylim=c(0, dnorm(0)), add=a, lwd=1.2, col=gl, lty=gl)
    }
    title("densita di chisq al crescere dei gl")
    
}


# ES2

data = data.frame(cod.fisc = "aaa", 
                regione=sample(1:20, 3000, rep=T), 
                componenti = sample(1:5, 3000, rep=T),
                tipo.app = sample(1:4, 3000, rep=T),
                durata = sample(5:21, 3000, rep=T),
                affitto = sample(1500:5000, 3000, rep=T))
media.sogg = function(regione, data){
    # limita il dataframe alle sole regioni interessate
    data = data[data$regione==regione,]
    # fai split del dataframe in base alla regione e al tipo di app
    s = split(data$durata, data$tipo.app)
    # ritorna le medie con simplified apply
    return(sapply(s, mean))
}





solitario.f = function(nrepl=1e4){
    mazzo = rep(1:10, 4)

    
    play = function(i){
        mazzo = sample(mazzo)
        # lancia dado 40 volte
        dado = sample(1:6, 40, rep=T)
        # confronta vettore mazzo e vettore dado
        chk = sum(mazzo==dado)
        if(chk<3) win=0
        else if(chk >= 3 & chk <= 6) win = (chk-2)*5 
        else if(chk >= 7) win = 100
        return(win)
    }
    e=sapply(1:nrepl, play)
    
    return(mean(e))
}