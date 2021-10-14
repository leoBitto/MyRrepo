# il paradosso di san pietroburgo consiste nello studio 
# di un gioco di azzardo in cui si lancia una moneta
# tante volte finche non esce una certa faccia scelta (T o C)
# raddoppiando la vincita finche non esce tale faccia
# il paradosso consiste nel trovare una quota di partecipazione
# che consiste al valore atteso ma questo è infinito.

# OMEGA     p       vincite
# T         1/2     2  cent/$
# CT        1/4     4  cent/$
# CCT       1/8     8  cent/$
# CCCT      1/16    16 cent/$   
# CCCCT     1/32    32 cent/$
# CCCCCT    1/64    64 cent/$
# ...       ...     ...

# il valore atteso calcolato come la sommatoria tra p*vincite
# ha valore infinito.

PSP = function(nrepl=1000){
    #1 == T
    #0 == C
    lancio = function(i){
        x=rbinom(1,1,0.5)
        while(sum(x)==0){
            x=c(x, rbinom(1,1,0.5))
        }
        return(2^length(x))
    }
    vincite=mapply(lancio, 1:nrepl)

    medie.prog=function(i){
        return(mean(vincite[1:i]))
    }
    medie = mapply(medie.prog, 1:nrepl)

    #soluzione prof nel calcolare le medie
    #medie=cumsum(vincite)/(1:nrepl)

    plot(1:nrepl, medie, type="l", col="red")
    points(1:nrepl, medie, pch=19, col="navy")
}