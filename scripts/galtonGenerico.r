# 1                                                               |
                                1/10 /                                                       \  9/10

# 2                   2/10 /                  \ 8/10                              2/10 /                   \ 8/10          

# 3               3/10 /     \ 7/10    3/10 /     \ 7/10                     3/10 /     \ 7/10       3/10 /     \ 7/10

# 4                     ...

# 5                     ...

# 6                     ...

# 7                     ...

# 8 

# 9                     ...

#10                 a sinistra si va con 1/2
#
# la funzione si aspetta:
#   -il numero di replicazioni
#   -il binario indagato
#   -un vettore lungo quanti sono gli snodi 
#       che indichi le regole di smistamento in 
#       una sola direzione( solo sx o solo dx)
        # in questo caso il vettore indica la prob di andare a sx
stazione = function(nrepl=1e4, binario=3, regole = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.5)) {

    treno = function(i){
        # calcola per ogni snodo una probabilita stabilita
        # tramite una sample con pesi
        # se 0 allora sinistra 
        # se 1 allora destra
        
        entra_stazione=function(j){
            dir = sample(c(-1,1), 1, prob=c(regole[j], 1-regole[j]))
            return(dir)
        }
        # simula la posizione finale del treno
        pos = sum(mapply(entra_stazione, 1:length(regole)))
        return(pos)
    }
    # simula arrivo di nrepl treni in stazione
    treni = mapply(treno, 1:nrepl)

    # visualizzazione esperimento 
    t = table(treni)
    names(t) = 1:length(t)
    print(t[binario])
    plot(t, type="l")
    #ritorna la tabella dei dati
    return(treni)
}

# circa 3 secondi
set.seed(3)
t0= Sys.time()
stazione()
Sys.time()-t0


# correzione prof
###############
stazione = function(nrepl=1e4, binario=3, regole = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.5)) {

    treno = function(i){
        # calcola per ogni snodo una probabilita stabilita
        # tramite una sample con pesi
        # se 0 allora sinistra 
        # se 1 allora destra
    
        # simula la posizione finale del treno
        return(sum(rbinom(10, 1, regole)))
    }
    # simula arrivo di nrepl treni in stazione
    treni = mapply(treno, 1:nrepl)

    # visualizzazione esperimento 
    t = table(treni)
    print(t[binario])
    print(t)
    plot(t, type="l")
    #ritorna la tabella dei dati
    return(treni)
}



# versioni alternative
stazione1 = function(nrepl=1e4){ 
    #per convenzione: 1 = sx 
    pr=c( (1:9)/10, .5)
    binari.arrivo=NULL
    for(i in 1:nrepl){ 
        res=NULL 
        for(pp in pr)
        res=c(res, rbinom(1,1,pp))
        binari.arrivo=c(binari.arrivo, sum(res)+1 )
    }
    t=table(binari.arrivo)/nrepl print(t)
}
        
stazione2 = function(nrepl=1e4){
    #per convenzione: 1 = sx
    pr=c( (1:9)/10, .5)
    binari.arrivo=NULL 
    for(i in 1:nrepl){ 
        res=rbinom(10,1,pr) 
        binari.arrivo=c(binari.arrivo, sum(res)+1) 
    }
    t=table(binari.arrivo)/nrepl print(t)
}
stazione3 = function(nrepl=1e4){
    #per convenzione: 1 = sx 
    pr=c( (1:9)/10, .5) 
    f = function(i) 
        sum(rbinom(10,1,pr))+1
    binari.arrivo=sapply(1:nrepl, f) 
    t=table(binari.arrivo)/nrepl print(t) 
} 

set.seed(3)
t0=Sys.time() 
stazione1(1e5) 
Sys.time()-t0 

set.seed(3) 
t0=Sys.time() 
stazione2(1e5) 
Sys.time()-t0 

set.seed(3) 
t0=Sys.time() 
stazione3(1e5) 
Sys.time()-t0
