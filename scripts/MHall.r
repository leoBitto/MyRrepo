# dimostrazione che nel gioco del monty hall cambiare conviene
Mhall = function(nrepl=1e4){
    # crea buste
    buste = c(0,0,1)

    game= function(i){

        # mescola buste
        buste = sample(buste)
        #estraggo busta
        estr = buste[1]
        #indice della prima busta vuota sul banco
        # potrbbero esserci due indici, si prende il primo
        # indice può essere 2 oppure 3
        indice = min( (2:3)[buste[2:3]==0] ) 
        # si usa complemento per scegliere la busta 
        cambio = buste[5-indice]
        # ritorna la prob di scelta e cambio
        return(c(estr, cambio))
    }
    res = mapply(game, 1:nrepl)


    medie1=cumsum(res[2,])/(1:nrepl)
    medie2=cumsum(res[1,])/(1:nrepl)

    par(mfrow=c(1,2))

    plot(1:nrepl, medie1, type="l", col="red")
    points(1:nrepl, medie1, pch=19, col="navy")

    plot(1:nrepl, medie2, type="l", col="cyan")
    points(1:nrepl, medie2, pch=19, col="pink")

    return(apply(res,1,mean))

}
