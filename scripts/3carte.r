tre.carte = function(nrepl=1000){
    # crea mazzo
    mazzo = c(rep(0,3),rep(1,3))
    names(mazzo) = sort(rep(1:3, 2))

    game = function(i){
        #mescola mazzo 
        mazzo = sample(mazzo)
        #estraggo faccia dunque carta
        estr = mazzo[1]
        #controllo quali carte hanno stesso colore
        return( sum(mazzo[names(mazzo)== names(estr)])!=1 )
    }
    #replica gioco
    results = mapply(game, 1:nrepl)
    # calcola media risultati
    print(mean(results))
    

    medie=cumsum(results)/(1:nrepl)

    plot(1:nrepl, medie, type="l", col="red")
    points(1:nrepl, medie, pch=19, col="navy")
}