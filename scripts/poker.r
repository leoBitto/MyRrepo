# cerca un poker generico
poker = function(nrepl=1e4){

    mazzo = rep(1:13, 4)
    play = function(i){
        win=0
        # mescolo mazzo
        mazzo = sample(mazzo)
        # estraggo 5 carte
        estr = mazzo[1:5]
        # creo tabella estrazione frequenze
        t = table(estr)
        #controllo che abbia gia un poker in mano, molto improbabile
        # ovvero se c'è una frequenza uguale a 4
        if(sum(t == 4)>0){
            win=1
        }else{ # se non abbiamo un poker dobbiamo cambiare
            hold = max( as.numeric(names(t[t==max(t)])))
            estr = c(estr[estr==hold], mazzo[6:( 5+sum( estr!=hold ) )])
            # ricontrolla se c'è un poker
            t = table(estr)
            if(sum(t == 4)>0){
                win=1
            }
        }

        return(win)
    }   
    w = mapply(play, 1:nrepl)
    print(paste("valore atteso: ", mean(w)) )
}