# es a casa "solitario scarta re"
#   matrice 4*9
#   rimangano fuori 4 carte
#   ogni riga identifica il seme denari coppe bastoni spade
#   si scopre la carta e si mette nella posizione giusta
#   ogni volta che esce un re lo scarto e riparto
#   si vince se l'ultima carta girata è un re
#   probabilità di vincere al solitario?
#
scarta.Re = function(nrepl=1e4){
    mazzo = sort(rep(1:10, 4))
    names(mazzo)= rep(c("D","C","S","B"),10)


    play = function(i){
        win = 0
        # mescola mazzo
        mazzo = sample(mazzo)
        # inserisci le prime 36 carte nel tavolo
        tavolo = matrix(mazzo[1:36], nrow=4, ncol=9)
        rownames(tavolo) = c("D","C","S","B")
        colnames(tavolo) = 1:9
        tavolo.semi = matrix(names(mazzo[1:36]), ncol=9, nrow=4)
        rownames(tavolo.semi) = c("D","C","S","B")
        colnames(tavolo.semi) = 1:9
        mano = mazzo[37:40]
        # finchè hai carte in mano
        while(length(mano)>0 & ){
            # estrai una carta dalla mano
            carta = sum(mano[1])
            seme.carta = names(mano[1])
            # se è un re allora scartalo
            if(carta == 10){
                mano = mano[2:length(mano)]
            }else{
                # prendi la carta nella posizione dal tavolo 
                carta.tavolo = tavolo[seme.carta,carta]
                # ricorda di portare i nomi
                names(carta.tavolo) = tavolo.semi[seme.carta, carta]
                # e mettila in mano
                mano = c(carta.tavolo, mano[2:length(mano)])
                # piazzala sul tavolo nella sua posizione
                tavolo[seme.carta,carta] = carta
                # ricorda di portare i nomi
                tavolo.semi[seme.carta,carta] = seme.carta                
            }
        }
        # se non hai carte in mano e sul tavolo ci sono tutte le carte giuste allora hai vinto
        if(     sum(    tavolo["D",]==1:9   )!=9    &      sum(    tavolo.semi["D",]==rep("D", 9)  )!=9      )
            if(     sum(    tavolo["C",]==1:9   )!=9    &      sum(    tavolo.semi["C",]==rep("C", 9)  )!=9      )
                if(     sum(    tavolo["S",]==1:9   )!=9    &      sum(    tavolo.semi["S",]==rep("S", 9)  )!=9      )
                    if(     sum(    tavolo["B",]==1:9   )!=9    &      sum(    tavolo.semi["B",]==rep("B", 9)  )!=9      )
                        win = 1

        return(win)
    }

    VA = sapply(1:nrepl, play)
    return(mean(VA))
}

### soluzione gruppo  non funge
scarta.Re = function(nrepl=1e4){
    mazzo = sort(rep(1:10, 4))
    names(mazzo)= rep(1:4,10)

    game = function(i){
        win = 0
        mazzo = sample(mazzo)
        tavolo = matrix(mazzo[1:36], nrow=4, ncol=9, byrow=T)
        names(tavolo)=names(mazzo[1:36])
        mano = mazzo[37:40]
        play = function(tavolo, mano){
            if(length(mano)==0) return(sum(tavolo))
            carta=mano[1]
            while(carta!=10){
                cambio=tavolo[as.numeric(names(carta)), carta]
                names(cambio) = names(tavolo)[9*(as.numeric(names(carta))-1)+carta]
                tavolo[as.numeric(names(carta)), carta]=0
                carta = cambio
            }
            return(play(tavolo, mano[-1]))
        }
        if(play(tavolo,mano)==0) win=0
        return(win)
    }
    VA = sapply(1:nrepl, game)
    return(mean(VA))
}


# versione prof
scartaRe = function(nrepl=1e4){

    mazzo = sort(rep(1:10,4))
    names(mazzo)= rep(1:4,10)

    play = function(i){
        win = 0
        mazzo = sample(mazzo)
        tallone = mazzo[37:40]

        while(length(tallone)>0){
            start = tallone[1]
            tallone = tallone[-1]
            while(start!=10){
                old.s = start
                start = mazzo[9*(as.numeric(names(start))-1)+start]
                mazzo[9*(as.numeric(names(old.s))-1)+old.s]=0
            }
        }
        if(sum(mazzo[1:36])==0) win = 1
        return(win)
    }
    e = mapply(play, 1:nrepl)
    return(mean(e))

}
# parallelizzazione
scartaRe = function(nrepl=1e4){

    mazzo = sort(rep(1:10,4))
    names(mazzo)= rep(1:4,10)

    play = function(i){
        win = 0
        mazzo = sample(mazzo)
        tallone = mazzo[37:40]

        while(length(tallone)>0){
            start = tallone[1]
            tallone = tallone[-1]
            while(start!=10){
                old.s = start
                start = mazzo[9*(as.numeric(names(start))-1)+start]
                mazzo[9*(as.numeric(names(old.s))-1)+old.s]=0
            }
        }
        residui = sum(mazzo!=0)
        if(sum(mazzo[mazzo!=0]==rep(1:9,4)[mazzo!=0] & as.numeric(names(mazzo[mazzo!=0])==sort(rep(1:4,9))[mazzo!=0]==residui)))
            mazzo[mazzo!=0]=0
        if(sum(mazzo[1:36])==0) win = 1
        return(win)
    }
    e = mcmapply(play, 1:nrepl, mc.cores=4)
    return(mean(e))

}