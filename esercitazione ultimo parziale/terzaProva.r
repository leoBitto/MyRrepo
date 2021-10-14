#Da un mazzo di 40 carte romagnole ben mescolato si estraggono senza reimmissione 10 carte.
#Successivamente, si lancia 10 volte una moneta perfettamente bilanciata.

#Le posizioni delle teste ottenute nella sequenza dei 10 lanci indicano le carte 
#che devono essere sostituite, solo se sono maggiori di 3, pescando casualmente dal mazzo delle carte rimanenti.

#Se tra le carte "di mano" è presente una "marafona" di Denari (ovvero asso, due e tre) si vincono 100 euro.
#Se tra le carte "di mano" è presente una "marafona" di uno degli altri seme si vincono 50 euro.
#Negli altri casi non si vince niente.

#Calcolare, tramite adeguato processo simulativo, il valore atteso di questo gioco.
#

es = function(nrepl=1e4){
    mazzo = sort(rep(1:10, 4))
    names(mazzo)= rep(c("S","C","D","B"), 10)

    game = function(i){
        win = 0
        mazzo = sample(mazzo)
        estr = mazzo[1:10]
        teste = rbinom(10, 1, 0.5)
        n.cambio = sum(teste)
        # prendi le nuove carte dal mazzo rimanente in numero uguale alle carte che
        # sono maggiori di 3 nella posizione dettata dalle teste
        newCards = sample(mazzo[11:40], n.cambio)
        # prendi il nome delle nuove carte
        n.newCards = names(newCards)
        # metti le nuove carte nella mano al posto delle carte segnate da
        # teste==1 & estr>3
        estr[teste==1 & estr>3] = newCards
        ## MI DA ERRORE
        # qui l'inghippo.. non so perchè ma mi dice che 
        #   Error in names(estr[teste == 1 & estr > 3]) <- n.newCards : 
        #   l'attributo 'names' [3] deve essere della stessa lunghezza del vettore [2]
        # eppure 
        #   > n.newCards
        #   [1] "C" "S" "C" "S"
        #   > length(n.newCards)
        #   [1] 4
        # e

        names(estr[teste==1 & estr>3]) = n.newCards

        mara.D = sort(estr[names(estr) == "D"])
        if( length(mara.D)==3){
            if(mara.D[1] == 1 & mara.D[2]==2 & mara.D[3]==3) win=100
        }
        mara.C =  sort(estr[names(estr) == "C"])
        if( length(mara.C)==3){
            if(mara.C[1] == 1 & mara.C[2]==2 & mara.C[3]==3) win=50
        }
        mara.B =  sort(estr[names(estr) == "B"])
        if( length(mara.B)==3){
            if(mara.B[1] == 1 & mara.B[2]==2 & mara.B[3]==3) win=50
        }
        mara.S =  sort(estr[names(estr) == "S"])
        if( length(mara.S)==3){
            if(mara.S[1] == 1 & mara.S[2]==2 & mara.S[3]==3) win=50
        }

        return(win)
    }
    e = mapply(game, 1:nrepl)
    return(mean(e))
}