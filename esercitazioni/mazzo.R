# se il parametro viene specificato quello è il valore di default
secondoRe = function(e=10000){

    #creo un mazzo distinto in re-non re di 40 carte:
    mazzo = c(rep(0, 4), rep(1, 36))
    #inizializzazione vettore carte estratte
    estratte = NULL

    #DA FARE e volte
    for(i in 1:e){
        #rimescolo
        mazzo = sample(mazzo)
        #estraggo la seconda carta e la metto in un vettore
        estratte = c(estratte, mazzo[2])
    }
    #FINE LOOP

    #conta quanti re (0) sono stati estratti e calcola prob
    return(sum(estratte==0)/e)
}
