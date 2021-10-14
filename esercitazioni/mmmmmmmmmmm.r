#solitario 123
sol123 = function(nrepl=1e4){
    mazzo = rep(1:10, 4)
    v = rep(1:3, 13, 40)

    game = function(i){

        mazzo = sample(mazzo)
        return (sum(v!=mazzo)==40)        

    }
    vincite = mapply(game, 1:nrepl)
    return(mean(vincite))

}
# versione generalizzata
sol = function(k = 3, nrepl=1e4){

    if(k<3 | k>10) stop("k deve essere compreso tra 3 e 10")

    mazzo = rep(1:10, 4)
    v = rep(1:k, l=40)

    game = function(i){

        mazzo = sample(mazzo)
        return (sum(v!=mazzo)==40)        

    }
    vincite = mapply(game, 1:nrepl)
    plot(1:nrepl, cumsum(vincite)/1:nrepl, type="l")
    return(mean(vincite)) 
}

poker.assi = function(nrepl=1e4){
    mazzo = rep(1:13, 4)
    names(mazzo) = c(rep("D", 13), rep("P", 13), rep("C", 13), rep("F", 13))
    

    mano = function(i){
        mazzo = sample(mazzo)
        win=0
        estr = mazzo[1:5]
        if(sum(estr==1)==4) win=1
        else{
             # cambia carte  non asso
            estr[ which(estr!=1)] = mazzo[6:(5+length(which(estr!=1)))]
            if(sum(estr==1)==4) win=1
        }
        return(win)
           
    }
    vincite = mapply(mano, 1:nrepl)
    plot(1:nrepl, cumsum(vincite)/1:nrepl, type="l")
    return(mean(vincite))

}

