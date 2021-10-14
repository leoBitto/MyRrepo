#es1
es1 = function(nrepl=1e4){
    chi = function(i){
        v = rchisq(nrepl, i)
        return(sum(v[v<2]/nrepl))
    }
    return(mapply(chi, 2:30))
}
#es2


#es3
es3 = function(nrepl=1e4){
    mazzo= rep(1:10,4)

    game = function(i){
        win = 0
        mazzo = sample(mazzo)
        dadi = sample(1:6, 40, rep=T)
        s = sum(mazzo==dadi)
        if( s > 2 ){
            if(s == 3){
                win = 5
            }else if(s<7){
                win = (s-2)*5
            }else{
                win = 100
            }
        }
        return(win)
    }
    VA = mapply(game, 1:nrepl)
    return(mean(VA))
}