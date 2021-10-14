es1 = function(nrepl=1e4){
    y = rchisq(nrepl, df=4)
    l = length(y[y<8])
    y[y<8]=rnorm(l, 15,2)
    hist(y)
    
}

#es3
es3 = function(nrepl=1e4){
    mazzo = sort(rep(1:10,4))
    names(mazzo) = rep(c("C","S","B","D"),10)

    game = function(i){
        win = 0
        mazzo = sample(mazzo)
        estr = mazzo[1:5]
        if(sum(estr)>35){
            estr = mazzo[1:8]
            if( sum(estr)>40 & sum(names(estr)=="D")>2 ){
                win = 100
            }
        }
        return(win)
    }
    VA = mapply(game, 1:nrepl)
    return(mean(VA))
}