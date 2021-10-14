#es1
mistura = function(nrepl=1e4){
    y = rchisq(nrepl, 4)
    #l = length(y[y>8])
    l = sum(y>8)
    y[y>8] = rnorm(l,15, 2)
    #plot(y, type="l") NO PLOT
    # si usa hist
    hist(y, prob=T)
    lines(density(x), col="navy", lwd=2)
}






#es3
nap = function(nrepl=1e4){
    mazzo = rep(1:10,4)
    names(mazzo) = c(rep("D", 10), rep("C", 10), rep("S", 10), rep("B", 10))

    # mazzo = sort(rep(1:10,4))
    # names(mazzo)=rep("B","C","D","S"),10)

    play = function(i){
        mazzo = sample(mazzo)
        win = 0
        estr = mazzo[1:5]

        if(sum(estr)>35){
            estr = mazzo[1:8]
            if(sum(estr)>40 & sum(names(estr)=="D")>=3) win=100
        }
        return(win)
    }
    e = mapply(play, 1:nrepl)
    return(mean(e))
}

# correzione prof es3