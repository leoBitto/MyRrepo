es3 = function(nrepl=1e4){
    
    v = function(i){
        win = 0
        mazzo = rep(1:10,4)
        dadi = sample(1:6, 40, rep=T)
        corr = sum(mazzo==dadi)
        
        if(corr>2 & corr<7) win = (corr-2)*5
        else if(corr>6) win=100
        
        return(win)
    }
    vincite = mapply(v, 1:nrepl)
    return(mean(vincite))

}

pokerassi= function(nrepl=1e4){
    game = function(i){
        mazzo = rep(1:10, 4)
        names(mazzo) = c(rep("D",10), rep("C", 10), rep("B",10), rep("S",10))
        mazzo = sample(mazzo)
        win=0
        estr = mazzo[1:5]
        
        den = sum(names(estr)=="D")
        teste = sum(rbinom(den,1,0.5))

        if(teste>0) estr=c(estr, mazzo[6:(5+den)])
        if(sum(estr==1)==4) win=1
        return(win)
    }
    vincite = mapply(game, 1:nrepl)
    return(mean(vincite))
  
}

h = function(nrepl=1e4){
    t0 = Sys.time()
    es = function(i){
        x = rchisq(1,4)
        if(x>8)
            x = rnorm(1,15,2)
        
        return(x)
    }
    values = mapply(es, 1:nrepl)
    hist(values, col="red")
    print(Sys.time()-t0)
}


h2 = function(nrepl=1e4){
    t0 = Sys.time()
    x = rchisq(nrepl, 4)
    l = sum(x>8)
    x[x>8] = rnorm(l, 15,2)

    hist(x, col="red")
    print(Sys.time()-t0)
}

d= data.frame(id=1:217,
 regioni=sample(1:20, 217, rep=T),
  notti=sample(1:15, 217, rep=T),
 sodd = sample(1:10, 217, rep=T))

 reg = sort(unique(d$regioni))
 notti = sort(unique(d$notti))
 m = matrix(ncol=length(notti), nrow=length(reg))
 for(i in 1:length(reg)){
     for(j in 1:length(notti)){
         m[i,j] = mean(d$sodd[d$reg==reg[i] & d$notti==notti[j]])
     }
 }

g= function(nrepl=1e4){
    sg = function(i){
        mazzo = rep(1:10, 4)
        names(mazzo) = c(rep("D", 10), rep("C",10), rep("S", 10), rep("B", 10))
        mazzo= sample(mazzo)
        estr=mazzo[1:5]
        val=sum(estr)
        win = 0
        if(val>35){
            estr = mazzo[1:8]
            val=sum(estr)
            if(val>40 & sum(names(estr)=="D")>2)
                    win = 100
        } 
        return(win)
    }
    wins = mapply(sg, 1:nrepl)
    plot(1:nrepl, cumsum(wins)/(1:nrepl), type="l")
    return(mean(wins))
    
}

ff = function(nrepl=1e4){
    genera= function(i){
        x = rt(1, df=15)
        if(x< -2) x = rnorm(1,-8, 2)
        if(x> 2)  x = rnorm(1, 8, 2)
        return(x)
    }
    x = sapply(1:nrepl, genera)
    hist(x, col="navy", prob=T)
}
