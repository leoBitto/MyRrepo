es1 = function(nrepl=1e4){
    mazzo = sort(rep(1:10,4))
    names(mazzo) = rep(c("S","D","C","B"), 10)
    game = function(i){
        win = 0
        mazzo = sample(mazzo)
        estr = mazzo[1:10]
        teste = sum(rbinom(5, 1, 0.5))
        BoS = sum(names(estr)=="B" | names(estr)=="S")
        if(BoS < teste) teste = BoS
        estr = c(estr[!(names(estr)=="B" | names(estr)=="S")],
        sort(estr[names(estr)=="B" | names(estr)=="S"])[-(1:teste)],
        mazzo[11:(10+teste)])


        return(win)
    }
    VA = mapply(game, 1:nrepl)
    return(mean(VA))
}   


es2 = function(nrepl=1e4){
    mazzo = sort(rep(1:10,4))
    names(mazzo)=rep(c("D","C","B","S"),10)
    
    game = function(i){
        win = 0
        mazzo = sample(mazzo)
        estr = mazzo[1:4]
        dado = sample(1:6, 1)
        if(sum(estr==dado)>0){
            estr = mazzo[1:6]
            if(sum(names(estr)[estr==dado]=="D")>4)
                estr = mazzo[1:8]
        }
        if(sum(estr==1)==4) win = 1
        return(win)
    }
    VA = mapply(game, 1:nrepl)
    return(mean(VA))
}


es3= function(nrepl=1e4){
mazzo=sort(rep(1:10,4))
names(mazzo)=rep(c("D","C","B","S"),10)
play=function(i){
mazzo=sample(mazzo)
win=0
estr=mazzo[1:4]
dado=sample(1:6,1)
#se almeno una delle carte estratte riporta il punteggio del dado...
if(sum(estr==dado)>0){
 n.add=2
 if (sum( (names(estr)[estr==dado]) =="D" )>0) n.add=4
 estr=mazzo[1:(4+n.add)]
 }
if(sum(estr==1)==4) win=1
return(win)
}
e=sapply(1:nrepl, play)
return(mean(e))
}