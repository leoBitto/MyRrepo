#es1
es1 = function(camp=1e4){
    y = rt(camp, 15)
    y[y<0 & y>-2] = rnorm(length(y[y<0 & y>-2]), -8, 2)
    y[y>0 & y<2] = rnorm(length(y[y>0 & y<2]), 8, 2)
    
    hist(y)
}

#es3
es3 = function(nrepl=1e4){
    mazzo = sort(rep(1:10,4))
    names(mazzo) = rep(c("D","C","S","B"),10)

    game = function(i){
        win = 0
        mazzo = sample(mazzo)
        estr = mazzo[1:8]
        if( sum( names(estr)=="D")==4){
            nb = sum(names(estr)=="B")
            estr = c(estr[ !names(estr)=="B" ], mazzo[9:(8+nb)])
        }
        if(sum(names(estr)=="D")>4){
                win=100
        }else{
            if(sum(estr)==75) win = 1000
            else if(sum(estr)>50) win = 50
        }
        return(win)
    }
    VA = mapply(game, 1:nrepl)
    return(mean(VA))
}


es3 = function(nrepl=1e4){
mazzo=sort(rep(1:10,4))
names(mazzo)=rep(c("D","C","B","S"),10)
play = function(i){
 win=0
 mazzo=sample(mazzo)
 estr=mazzo[1:8]
 if(sum(names(estr)=="D")==4){
 estrB=sum(names(estr)=="B")
 if(estrB>0){
 #prima si cambiano i valori
 estr[ names(estr)=="B" ]= mazzo[9:(8+estrB)]
 #poi consapevoli del fatto che la sostituzione cambia i valori ma non i nomi
 #occorre poi agire sui nomi (metadati)
 names(estr)[names(estr)=="B"]=names(mazzo[9:(8+estrB)])
 }
 }
 if(sum(names(estr)=="D")>4) win=100
 else{
 if(sum(estr)==75) win=1000
else if(sum(estr)>50) win=50
 }
 return(win)
 }
e=sapply(1:nrepl, play)
print(mean(e))
}