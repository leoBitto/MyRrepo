es3 = function(nrepl=1e4){
    game = function(i){
        dadi = sample(1:6,4,rep=T)
        win = sum(dadi)
        if(sum(dadi==1)>3){
            win = 0
        }else if( sum(dadi==6)==4){
            win = 1000
        }else if( sum(dadi==6)==3){
            dadi = sort(dadi)
            dadi[1] = sample(1:6,1)
            if(dadi[1] == 6){
                win = 24 + sample(1:6, 1)
            }
        }
        return(win)
    }
    VA = mapply(game, 1:nrepl)
    return(mean(VA))
}

fun=function(nrep=1e4){
game=function(i){
win=0
dadi=sample(1:6,4,rep=T)
if(sum(dadi==1)<3){
 if(sum(dadi)==24) win=1000
 else{
 if(sum(dadi==6)==3){
 dadi=sort(dadi)
 dadi[1]=sample(1:6,1)
 if(sum(dadi)==24) dadi[5]=sample(1:6,1)
 }
 win=sum(dadi)
 }
 }
return(win)
}
res=mapply(game,1:nrep)
valatt=mean(res)
return(valatt)
}