#corretto
e = function(nrepl=1e4){
    
    play = function(i){
        win = 0
        dadi = sample(1:6, 4, rep=T)

        if(sum(dadi==1)>2){
            win = 0
        }
        else if(sum(dadi)==24){
            win=1000
        }
        else if(sum(dadi==6)==3){
            dadi = sort(dadi)
            dadi[1] = sample(1:6, 1)
            if(sum(dadi)==24){
                dadi=c(dadi,sample(1:6,1))
               
            }
        }

        win = sum(dadi)
        return(win)
    }
    res = mapply(play, 1:nrepl)
    plot(1:nrepl, cumsum(res)/1:nrepl, type="l")
    return(mean(res))
}


### correzione prof
e = function(nrepl=1e4){
    
    play = function(i){
        win = 0
        dadi = sample(1:6, 4, rep=T)

        if(sum(dadi==1)<3){
            if(sum(dadi)==24) win=1000
            else{
                if(sum(dadi==6)==3){
                    dadi = sort(dadi)
                    dadi[1] = sample(1:6, 1)
                    if(sum(dadi)==24)
                        dadi=c(dadi,sample(1:6,1))
                }
                win=sum(dadi)
            }
        }
        return(win)
    }
    res = mapply(play, 1:nrepl)
    plot(1:nrepl, cumsum(res)/1:nrepl, type="l")
    return(mean(res))
}



#### versione "san pietroburgo"
e = function(nrepl=1e4){
    
    play = function(i){
        win = 0
        dadi = sample(1:6, 4, rep=T)

        if(sum(dadi==1)>=3){
            win = 0
        }
        else if(sum(dadi)==24){
            win=1000
        }
        else if(sum(dadi==6)==3){
            # è sbagliato in quanto vengono rimpiazzati tutti i dadi
            # diversi da 6 con un unico valore
            #dadi[dadi!=6] = sample(1:6, 1)
            dadi = sort(dadi)
            dadi[1] = sample(1:6, 1)
            if(sum(dadi)==24){
                # solo in questo caso avviene la vincita
                # come somma dei punteggi dei dadi
                win = 24 + sample(1:6, 1)
            }
        }
        #win = sum(dadi)
        return(win)
    }
    res = mapply(play, 1:nrepl)
    plot(1:nrepl, cumsum(res)/1:nrepl, type="l")
    return(mean(res))
}