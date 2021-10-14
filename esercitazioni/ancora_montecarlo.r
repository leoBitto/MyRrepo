# tre dadi lanciati simultaneamente
# il dado con punteggio più alto viene tolto
# se due o tre hanno punteggio piu alto se ne
# toglie uno a caso dopodiche si guarda alla somma
# se la somma è sette si lancia una moneta se testa allora si vince 50
# se la somma dei dadi è maggiore di 10 si vince 100

game = function(nrepl=1e4){

    single.game= function(i){
        win=0
        dadi = sample(1:6, 3, rep=T)
        # elimina dado maggiore
        dadi = sort(dadi)
        dadi = dadi[1:2]

        if(sum(dadi)==7){
            # lancio moneta
            moneta = sample(1:2, 1)
            if(moneta==1){
                win = 50
            }
        }else if(sum(dadi)>10){
            win = 100
        }
        return(win)
    }

    res = mapply(single.game, 1:nrepl)
    plot(1:nrepl, cumsum(res)/1:nrepl, type="l")
    print(paste("valore atteso: ", mean(res)))

    return(mean(res))

}

# craps
craps = function(nrepl=1e4){
    
    mano = function(i){
        win=F
        # primo lancio di dadi
        punti = sum(sample(1:6, 2, rep=T))
    
        if(punti==7 | punti==11){
            win=T
        }else if( sum(punti==c(4,5,6,8,9,10))==1 ){
        
            punti2 = 0
            while(punti != punti2 & punti2 != 7)
                # secondo lancio di dadi
                punti = sum(sample(1:6, 2, rep=T))
                
            if(punti == punti2) win=T
        }
        return(win)
    }
    
    res = mapply(mano, 1:nrepl)
    plot(1:nrepl, cumsum(res)/1:nrepl, type="l")
    return(mean(res))
    
}




########### VERSIONE PROF

craps = function(nrepl=1e4){
    play=function(i){
        win=0
        punti=sum(sample(1:6,2,rep=T))

        if( sum(punti==c(7,11))==1 ) win=1 

        else if(sum(punti==c(4:6,8:10))==1){

            new.pt=0 
            while(punti!=new.pt & new.pt!=7)
                 new.pt=sum(sample(1:6,2,rep=T))

            if(new.pt==punti) win=1

            }
            return(win)
    } 
    e=mapply(play,1:nrepl)
    plot(1:nrepl, cumsum(e)/1:nrepl, type="l")
    print(mean(e))
}