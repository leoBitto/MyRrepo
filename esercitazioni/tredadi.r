treDadi = function(nrepl=1e4){

    play = function(i){
        dadi = sort(sample(1:6, 3, rep=T))
        dadi = dadi[1:2]
        win = 0
        if(sum(dadi)==7 & rbinom(1,1,0.5)==1) win = 50
        if(sum(dadi)>10) win = 100
        return(win)
    }

    VA = mapply(play, 1:nrepl)
    return(mean(VA))
}