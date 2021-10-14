redditi = read.table('/home/leonardo/progetti/R/archivi/redditi.csv', header=T, dec=",", sep=";")
tasse = function(redditi){
    reddito.annuo = redditi$reddito * sample(13:14, nrow(redditi), replace=T, prob=c(0.7,0.3))
    redditi = cbind(redditi, reddito.annuo)

    calcolo = function(i, annuo){
        tasse = 0
        #print(paste("annuo: ", annuo))

        # i primi 3000 euro
        annuo = annuo -3000
        #print(paste("annuo primo scaglione: ", annuo))
        
        if((annuo-7000)>0){
            # 20% da 3000 a 10000
            tasse = tasse + ((7000*20)/100)
            annuo = annuo -7000
            #print(paste("annuo secondo scaglione: ", annuo))
            #print(paste("tasse secondo scaglione: ", tasse))
        } else{
            rim = 7000-annuo
            tasse = tasse + ((rim*20)/100)
        }
        if((annuo-20000)>0){
            # 27% da 10000 a 30000
            tasse = tasse + ((20000*27)/100)
            annuo = annuo -20000
            #print(paste("annuo terzo scaglione: ", annuo))
            #print(paste("tasse terzo scaglione: ", tasse))
        }else{
            rim = 20000-annuo
            tasse = tasse + ((rim*20)/100)
        }
        if(annuo>0){
            # 39% più di 30000
            # si calcola su tutto il rimanente
            tasse = tasse + ((annuo*39)/100)
            annuo = annuo -30000
            #print(paste("annuo quarto scaglione: ", annuo))
            #print(paste("tasse quarto scaglione: ", tasse))
        }
            
        #print(paste("tasse: ", tasse))
        return(tasse)
    }
    tasse.contribuenti = mapply(calcolo, 1:100, redditi$reddito )  
    
    redditi = cbind(redditi, tasse.contribuenti)   
    return(redditi)
}


###
#
# versione prof
#
###
scaglioni = c(0, 3000, 10000, 30000, Inf)

aliquote = c(0, 0.2, 0.27, 0.39)

tasseP = function(imponibile){
    tasse = 0
    # i indice aliquote ovvero indice ampiezza scaglioni
    i=1
    while( ( imponibile - scaglioni[i+1] ) > 0 ){
        tasse = tasse + (scaglioni[i+1]-scaglioni[i]) * aliquote[i]
        i = i+1
    }
    tasse = tasse + (imponibile - scaglione[i]) * aliquote[i] 
    return(tasse)
}
tx = mapply(tasseP, redditi$reddito.annuo)
redditi = cbind(redditi, tx)