## crea archivio servizio
dati = data.frame(sesso=sample(1:2, 500, rep=T), tipo=sample(1:2, 500,rep=T), qualità = sample(1:10, 500, rep=T), velocità = sample(1:10, 500, rep=T), cortesia = sample(1:10, 500, rep=T))

f = function(){
    dati1=dati[dati$sesso==1,]
    s = split(dati1[,3:5], dati1$tipo)
    m1 = t(sapply(s, colMeans))

    dati2 = dati[dati$sesso==2,]
    s = split(dati2[,3:5], dati2$tipo)
    m2 = t(sapply(s, colMeans))

    a = array(NA,c(2,2,3))
    a[1,,] = m1
    a[2,,] = m2
    return(a)
}


# alternativa con for

f = function(){
    sex = unique(dati$sesso)
    a = array(NA,c(2,2,3))

    for(i in 1:length(sex)){
        data=dati[dati$sesso==sex[i],]
        s = split(data[,3:5], data$tipo)
        m = t(sapply(s, colMeans))
        a[i,,] = m
    }
    
    return(a)
}