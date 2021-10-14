media.soggiorno = function(){
    sex = sort(unique(dati$sesso))
    tipo = sort(unique(dati$tipo))
    m = matrix(NA, nrow=length(tipo), ncol=length(sex))
    media = function(i, j){
        m[i,j] = mean( dati$durata[dati$tipo==tipo[i] & dati$sesso==sex[j]] )
    }
    m = mapply(mapply(media, 1:length(sex)), 1:length(tipo))
    return(m)
}