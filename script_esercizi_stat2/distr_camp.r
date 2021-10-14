# restituisce sempre la prob minore del parametro passato
# VIENE FUORI DA PRIMI ESERCIZI DEL CAPITOLO 7 DEL NEWBOLD
# da migliorare
DC = function(media, varianza, nsam, val){
    media.sam = media
    var.sam = varianza/nsam
    z = (val-media.sam)/sqrt(var.sam)
    
    return(pnorm(z))
}