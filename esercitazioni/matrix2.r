frequencies2 = function(v){
    
    if(!is.vector(v)) stop("il parametro in ingresso non è un vettore")

    #crea una tabella dal vettore in ingresso
    t = table(v)
    #crea una matrice di 1 con t.length colonne e t.length righe
    matrice_diagonale = matrix(1, nrow=length(t), ncol=length(t))
    #modifica la matrice facendola diventare una matrice triangolare 
    matrice_diagonale[upper.tri(matrice_diagonale)] = 0

    m = data.frame(values = sort(unique(v)),
                    f.ass = as.vector(t),
                    f.rel = as.vector(t)/length(v),
                    fcum.ass = matrice_diagonale %*% as.vector(t),
                    fcum.rel = matrice_diagonale %*% as.vector(t)/length(v))
    
    return(m)
}