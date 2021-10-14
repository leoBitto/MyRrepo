# In riferimento al dataset IRIS ( al prompt di R digitare: data(iris) ),
# implementare una funzione che calcoli, per ciascuna specie di iris,
# il minimo e il massimo di tutte le variabili quantitative presenti nel dataset.
# La funzione deve in particolare restituire un oggetto bidimensionale contenente:
# 1) nella prima colonna, il nome della specie di iris;
# 2) nelle colonne successive, minimo e massimo della prima variabile,
#   poi minimo e massimo della seconda variabile, ecc..
s = split( iris[, 1:4], iris$Species )

specie = sort(unique(iris$Species))

max.min = function(j, i){
    quantile(s[[j]][,i], prob=c(0,1))
}

m = NULL
for(j in 1:length(specie)){
    m = rbind(m, as.vector(mapply(max.min, j, 1:4)))
}
 d = data.frame(specie, m)
names(d) = c("Species", as.vector(t(outer(names(iris[,1:4]), c("min", "max"), paste, sep="."))))