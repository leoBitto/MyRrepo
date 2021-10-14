moda=function(v){
    if(!is.vector(v)) stop("atteso un vettore in ingresso")
    t = table(v)
    m = as.numeric(names(t[t==max(t)]))
    return(m)
}

diffQ3Q1 = function(v){
    if(!is.vector(v)) stop("atteso un vettore in ingresso")
    q = quantile(v, prob=c(0.25, 0,75))
    d = q[2]-q[1]
    names(d)="Q3-Q1"
    return(d)

}


CV=function(v){
    if(!is.vector(v)) stop("atteso un vettore in ingresso")
    return(sd(v)/abs(mean(v)))
}