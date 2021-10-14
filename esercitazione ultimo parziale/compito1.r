#es1
es1 = function(nrepl = 1e4, gdl = 3){
    campioni = function(i){
        return(quantile(rchisq(100, df=gdl), 0.25))
    }
    e = mapply(campioni, 1:nrepl)
    hist(e)
}

#es2
es2 = function(){
    print(quakes[quakes$stations>120,])
    ...
}

#es3
es3 = function(p=0.5, epsilon=0.01){
    medie = mean(rbinom(50, 1, p))
    while(abs(mean(medie)-p)>epsilon){
        medie = c(medie, mean(rbinom(50,1,p)))
        
    }
    return(length(medie))
    plot(medie)

}