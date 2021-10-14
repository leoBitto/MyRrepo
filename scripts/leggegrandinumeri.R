# dimostrata tramite R per la binomiale
# la LEGGE DEI GRANDI NUMERI dice che 
# supponendo di estrarre valori da una binomiale
# la media di Xn - mi tende ( con n che tende a infinito) a zero
# ovvero converge verso il parametro che definisce l'esperimento
# in questo caso p

# nel caso n sia = 1 allora x medio è 0.5 dato da {0,1}
# nel caso n sia = 2 allora x medio è 0.5 dato da {0,0.5,1}
# nel caso n sia = 3 allora x medio è 0, 1/3, 2/3, 1
# etc etc etc..

# fino a x medio N
# quindi dobbiamo dire a R che Xmedion - p 
# in valore assoluto è maggiore di Epsilon
# (valore molto piccolo) allora si continua
# a estrarre valori 

ldgn = function(p, eps=1e-4){
    # calcola Xmedio per ogni n
    # sottrai p e mettilo in absolute value
    # controlla che sia maggiore di epsilon
    # in quel caso aumenta n e continua
    n = 1
    xmean = mean(rbinom(n,1,p))
    v.medie=xmean

    while(abs(xmean-p) > eps){
        n = n+1
        xmean=mean(rbinom(n, 1, p))
        v.medie=c(v.medie, xmean)
        print(xmean)
    }
    plot(1:n, v.medie, type="l")
    print(paste("convergenza a  ", n, "iterazioni"))
}


# versione per poisson
ldgn = function(lambda, eps=1e-4){
    # calcola Xmedio per ogni n
    # sottrai p e mettilo in absolute value
    # controlla che sia maggiore di epsilon
    # in quel caso aumenta n e continua
    n = 1
    xmean = mean(rpois(n, lambda))
    v.medie=xmean

    while(abs(xmean-lambda) > eps){
        n = n+1
        xmean=mean(rpois(n, lambda))
        v.medie=c(v.medie, xmean)
        
    }
    plot(1:n, v.medie, type="l")
    print(paste("convergenza a  ", n, "iterazioni"))
}



