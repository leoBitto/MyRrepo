
integrale = function(a,b, n=1000){
    # genera la funzione
    x = seq(from=a, to=b, length=n)
    y = x^2+x
    plot(x,y, type="l", col="red", lwd="2")

    # genera punti casuali x e y
    xGen = sample(a:b, n, rep=T)

    ##ERRORE: IN Y DOBBIAMO GENERARE PUNTI DA 0 AL MASSIMO NON DAL MINIMO DELLA FUNZIONE
    #yGen = sample(min(y):max(y), n, rep=T)
    ##nel caso la funzione sia minore di zero min(y) va bene
    if(min(y)>0) # dobbiamo specificare che il minimo sia zero 

    yGen = sample(0:max(y), n, rep=T)
    #counter
    cnt = 0

#BLOCCO DA RIPETERE
    for(i in 1:n){
        # guarda, per ogni punto generato, se è all'interno della funzione:
        #      se y.generato è superiore a y.funzione
        if(yGen[i]<y[i]) cnt = cnt+1
    }

    return(cnt/1000)
}


#VERSIONE PROF
#1 puoi passare una funzione a una funzione
fun = function(f,x){
    f(x)
}
#chiamabile con:
#fun(sqrt[sin,cos,etc..], pi[valore numerico, vettore])

#2
integrale = function(f,a,b, npti=10000){

    if(a>b) stop("a non può superare b")

    x = runif(npti, min=a, max=b)
    ymax = max(f(x))
    ymin = min(f(x))
    if(ymin>0) ymin=0
    y = runif(npti, min = ymin, max = ymax)

    #calcolo area nella parte positiva
    #numero di punti sotto la funzione positiva
    # fratto numero di punti generati nella parte positiva
    # moltiplicata per l'area totale positiva
    punti.positivi = (y<f(x) & y>0)
    area.pos = sum(punti.positivi)/sum(y>0) * (b-a)*(ymax-0)

    area.neg=0
    #calcolo area nella parte negativa
    if(ymin<0){
        punti.negativi = (y>f(x) & y<0)
        area.neg = sum(punti.negativi)/sum(y>0) * (b-a)* (0-ymin)
        chk = punti.positivi | punti.negativi
    }

    #disegna esperimento
    plot(x,y, pch=19, cex=.3, col=c("navy","pink")[chk+1])
    print(paste("integrale: ", area.pos - area.neg))
    
}

#DA FARE 
#DISEGNA PUNTI NON DENTRO AREA IN BIANCO
#FALLO FUNZIONARE SE LA FUNZIONE È SOLO NEGATIVA