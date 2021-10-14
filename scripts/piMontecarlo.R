stima.pi = function(npti=10000){
    x = runif(npti, min=-1, max=1)
    y = runif(npti, min=-1, max=1)

    #equazione cerchio x^2+y^2=1
    # usa la logica vettoriale per creare un vettore di true o false
    # permette di evitare un ciclo for che impatterebbe negativamente
    # sulle prestazioni dell'algoritmo
    z = (x^2+y^2)<1

    #disegna esperimento
    plot(x,y,
        pch=19,
        cex=.5,
        #si aggiunge z+1 perchè col ha bisogno di numeri e z è un vettore true o false
        col=c("navy", "pink")[z+1],
        xlab="x", ylab="y",
        main=paste("npti= ", npti)
        )

    return(sum(z)/npti*4)
}