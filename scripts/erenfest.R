#!/usr/bin/Rscript

# abbiamo un contenitore con un certo numero di
# palline di due colori diversi, l'estrazione di una
# pallina ne determina un cambio di colore della stessa.
# dopo quante estrazioni (iterazioni) sono necessarie
# per arrivare all'equilibrio?
# non si raggiunge mai l'equilibrio nel caso in cui 
# il totale delle palline sia dispari

#parametri
# nb = numero di palline bianche, codifica numero 0
# nr = numero di palline rosse, codifica numero 1
Er = function(nb , nr){
    urna = c( rep(0,nb), rep(1, nr) )

    # numero totale di palline, usato per evitare
    # di chiamare length tutti i cicli
    tot=nb+nr
    #contatore cicli
    cnt = 0

    #mescola le palline (INUTILE)
    urna = sample(urna)

    #RIPETIZIONE ESPERIMENTO (SIMULAZIONE)
    # la differenza in valore assoluto deve essere
    # maggiore di uno per continuare a cambiare
    while(abs(nr-nb) > 1){
        #ESPERIMENTO
        #estrai una pallina
        i = sample(1:tot, 1)

        #cambia colore [non si una un if!]
        # si usa la somma dei due codici di colore
        # usando il complementare. al posto di 1 si usa
        # la somma dei valori di codifica
        urna[i] = 1-urna[i]

        #conta le palline
        nr = sum(urna==1)
        nb = tot - nr
        cnt = cnt + 1
    }
    print(paste("raggiunto l'equilibrio in", cnt, "cicli")) 
}