################################################
VETTORI
################################################


FACILE

In riferimento ad un seme di generazione dei numeri casuali pari a 28 ed effettuando le operazioni in stretta sequenza, generare un vettore di 1000 osservazioni casuali estratte con reimmissione dalla sequenza che va da 10 a 70 con passo 3.
La mediana dei valori estratti è:

set.seed(28)
s=sample(seq(10,70, by=3),1000, rep=T)
median(s)


In riferimento ad un seme di generazione dei numeri casuali pari a 33 ed effettuando le operazioni in stretta sequenza, generare un vettore di 200 osservazioni casuali estratte senza reimmissione dalla sequenza di lunghezza 300 che va da 10 a 70.
La deviazione standard dei valori estratti è:

set.seed(33)
s=sample(seq(10,70, length.out=300),200, rep=F)
sd(s)


In riferimento ad un seme di generazione dei numeri casuali pari a 38 ed effettuando le operazioni in stretta sequenza, generare un vettore di 300 osservazioni casuali estratte con reimmissione dalla sequenza che va da 1 a 10 con passo 0.1.
La moda dei valori estratti è:

set.seed(38)
s=sample(seq(1,10, by=.1),300, rep=T)
t=table(s)
t[t==max(t)]




MEDIA

In riferimento ad un seme di generazione dei numeri casuali pari a 28 ed effettuando le operazioni in stretta sequenza, si simuli il lancio per 1000 volte di un dado a 6 facce truccato nel quale una qualsiasi faccia pari ha il doppio della probabilità di verificarsi di una quqlsiasi faccia dispari. 
Il 10° percentile delle estrazioni effettuate corrisponde al valore:

set.seed(28)
s=sample(1:6, 1000, rep=T, prob=rep(c(1,2),3))
quantile(s, prob=.10)


In riferimento ad un seme di generazione dei numeri casuali pari a 30 ed effettuando le operazioni in stretta sequenza, si simuli il lancio per 1000 volte di un dado a 20 facce truccato, nel quale una qualsiasi delle facce maggiori di 15 ha il doppio della probabilità di verificarsi di una qualsiasi delle precedenti.
Il 73esimo percentile delle estrazioni effettuate corrisponde al valore:

set.seed(30)
s=sample(1:20, 1000, rep=T, prob=c(rep(1,15), rep(2,5)))
quantile(s, prob=.73)


In riferimento ad un seme di generazione dei numeri casuali pari a 3 ed effettuando le operazioni in stretta sequenza, si ripeta 100 volte l''estrazione (con reimmissione) di una carta da un mazzo di 52 (composto da 4 semi di 13 carte ciascuno).
Il 23esimo percentile delle estrazioni effettuate corrisponde alla carta:

set.seed(3)
s=sample(rep(1:13,4), 100, rep=T)
quantile(s, prob=.23)



DIFFICILE

In riferimento ad un seme di generazione dei numeri casuali pari a 12 ed effettuando le operazioni in stretta sequenza, si estraggano 1000 valori da una distribuzione uniforme tra 5 e 7.
Si pongano a missing i valori che distano dalla media per più di 1.3 volte la deviazione standard.
Il numero dei valori posti a missing è:

set.seed(12)
v=runif(1000, min=5, max=7)
m=mean(v)
s=sd(v)
v[abs(v-m)>1.3*s]=NA
sum(is.na(v))


In riferimento ad un seme di generazione dei numeri casuali pari a 2 ed effettuando le operazioni in stretta sequenza, si estraggano 500 valori da una distribuzione uniforme tra 0 e 6.
Si pongano a missing i valori che distano dalla media per più di 1.5 volte la deviazione standard.
La media dei valori rimanenti è:

set.seed(2)
v=runif(500, min=0, max=6)
m=mean(v)
s=sd(v)
v[abs(v-m)>1.5*s]=NA
mean(v, na.rm=T)


In riferimento ad un seme di generazione dei numeri casuali pari a 20 ed effettuando le operazioni in stretta sequenza, si estraggano 5000 valori da una distribuzione uniforme tra 1 e 6.
Si pongano a missing i valori che distano dalla mediana per più di 1.6 volte la deviazione standard.
La deviazione standard dei valori rimanenti è:

set.seed(20)
v=runif(5000, min=1, max=6)
m=median(v)
s=sd(v)
v[abs(v-m)>1.6*s]=NA
sd(v, na.rm=T)






################################################
MATRICI
################################################

In riferimento ad un seme di generazione dei numeri casuali pari a 3, si lancino 2 dadi regolari 100 volte.
Quante volte esce lo stesso punteggio su entrambe le facce?

set.seed(3)
d1=sample(1:6,100, rep=T)
d2=sample(1:6, 100, rep=T)
m=table(d1,d2)
sum(diag(m))


In riferimento al seme di generazione dei numeri casuali pari a 28, ed eseguendo le operazioni in stretta sequenza:

a) riempire PER RIGA una matrice 5 x 4 di 20 osservazioni estratte senza reimmissione dall''oggetto sequenza numerica tra -2*pigreco e 2*pigreco di lunghezza 200;
b) riempire PER COLONNA una matrice 4 x 5 di 20 osservazioni estratte da una uniforme U(0,1);
c) eseguire l'operazione prodotto matriciale tra i due oggetti nell'ordine descritto.

Il determinante della matrice è:

set.seed(28)
m1=matrix( sample(seq(-2*pi,2*pi, length.out=200),20), byrow=T, ncol=4 )
m2=matrix( runif(20), ncol=5)
det(m1%*%m2)


In riferimento al seme di generazione dei numeri casuali pari a 20, riempire una matrice 50 x 50 di osservazioni estratte senza reimmissione dalla sequenza numerica da 1 a 2*pi-greco lunga 10000.
Estrarre la sottomatrice corrispondente a tutte le riche e le colonne pari.
La traccia di questa matrice è:

set.seed(20)
m1=matrix( sample(seq(1,2*pi, length.out=10000), 2500, rep=F), ncol=50 )
s=seq(2,50, by=2)
m2=m1[s,s]
sum(diag(m2))


In riferimento ad un seme di generazione dei numeri casuali pari a 3 ed effettuando le operazioni in stretta sequenza, creare PER RIGA  una matrice 10x10 di 100 osservazioni estratte da una Uniforme(1,8).
La media dei valori sulla prima e terza colonna è:

set.seed(3)
m=matrix(runif(100, min=1, max=8),nrow=10, byrow=T)
mean(m[,c(1,3)])





