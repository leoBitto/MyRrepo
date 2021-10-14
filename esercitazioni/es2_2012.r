cf = rep("aaa", 200)
regione = sample( 1:20, 200, rep=T)
ncf = sample(1:5, 200, rep=T)
ta = sample(1:2, 200, rep=T)
ds = sample(1:15, 200, rep=T)
ac = sample(200:5000, 200, rep=T)
d = data.frame(
   codice = cf,
   regione = regione,
   n.comp.fam = ncf,
   tipo = ta, 
   durata = ds,
   affitto = ac
   )

durata.media = function(regione, data){
 data = data[data$regione==regione,]

s = split(data$durata, data$tipo)

 return(sapply(s,mean))
}