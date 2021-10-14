pdf.unif = function(a,b, xlim=c(-5,5)){
 v = c(a,0)
 v = rbind(v, c(a, dunif(a,a,b)))
 v = rbind(v, c(b, dunif(b,a,b)))
 v = rbind(v, c(b,0))
 ylim=c(0, 1.2/(b-a))
 plot(v, type="l", lwd=2, xlab="x", ylab="density",xlim=xlim,ylim=ylim)
 title(paste("Uniform distribution between ",a," and ",b))
}
