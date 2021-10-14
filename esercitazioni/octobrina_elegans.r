t = seq(-6,6, by=.01)

octobrinaE = function(t){
    x = t
    y = 2*sin(4*t)/(1+t^2)
    plot(x,y, type="l", col="red", lwd="2")
}