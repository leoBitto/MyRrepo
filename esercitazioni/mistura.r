mix = function(nrepl=1e4, alpha=0.01){
    d = rt(nrepl, 25)
    l = length(d[pt(d,25)<0.01])
    d[pt(d,25)<0.01] = rnorm(l, 0, 1)
    hist(d, breaks=30, prob=T)
}