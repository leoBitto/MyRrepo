stima = function(camp, propor){

    n = length(camp)
    media.pop = mean(camp)
    var.pop = sum((camp-media.pop)^2)/(n-1)
    var.media.camp = var.pop/n
    phat = sum(camp>propor)/n
    var.phat = (phat*(1-phat))/n

    print(paste("media pop.: ", media.pop))
    print(paste("var pop.: ", var.pop))
    print(paste("var media camp.: ", var.media.camp))
    print(paste("proporzione con più di ", propor, " :", phat))
    print(paste("varianza proporzione: ", var.phat))
}