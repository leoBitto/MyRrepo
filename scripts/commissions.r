# crea una stima delle commissioni di una azienda 
# di vendite grazie a una simulazione di montecarlo.
# sono necessari una equazione che ci permetta di calcolare 

### commissioni = actual sales * commission rate ###

# date degli obbiettivi di vendita possiamo confrontarli
# con le vendite effettive e determinarne la percentuale
# relativa a queste. in base a queste percentuali possiamo
# determinare degli scalini che determinano le commissioni.

# ESEMPI di commissioni
# SAles Rep | sales target | actual sales | percent to plan | commission rate | commission amount
#     1         100000           88000          88.%                2%                  1760
#     2         200000          202000          101%                4%                  8080
#     3          75000           90000          120%                4%                  3600
#     4         400000          360000          90%                 0%                    0
#     5         500000          350000          70%                 0%                    0

# total     |  1275000         1090000                                                 13440

# Rate schedule
# 0-90%  | 2%
# 91-99% | 3%
# >=100% | 4%

# per creare la simulazione montecarlo abbiamo anche bisogno di 
# una serie di input che permettono di essere inseriti nella
# equazione:
# 1. ci servolo le percentuali che possiamo definire da un database 
#    di dati storici dell'azienda, in questo caso verranno simulati
#    dati tramite una distribuzione normale con media 1 e sd 0.1
# 2. i sales target, che possiamo definire sempre da dati storici 
#     dell'azienda. in questo caso li simuleremo come una poisson 
#       la cui lambda è da decidere

# possiamo dunque calcolare gli actual sales moltiplicando i due input 
# che abbiamo generato tra di loro.

# calcoliamo poi i commission rate per ogni venditore e li moltiplichiamo
# per ottenere le singole commissioni. che vengono poi sommate

# ABBIAMO FINITO DI PROGRAMMARE una SIMULAZIONE



Mcommissioni = function(addetti=200, nrepl=1e4){

    #crea matrice di sintesi
    results = matrix(ncol = 3, nrow=8 )
    rownames(results) = c("count", "mean", "std", "min", "25%", "50%", "75%", "max")
    colnames(results) = c("Sales", "Commission Amount", "Sales Target")
    
    comm.v= NULL
    
    # loppalo
    for(i in 1:nrepl){
        # crea percentage to plan
            perc.to.plan = rnorm(addetti, 1, 0.1)
        # crea sales target
            st.values=c(75000, 100000, 200000, 300000,400000,500000)
            st.prob = c(.3,.3,.2,.1,.05,.05)
            sales.target = sample(st.values, addetti, rep=T, p=st.prob)
        # calcola actual sales ( servono a calcolare le commissioni)
            actual.sales= perc.to.plan * sales.target
        # map rate schedule to commision rate
            rate_schedule = function(v){
                rate = NULL
                for( i in 1:length(v)){
                    if(v[i]<=0.9)
                        rate = c(rate, 0.02)
                    else if(v[i]<=0.99)
                        rate = c(rate, 0.03)
                    else
                        rate = c(rate, 0.04)
                }
                return(rate)
            }

        # crea dataframe
            dati = data.frame(perc.to.plan = perc.to.plan, sales.target = sales.target, actual.sales = actual.sales)
            commission.rate = rate_schedule(dati$perc.to.plan)
            dati = cbind(dati, commission.rate)
            commission.amount = dati$commission.rate * dati$actual.sales
            comm.v = c(comm.v, commission.amount)
            dati = cbind(dati, commission.amount)
            sum.commissions = sum(dati$commission.amount)

        # summarize data
            d = c(2,3,5)
            for(j in 1:dim(results)[2]){
                for( k in d){
                    results[1,j] = length(dati[,k])
                    results[2,j] = mean(c(results[2,j], dati[,k]), na.rm=T)
                    results[3,j] = sd(c(results[3,j], dati[,k]), na.rm=T)
                    results[4,j] = min(c(results[4,j], dati[,k]), na.rm=T)
                    results[5,j] = quantile(c(results[5,j], dati[,k]), p=0.25, na.rm=T)
                    results[6,j] = quantile(c(results[6,j], dati[,k]), p=0.5, na.rm=T)
                    results[7,j] = quantile(c(results[7,j], dati[,k]), p=0.75, na.rm=T)
                    results[8,j] = max(c(results[8,j], dati[,k]), na.rm=T)
                }
            }
    }

    # plot data
    hist(comm.v, col="cyan")

}


