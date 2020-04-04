# qui si crea il dataset, o meglio ci penso 
library(tidyverse)

len = function(x){
  length(x) %>% 
    print()
}


tasso = 0.003
entita_fin = 5000
anni_rim = 8
numero_rata = 8 
tipo_amm = c('all\'italiana', 'alla francese')
quota_capitale = entita_fin/anni_rim
quota_interessi = quota_capitale * tasso
debito_residuo = entita_fin - (cumsum(c(0,rep(quota_capitale,8))))
quota_interessi = tasso * debito_residuo %>% 
  append(FALSE, after =0)
rata = quota_capitale + quota_interessi

data = tibble(tasso = rep(tasso,9),
            quota_capitale = rep(quota_capitale,9),
            debito_residuo = debito_residuo,
            quota_interessi= quota_interessi[-length(quota_interessi)],
            rata = rata[-length(rata)]
            )

# rate_cr =rep(0,anni_rim)
# a = 1:anni_rim
# for (i in seq_along(a)){
#   rate_cr[i]= i*quota_capitale
# }
# append(rate_cr,FALSE, after=0)
# debito_residuo = entita_fin-rate_cr
# tasso_i =rep(tasso,anni_rim)
# quota_interessi = debito_residuo*tasso


# cpt_res = entita_fin  / c(1,cumprod(1+tasso))
# capitale_residuo = entita_fin  /  c(0,cumsum(1+tasso))
# capitale_residuo = capitale_residuo[-length(capitale_residuo)]
# quota_interessi = capitale_residuo*tasso
