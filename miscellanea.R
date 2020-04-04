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
            rata = rata[-length(rata)])


tasso = 0.003
entita_fin = 5000
anni_rim = 20
tipo_amm = c('all\'italiana', 'alla francese')
quota_capitale = entita_fin/anni_rim
quota_interessi = quota_capitale * tasso
debito_residuo = entita_fin - (cumsum(c(0,rep(quota_capitale,anni_rim))))
quota_interessi = tasso * debito_residuo %>% 
  append(FALSE, after =0)
rata = quota_capitale + quota_interessi

dt = tibble(tasso = rep(tasso,anni_rim +1),
            quota_capitale = rep(quota_capitale,anni_rim+1),
            debito_residuo = debito_residuo,
            quota_interessi= quota_interessi[-length(quota_interessi)],
            rata = rata[-length(rata)]
            )

