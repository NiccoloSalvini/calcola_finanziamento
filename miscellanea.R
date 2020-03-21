# qui si crea il dataset, o meglio ci penso 
library(tidyverse)
tasso = 0.02
entita_fin = 20000
anni_rim = 10
tipo_amm = c('all\'italiana', 'alla francese')
quota_capitale = rep(entita_fin/anni_rim, anni_rim)
capitale_residuo = rep(20000,anni_rim) - quota_capitale
rate_cr =rep(0,anni_rim)
a = 1:anni_rim
for (i in seq_along(a)){
  rate_cr[i]= i*quota_capitale
}
append(rate_cr,FALSE, after=0)
debito_residuo = entita_fin-rate_cr
tasso_i =rep(tasso,anni_rim)
quota_interessi = debito_residuo*tasso
debito_res = 
df = tibble(tasso = tasso,
            n_rata = 1:anni_rim)
