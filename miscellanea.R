# qui si crea il dataset, o meglio ci penso 
library(tidyverse)

len = function(x){
  length(x) %>% 
    print()
}

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
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


output$Dataset = DT::renderDataTable({
  
  
  tasso = 0.003
  entita_fin = 20000
  PAit = 4
  num_ann = 20
  num_rate = PAit * num_ann
  tipo_amm = c('all\'italiana', 'alla francese')
  quota_capitale = entita_fin/num_rate
  quota_interessi = quota_capitale * tasso
  debito_residuo = entita_fin - (cumsum(c(0,rep(quota_capitale,num_rate))))
  quota_interessi = tasso * debito_residuo %>% 
    append(FALSE, after =0)
  rata = quota_capitale + quota_interessi
  
  dt = tibble(tasso = rep(tasso,num_rate +1),
              quota_capitale = rep(quota_capitale,num_rate+1),
              debito_residuo = debito_residuo,
              quota_interessi= quota_interessi[-length(quota_interessi)],
              rata = rata[-length(rata)]
  )
  
  DT::datatable(data = dt, 
                options = list(orderClasses = TRUE))



###################################################
###################################################


tags$link(rel = "icon",
          type = "image/gif",
          href = "https://guidetoiceland.is/image/389003/x/0/the-beautiful-waterfalls-of-south-iceland-seljalandsfoss-skogafoss-amp-gljufrabui-1.jpg"),
title = "Ti Spiego il Finanziamento WEBAPP",
windowTitle = 'CalFin APP

