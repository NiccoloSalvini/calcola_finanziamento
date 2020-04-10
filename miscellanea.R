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

            # TASSO ITALIANO
######################################################
######################################################


TassoAnnuale = 10/100
entita_fin = 100
PAit = 1/4
num_ann = 5
TassoInfra = TassoAnnuale * PAit
num_rate = (PAit)^-1 * num_ann
quota_capitale = entita_fin/num_rate
debito_residuo = entita_fin - (cumsum(c(0,rep(quota_capitale,num_rate))))
quota_interessi = TassoInfra * debito_residuo %>% 
  append(FALSE, after =0)
rata = quota_capitale + quota_interessi
rata = rata[-length(rata)]
rata[1] = 0
totale_interessi = sum(quota_interessi)
totale_rata = sum(rata)

dt = tibble(TassoInfra = rep(TassoInfra,num_rate +1),
            TassoAnnuale = rep(1:num_ann,len = num_rate+1, each = (PAit)^-1 ),
            quota_capitale = round(rep(quota_capitale,num_rate+1),2),
            debito_residuo = round(debito_residuo,2),
            quota_interessi= round(quota_interessi[-length(quota_interessi)],2),
            rata = round(rata,2)
)


###################################################
###################################################




          # TASSO FRANCESE
######################################################
######################################################



TassoAnnuale = 5/100
entita_fin = 8000
PAfr = 1/4
num_ann = 3
TassoInfra = (1+ TassoAnnuale)^PAfr -1
num_rate = (PAfr)^-1 * num_ann
rata = rep(entita_fin / ((1 - (1+TassoInfra)^-num_ann)/TassoInfra), num_rate)

quota_interessi = entita_fin * TassoInfra
quota_capitale = entita_fin - quota_interessi
debito_residuo = entita_fin - quota_capitale
rata = rata[-length(rata)]

# trovi i marginali 
totale_interessi = sum(quota_interessi)
totale_rata = sum(rata)

dt = tibble(TassoInfra = round(rep(TassoInfra,num_rate +1),4),
            AnnoCorrente = rep(1:num_ann,len = num_rate+1, each = (PAit)^-1 ),
            quota_capitale = round(rep(quota_capitale,num_rate+1),2),
            debito_residuo = round(debito_residuo,2),
            quota_interessi= round(quota_interessi[-length(quota_interessi)],2),
            rata = round(rata,2))




######################################################
######################################################






tags$link(rel = "icon",
          type = "image/gif",
          href = "https://guidetoiceland.is/image/389003/x/0/the-beautiful-waterfalls-of-south-iceland-seljalandsfoss-skogafoss-amp-gljufrabui-1.jpg"),
title = "Ti Spiego il Finanziamento WEBAPP",
windowTitle = 'CalFin APP'



###################################################
###################################################
  
# inizializzo url e query css
url1 = 'https://www.euribor-rates.eu/it/tassi-euribor-aggiornati/2/euribor-tasso-3-mesi/'
css = '.col-lg-4 .card-body'
NamesGio = c('Data','TassoGio')
NamesMes = c('Data','TassoMes')
NamesAnn = c('Data','TassoAnn')

# prendo tabellone con tutti e tre dataset  
tabellone = url1 %>%
  read_html() %>% 
  html_table()

# qui prendo la sublist [[1]] che corrisponde ai dati al giorno
# e pulisco togliendo percentuale e converto a data
algiorno = tabellone[[1]] %>% 
  as_tibble() %>%
  set_names(NamesGio)

algiorno$Data = algiorno$Data %>% 
  dmy()

algiorno$TassoGio = algiorno$TassoGio %>%
  str_replace_all('\\%','') %>% 
  str_replace_all('\\,','.') %>%
  as.numeric()
            
# qui prendo la sublist [[2]] che corrisponde ai dati al mese
# e pulisco togliendo percentuale e converto a data
almese = tabellone[[2]] %>% 
  as_tibble() %>% 
  set_names(NamesMes)

almese$Data = almese$Data %>% 
  dmy()

almese$TassoMes = almese$TassoMes %>%
  str_replace_all('\\%','') %>% 
  str_replace_all('\\,','.') %>%
  as.numeric()
  

# qui prendo la sublist [[3]] che corrisponde ai dati al anno
# e pulisco togliendo percentuale e converto a data
allanno = tabellone[[3]] %>%
  as_tibble() %>% 
  set_names(NamesAnn)

allanno$Data = allanno$Data %>% 
  dmy()

allanno$TassoAnn = allanno$TassoAnn %>%
  str_replace_all('\\%','') %>% 
  str_replace_all('\\,','.') %>%
  as.numeric()
# qui faccio merge tutto insieme

completo = cbind(algiorno,almese,allanno)

