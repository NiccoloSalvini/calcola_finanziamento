
library(rvest)
url = 'https://www.euribor-rates.eu/it/tassi-euribor-aggiornati/2/euribor-tasso-3-mesi/'

tassi_giorno = url %>% 
  read_html()  %>%  
  html_node(css  ='.mb-lg-0:nth-child(1) .card-body') %>%  
  html_text() %>% 
  str_replace_all('\\r', '') %>% 
  str_squish() %>% 
  substring(seq(1,180,18),seq(18,180,18))

tassi_mese = url %>% 
  read_html()  %>%  
  html_node(css  ='.mb-lg-0+ .mb-lg-0 .card-body') %>%  
  html_text() %>% 
  str_replace_all('\\r', '') %>% 
  str_squish() %>% 
  substring(seq(1,180,18),seq(18,180,18))

tassi_anno= url %>% 
  read_html()  %>%  
  html_node(css  ='.mb-lg-0~ .mb-lg-0+ .col-lg-4 .card-body') %>%  
  html_text() %>% 
  str_replace_all('\\r', '') %>% 
  str_squish() 

