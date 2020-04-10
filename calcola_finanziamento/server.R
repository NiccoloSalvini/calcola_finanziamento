#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    observe({
        
        # qui renderizza il plot della distributizone    
        output$distPlot = renderPlot({
    
            tasso = input$tassoit
            entita_fin = input$entita_finanziamento
            PAit = input$PAit
            num_ann = input$ANNIit
            tipo_amm = c('all\'italiana', 'alla francese')
            quota_capitale = entita_fin/num_ann
            quota_interessi = quota_capitale * tasso
            debito_residuo = entita_fin - (cumsum(c(0,rep(quota_capitale,num_ann))))
            quota_interessi = tasso * debito_residuo %>% 
                append(FALSE, after =0)
            rata = quota_capitale + quota_interessi
            
            dt = tibble(tasso = rep(tasso,num_ann +1),
                        quota_capitale = rep(quota_capitale,num_ann+1),
                        debito_residuo = debito_residuo,
                        quota_interessi= quota_interessi[-length(quota_interessi)],
                        rata = rata[-length(rata)]
            )
            hist(dt$tasso,dt$debito_residuo)
            
            
            # qui renderizza la parte Dataset con tutte le combinazioni
            # di possibili finanziameneti e tassi
            
            output$Struttura = DT::renderDataTable({
                
                if (input$selection == "Ammortamento alla Italiana" & input$selectionRegime == "Regime Interesse Semplice") {
                    TassoAnnuale = (input$tassoit)/100
                    entita_fin = input$entita_finanziamento
                    PAit = as.numeric(input$PAit)
                    num_ann = input$ANNIit
                    TassoInfra = TassoAnnuale * PAit
                    num_rate = (PAit)^-1 * num_ann
                    quota_capitale = entita_fin/num_rate
                    debito_residuo = entita_fin - (cumsum(c(0,rep(quota_capitale,num_rate))))
                    quota_interessi = TassoInfra * debito_residuo %>% 
                        append(FALSE, after =0)
                    rata = quota_capitale + quota_interessi
                    rata = rata[-length(rata)]
                    totale_interessi = sum(quota_interessi)
                    totale_rata = sum(rata)
                    dt = tibble('Numero Rata' = 0:num_rate,
                                'Tasso Infra%' = round(rep(TassoInfra,num_rate +1),4),
                                'Anno Corrente' = c(0,rep(1:num_ann,len = num_rate, each = (PAit)^-1)),
                                'Quota Capitale' = round(rep(quota_capitale,num_rate+1),2),
                                'Debito Residuo' = round(debito_residuo,2),
                                'Quota Interessi'= round(quota_interessi[-length(quota_interessi)],2),
                                'Rata' = round(rata,2))
                    
                    # Qui trasformo la prima riga
                    # di modo che sia coerente col Fin
                    dt[1,] = 0 
                    dt[1,4] = entita_fin
                    
                    DT::datatable(data = dt,
                                  filter = 'none',
                                  caption = tags$caption("Struttura Amm Ita Sem "),
                                  rownames = F,
                                  options = list(orderClasses = TRUE,
                                                 scrollCollapse = T,
                                                 pageLength = 20)
                                  ) %>%
                        formatCurrency(c('Quota Capitale', 'Debito Residuo', 'Quota Interessi', 'Rata'),'\U20AC')%>% 
                        formatPercentage('Tasso Infra%',2) %>%
                        formatStyle('Numero Rata',  
                                    color = 'red',
                                    backgroundColor = 'teal',
                                    fontWeight = 'bold',
                                    textAlign = 'center')
                }
                else if (input$selection == "Ammortamento alla Italiana" & input$selectionRegime == "Regime Interesse Composto") {
                    
                    TassoAnnuale = (input$tassoit)/100
                    entita_fin = input$entita_finanziamento
                    PAit = as.numeric(input$PAit)
                    num_ann = input$ANNIit
                    TassoInfra = (1+ TassoAnnuale)^PAit -1
                    num_rate = (PAit)^-1 * num_ann
                    quota_capitale = entita_fin/num_rate
                    debito_residuo = entita_fin - (cumsum(c(0,rep(quota_capitale,num_rate))))
                    quota_interessi = TassoInfra * debito_residuo %>% 
                        append(FALSE, after =0)
                    rata = quota_capitale + quota_interessi
                    rata = rata[-length(rata)]
                    totale_interessi = sum(quota_interessi)
                    totale_rata = sum(rata)
                    dt = tibble('Numero Rata' = 0:num_rate,
                                'Tasso Infra%' = round(rep(TassoInfra,num_rate +1),4),
                                'Anno Corrente' = rep(1:num_ann,len = num_rate+1, each = (PAit)^-1 ),
                                'Quota Capitale' = round(rep(quota_capitale,num_rate+1),2),
                                'Debito Residuo' = round(debito_residuo,2),
                                'Quota Interessi'= round(quota_interessi[-length(quota_interessi)],2),
                                'Rata' = round(rata,2))
                    
                    # Qui trasformo la prima riga
                    # di modo che sia coerente col Fin
                    
                    dt[1,] = 0 
                    dt[1,4] = entita_fin
                    
                    DT::datatable(data = dt,
                                  filter = 'none',
                                  caption = tags$caption("Struttura Amm Ita Comp"),
                                  rownames = F,
                                  options = list(orderClasses = TRUE,
                                                 scrollCollapse = T,
                                                     pageLength = 20)) 
                    
                }
                else if (input$selection == "Ammortamento alla Francese" & input$selectionRegime == "Regime Interesse Semplice"){
                    
                    TassoAnnuale = (input$tassofr)/100
                    entita_fin = input$entita_finanziamento
                    PAit = as.numeric(input$PAfr)
                    num_ann = input$ANNIfr
                    TassoInfra = TassoAnnuale * PAfr
                    num_rate = (PAfr)^-1 * num_ann
                    quota_capitale = entita_fin/num_rate
                    debito_residuo = entita_fin - (cumsum(c(0,rep(quota_capitale,num_rate))))
                    quota_interessi = TassoInfra * debito_residuo %>% 
                        append(FALSE, after =0)
                    rata = quota_capitale + quota_interessi
                    rata = rata[-length(rata)]
                    totale_interessi = sum(quota_interessi)
                    totale_rata = sum(rata)
                    dt = tibble('Numero Rata' = 0:num_rate,
                                'Tasso Infra%' = round(rep(TassoInfra,num_rate +1),4),
                                'Anno Corrente' = rep(1:num_ann,len = num_rate+1, each = (PAit)^-1 ),
                                'Quota Capitale' = round(rep(quota_capitale,num_rate+1),2),
                                'Debito Residuo' = round(debito_residuo,2),
                                'Quota Interessi'= round(quota_interessi[-length(quota_interessi)],2),
                                'Rata' = round(rata,2))
                    
                    # Qui trasformo la prima riga
                    # di modo che sia coerente col Fin
                    dt[1,] = 0 
                    dt[1,4] = entita_fin
                    
                    DT::datatable(data = dt,
                                  filter = 'none',
                                  caption = tags$caption("Struttura Amm Fra Sem"),
                                  rownames = F,
                                  options = list(orderClasses = TRUE,
                                                 scrollCollapse = T,
                                                 pageLength = 20))                    
                    
                    
                }
                else if(input$selection == "Ammortamento alla Francese" & input$selectionRegime == "Regime Interesse Composto"){
                    
                    TassoAnnuale = (input$tassofr)/100
                    entita_fin = input$entita_finanziamento
                    PAfr = as.numeric(input$PAfr)
                    num_ann = input$ANNIfr
                    TassoInfra = (1+ TassoAnnuale)^PAfr -1
                    num_rate = (PAfr)^-1 * num_ann
                    rata = rep(entita_fin / ((1 - (1+TassoInfra)^num_ann)/TassoInfra), num_rate)
                    
                    quota_interessi = entita_fin * TassoInfra    
                    quota_capitale = entita_fin - quota_interessi
                    debito_residuo = entita_fin -quota_capitale
                    rata = rata[-length(rata)]
                    
                    # trovi i marginali 
                    totale_interessi = sum(quota_interessi)
                    totale_rata = sum(rata)
                    
                    dt = tibble('Numero Rata' = 0:num_rate,
                                'Tasso Infra%' = round(rep(TassoInfra,num_rate +1),4),
                                'Anno Corrente' = rep(1:num_ann,len = num_rate+1, each = (PAit)^-1 ),
                                'Quota Capitale' = round(rep(quota_capitale,num_rate+1),2),
                                'Debito Residuo' = round(debito_residuo,2),
                                'Quota Interessi'= round(quota_interessi[-length(quota_interessi)],2),
                                'Rata' = round(rata,2))
                    
                    # Qui trasformo la prima riga
                    # di modo che sia coerente col Fin
                    dt[1,] = 0 
                    dt[1,4] = entita_fin
                    
                    DT::datatable(data = dt,
                                  filter = 'none',
                                  caption = tags$caption("Struttura Amm Fra Comp"),
                                  rownames = F,
                                  options = list(orderClasses = TRUE,
                                                 scrollCollapse = T,
                                                 pageLength = 20))  
                    
                    
                }
            })
            
            #qui metto la parte del download della struttura del prestito
            
            output$Download <- downloadHandler(
                filename = function() {
                    paste0(output$Struttura, Sys.Date(), ".csv")
                },
                content = function(file) {
                    vroom::vroom_write(output$Struttura, file)
                }
            )
            
            
            output$Tassi = DT::renderDataTable({
                
                url = 'https://mutuionline.24oreborsaonline.ilsole24ore.com/guide-mutui/euribor.asp'
                VettoreTasso = url  %>%
                    read_html(verbose = 1) %>%
                    html_nodes('.tabellaosservatorio:nth-child(5) td:nth-child(3) , .tabellaosservatorio:nth-child(5) 
                               td:nth-child(2) , .tabellaosservatorio:nth-child(5) td:nth-child(1)') %>% 
                    html_text() %>%
                    str_squish()
                
                # initialize matrix
                tab = matrix(VettoreTasso, ncol = 3,nrow = 5 ,byrow  = T)
                
                # creation of the tibble
                tab = tab %>%
                    as.tibble() %>% 
                    set_names(tab[1,]) %>% 
                    slice(-1)
                
                # fixing col types
                tab$Fixing = tab$Fixing %>% 
                    str_replace_all('\\%','') %>% 
                    str_replace_all('\\,','.') %>%
                    as.numeric()
                    
                tab$Data = tab$Data %>% 
                    dmy()
                
                DT::datatable(data = tab,
                              options = list(orderClasses = TRUE))
                
                    })
            
            REF1 = a("OsservatorioS24O", href="https://mutuionline.24oreborsaonline.ilsole24ore.com/guide-mutui/euribor.asp")
            output$tab = renderUI({
                tagList("Here the source:", REF1)
            })
            
            
            output$p = renderPlotly({
                
                
                url1 = 'https://www.euribor-rates.eu/it/tassi-euribor-aggiornati/2/euribor-tasso-3-mesi/'
                css = '.col-lg-4 .card-body'
                Names = c('Data','Tasso')

                # prendo tabellone con tutti e tre dataset  
                tabellone = url1 %>%
                    read_html() %>% 
                    html_table()
                
                # qui prendo la sublist [[1]] che corrisponde ai dati al giorno
                # e pulisco togliendo percentuale e converto a data
                AlGiorno = tabellone[[1]] %>% 
                    as_tibble() %>%
                    set_names(Names)
                
                AlGiorno$Data = AlGiorno$Data %>% 
                    dmy()
                
                AlGiorno$Tasso = AlGiorno$Tasso %>%
                    str_replace_all('\\%','') %>% 
                    str_replace_all('\\,','.') %>%
                    as.numeric()
                
                # qui prendo la sublist [[2]] che corrisponde ai dati al mese
                # e pulisco togliendo percentuale e converto a data
                AlMese = tabellone[[2]] %>% 
                    as_tibble() %>% 
                    set_names(Names)
                
                AlMese$Data = AlMese$Data %>% 
                    dmy()
                
                AlMese$Tasso = AlMese$Tasso %>%
                    str_replace_all('\\%','') %>% 
                    str_replace_all('\\,','.') %>%
                    as.numeric()
                
                
                # qui prendo la sublist [[3]] che corrisponde ai dati al anno
                # e pulisco togliendo percentuale e converto a data
                AllAnno = tabellone[[3]] %>%
                    as_tibble() %>% 
                    set_names(Names)
                
                AllAnno$Data = AllAnno$Data %>% 
                    dmy()
                
                AllAnno$Tasso = AllAnno$Tasso %>%
                    str_replace_all('\\%','') %>% 
                    str_replace_all('\\,','.') %>%
                    as.numeric()
                # non rieco ad implementare i diversi data set
                # posso provare ad avere un unico dataset e poi 
                # filtrare per un determinato input
                plot_ly(AlMese, 
                        x = ~Data, 
                        y = ~Tasso) %>% 
                    add_lines()
            })
        
        })
        
    })

})
