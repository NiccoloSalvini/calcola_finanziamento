# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    observe({
        
        # qui renderizza il plot della distributizone    
        output$distPlot = renderPlot({
    
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
            hist(dt$`Tasso Infra%`,dt$`Debito Residuo`)
            
            
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
                                  caption = tags$caption("REGIME DI INTERESSI SEMPLICE"),
                                  rownames = F,
                                  callback = JS("$('div.dwnld').append($('#download1'));"),
                                  extensions = 'Buttons',
                                  options = list(orderClasses = TRUE,
                                                 scrollCollapse = T,
                                                 pageLength = 20,
                                                 dom = 'B<"dwnld">frtip',
                                                 buttons = list('copy')
                                                 )) %>%
                        formatCurrency(c('Quota Capitale', 'Debito Residuo', 'Quota Interessi', 'Rata'),'\U20AC')%>% 
                        formatPercentage('Tasso Infra%',2) %>%
                        formatStyle('Numero Rata', color = 'red',backgroundColor = 'teal',fontWeight = 'bold',textAlign = 'center')
                    
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
                                  caption = tags$caption("REGIME DI INTERESSI COMPOSTO"),
                                  rownames = F,
                                  extensions = 'Buttons',
                                  options = list(orderClasses = TRUE,
                                                 scrollCollapse = T,
                                                 pageLength = 20)) %>%
                        formatCurrency(c('Quota Capitale', 'Debito Residuo', 'Quota Interessi', 'Rata'),'\U20AC')%>% 
                        formatPercentage('Tasso Infra%',2) %>%
                        formatStyle('Numero Rata', color = 'red',backgroundColor = 'teal',fontWeight = 'bold',textAlign = 'center')
                    
                }
                
                else if (input$selection == "Ammortamento alla Francese" & input$selectionRegime == "Regime Interesse Semplice"){
                    
                    TassoAnnuale = (input$tassofr)/100
                    entita_fin = input$entita_finanziamento
                    PAfr = as.numeric(input$PAfr)
                    num_ann = input$ANNIfr
                    TassoInfra = TassoAnnuale * PAfr
                    num_rate = (PAfr)^-1 * num_ann
                    rata = entita_fin / ((1 - (1+TassoInfra)^-num_ann)/TassoInfra)
                    
                    ## genero matrice che fillo con for loop
                    ## metodo inefficace
                    
                    mat = matrix(ncol = 4, nrow = num_rate +1)
                    mat[,2] = rata
                    mat[1,] = 0
                    mat[1] = entita_fin
                    for (i in seq(2,nrow(mat))) {
                        mat[i,3] = mat[i-1,1] * TassoInfra
                        mat[i,4] = mat[i,2] - mat[i,3]
                        mat[i,1] = mat[i-1,1] - mat[i,4]
                    }
                    nomi = c("Debito Residuo", "Rata"," Quota Interessi" ,"Quota Capitale")
                    colnames(mat) = nomi
                    mat = mat %>%
                        as_tibble() %>% 
                        mutate('Numero Rata' = 0:num_rate) %>% 
                        mutate('Tasso Infra%' = round(TassoInfra,4)) %>%  
                        mutate('Anno Corrente' = c(0,rep(1:num_ann,len = num_rate, each = (PAfr)^-1)))
                    
                    tab = tibble('Numero Rata' = 0:num_rate,
                                 'Tasso Infra%' = round(TassoInfra,4),
                                 'Anno Corrente' = c(0,rep(1:num_ann,len = num_rate, each = (PAfr)^-1)),
                                 'Quota Capitale' = mat$`Quota Capitale`,
                                 'Debito Residuo' = mat$`Debito Residuo`,
                                 'Quota Interessi' = mat$` Quota Interessi`,
                                 'Rata' = mat$Rata) 
                        
                   
                    DT::datatable(data = tab,
                                  filter = 'none',
                                  caption = tags$caption("REGIME INTERESSI SEMPLICE"),
                                  rownames = F,
                                  extensions = 'Buttons',
                                  options = list(orderClasses = TRUE,
                                                 scrollCollapse = T,
                                                 pageLength = 20))%>%
                        formatCurrency(c('Quota Capitale', 'Debito Residuo', 'Quota Interessi', 'Rata'),'\U20AC')%>% 
                        formatPercentage('Tasso Infra%',2) %>%
                        formatStyle('Numero Rata', color = 'red',backgroundColor = 'teal',fontWeight = 'bold',textAlign = 'center')
                    
                    
                    
                    
                }
                else if(input$selection == "Ammortamento alla Francese" & input$selectionRegime == "Regime Interesse Composto"){
                    
                    TassoAnnuale = (input$tassofr)/100
                    entita_fin = input$entita_finanziamento
                    PAfr = as.numeric(input$PAfr)
                    num_ann = input$ANNIfr
                    TassoInfra = (1 + TassoAnnuale)^PAfr -1
                    num_rate = (PAfr)^-1 * num_ann
                    rata = entita_fin / ((1 - (1+TassoInfra)^-num_ann)/TassoInfra)
                    
                    ## genero matrice che fillo con for loop
                    ## metodo inefficace
                    
                    mat = matrix(ncol = 4, nrow = num_rate +1)
                    mat[,2] = rata
                    mat[1,] = 0
                    mat[1] = entita_fin
                    for (i in seq(2,nrow(mat))) {
                        mat[i,3] = mat[i-1,1] * TassoInfra
                        mat[i,4] = mat[i,2] - mat[i,3]
                        mat[i,1] = mat[i-1,1] - mat[i,4]
                    }
                    nomi = c("Debito Residuo", "Rata"," Quota Interessi" ,"Quota Capitale")
                    colnames(mat) = nomi
                    mat = mat %>%
                        as_tibble() %>% 
                        mutate('Numero Rata' = 0:num_rate) %>% 
                        mutate('Tasso Infra%' = round(TassoInfra,4)) %>%  
                        mutate('Anno Corrente' = c(0,rep(1:num_ann,len = num_rate, each = (PAfr)^-1))) 
                    
                    tab = tibble('Numero Rata' = 0:num_rate,
                                 'Tasso Infra%' = round(TassoInfra,4),
                                 'Anno Corrente' = c(0,rep(1:num_ann,len = num_rate, each = (PAfr)^-1)),
                                 'Quota Capitale' = mat$`Quota Capitale`,
                                 'Debito Residuo' = mat$`Debito Residuo`,
                                 'Quota Interessi' = mat$` Quota Interessi`,
                                 'Rata' = mat$Rata)
                    
                    DT::datatable(data = tab,
                                  filter = 'none',
                                  caption = tags$caption("REGIME INTERESSI COMPOSTO"),
                                  rownames = F,
                                  extensions = 'Buttons',
                                  options = list(orderClasses = TRUE,
                                                 scrollCollapse = T,
                                                 pageLength = 20
                                                 ))%>%
                        formatCurrency(c('Quota Capitale', 'Debito Residuo', 'Quota Interessi', 'Rata'),'\U20AC')%>% 
                        formatPercentage('Tasso Infra%',2) %>%
                        formatStyle('Numero Rata', color = 'red',backgroundColor = 'teal',fontWeight = 'bold',textAlign = 'center')
                    
                    
                    
                }
            })

            
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
                    
                    plot_ly(AllAnno, 
                            x = ~Data,
                            y = ~Tasso) %>%
                        add_lines()
            })
        
        })
        
    })

})
