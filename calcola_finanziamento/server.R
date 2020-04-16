# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    
    REF1 = a("OsservatorioS24O", href="https://mutuionline.24oreborsaonline.ilsole24ore.com/guide-mutui/euribor.asp")
    euro = dollar_format(prefix = "", suffix = "\u20ac")
        
        
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
                    rata = entita_fin / (((1 - (1+TassoInfra)^-num_rate))/TassoInfra)
                    
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
                    rata = entita_fin / (((1 - (1+TassoInfra)^-num_rate))/TassoInfra)
                    
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
        
    })
