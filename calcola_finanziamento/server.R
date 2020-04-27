shinyServer(function(input, output, session) {
    
    REF1 = a("OsservatorioS24O", href="https://mutuionline.24oreborsaonline.ilsole24ore.com/guide-mutui/euribor.asp")
    euro = dollar_format(prefix = "", suffix = "\u20ac")
        
        
        # qui renderizza la parte Dataset con tutte le combinazioni
        # di possibili finanziameneti e tassi
    dataInput = reactive({
        
        ############################################################################
        ############################################################################
        ############################################################################
        # alla italiana
        
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
            # cover up the superfluous
            rata[1] = 0
            QC = rep(quota_capitale,num_rate+1)
            QC[1] = 0
            # other calculations that I will use after 
            totale_interessi = sum(quota_interessi)
            totale_rata = sum(rata)
            
            dt = tibble('Numero Rata' = 0:num_rate,
                        'Tasso Infra%' = rep(TassoInfra,num_rate +1),
                        'Anno Corrente' = c(0,rep(1:num_ann,len = num_rate, each = (PAit)^-1)),
                        'Quota Capitale' = QC,
                        'Debito Residuo' = debito_residuo,
                        'Quota Interessi'= quota_interessi[-length(quota_interessi)],
                        'Rata' = rata)
            
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
            # cover up the superfluous
            rata[1] = 0
            QC = rep(quota_capitale,num_rate+1)
            QC[1] = 0
            # other calculations that I will use after
            totale_interessi = sum(quota_interessi)
            totale_rata = sum(rata)
            
            dt = tibble('Numero Rata' = 0:num_rate,
                        'Tasso Infra%' = rep(TassoInfra,num_rate +1),
                        'Anno Corrente' = c(0,rep(1:num_ann,len = num_rate, each = (PAit)^-1 )),
                        'Quota Capitale' = QC,
                        'Debito Residuo' = debito_residuo,
                        'Quota Interessi'= quota_interessi[-length(quota_interessi)],
                        'Rata' = rata)
            
            
        }
        ############################################################################
        ############################################################################
        ############################################################################
        # alla francese
        
        
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
            
            
        }
        
    })
    
    ####################
    ####################
    ####################
    ####################
    # qui parte per dash iniziale
    
    dashInput = reactive({
      
      ############################################################################
      ############################################################################
      ############################################################################
      # alla italiana
      
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
        # cover up the superfluous
        rata[1] = 0
        QC = rep(quota_capitale,num_rate+1)
        QC[1] = 0
        # other calculations that I will use after 
        totale_interessi = sum(quota_interessi)
        totale_rata = sum(rata)
        TAE = TassoInfra
          
  
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
        # cover up the superfluous
        rata[1] = 0
        QC = rep(quota_capitale,num_rate+1)
        QC[1] = 0
        # other calculations that I will use after
        totale_interessi = sum(quota_interessi)
        totale_rata = sum(rata)
        TAE = (1 + TassoAnnuale*PAit)^(PAit^-1) -1
        
      }
      ############################################################################
      ############################################################################
      ############################################################################
      # alla francese
      
      
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
        TAE = TassoInfra

        
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
        TAE = (1 + TassoAnnuale*PAfr)^(PAfr^-1) -1
        
        
        
      }
      
    })
    
    url1 = 'https://www.euribor-rates.eu/it/tassi-euribor-aggiornati/2/euribor-tasso-3-mesi/'
    css = '.col-lg-4 .card-body'
    Names = c('Data','Tasso')
    
    # prendo tabellone con tutti e tre dataset  
    tabellone = url1 %>%
      read_html() %>% 
      html_table()
    
    tassiInputGiorno = reactive({
  
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
        
        AlGiorno
    })
    
    tassiInputMese = reactive({
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
      AlMese
    })
    
    tassiInputAnno = reactive({
      
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
      
      AllAnno
      # non rieco ad implementare i diversi data set
      # posso provare ad avere un unico dataset e poi 
      # filtrare per un determinato input
      
      
    })
    
    
    output$Struttura = DT::renderDataTable({
        DT::datatable(data = dataInput(),
                      filter = 'none',
                      caption = tags$caption("REGIME DI INTERESSI SEMPLICE"),
                      rownames = F,
                      callback = JS("$('div.dwnld').append($('#download1'));"),
                      extensions = 'Buttons',
                      options = list(orderClasses = TRUE,
                                     scrollCollapse = T,
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#222222', 'color': '#fff'});",
                                       "}"),
                                     pageLength = 20,
                                     dom = 'B<"dwnld">frtip',
                                     buttons = list('copy'))) %>%
            formatCurrency(c('Quota Capitale', 'Debito Residuo', 'Quota Interessi', 'Rata'),'\U20AC', digits =  2)%>%
            formatPercentage('Tasso Infra%',2) %>%
            formatStyle('Numero Rata', color = 'red',backgroundColor = 'teal',fontWeight = 'bold',textAlign = 'center')
    })
    
    
        q
        output$vbox = renderValueBox({

            valueBox(
                value = tags$p(euro(sum(dataInput()$'Quota Interessi')), style = "font-size: 150%;"),
                subtitle = tags$p("Totale interessi richiesti", style = "font-size: 150%;"),
                icon = icon("info")
                )
          })
        
        output$vbox1 = renderValueBox({
            
            valueBox(
                value = tags$p(euro(dataInput()$'Rata'[2]), style = "font-size: 150%;"),
                subtitle = tags$p("Totale Rata da Pagare", style = "font-size: 150%;"),
                icon = icon("info")
            )
          })
        
        output$vbox2 = renderValueBox({
            
            valueBox(
              value = tags$p(dataInput()$'Numero Rata'[length(dataInput()$'Numero Rata')], style = "font-size: 150%;"),
              subtitle = tags$p("Totale Numero delle Rate", style = "font-size: 150%;"),
              icon = icon("money")
            )
          })
        
        output$vbox3 = renderValueBox({
            
          valueBox(
         
            value = tags$p(percent(dashInput()), style = "font-size: 150%;"),
            subtitle = tags$p("TAE ", style = "font-size: 150%;"),
            icon = icon("money")
          )
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
                              options = list(orderClasses = TRUE,
                                             initComplete = JS(
                                               "function(settings, json) {",
                                               "$(this.api().table().header()).css({'background-color': '#222222', 'color': '#fff'});",
                                               "}")))
                
                    })
            
            
            output$tab = renderUI({
                tagList("Here It is the source:", REF1)
            })
            
            
            
            
            output$p = renderPlotly({
              #qui al giorno
              if(input$tuttitassi == 'AlGiorno'){
                plot_ly(tassiInputGiorno(),
                        x = ~Data,
                        y = ~Tasso) %>% 
                  add_lines()
              }
              
              else if (input$tuttitassi == 'AlMese'){
                plot_ly(tassiInputMese(),
                        x = ~Data,
                        y = ~Tasso) %>% 
                  add_lines()
              }
              
              else{
                plot_ly(tassiInputAnno(),
                        x = ~Data,
                        y = ~Tasso) %>% 
                  add_lines()  
              }
              
            })
            
            
            output$Plot = renderPlotly({
              #qui al giorno
              quota = dataInput()$'Quota Interessi'
              totale = sum(dataInput()$'Quota Interessi')
              rapporto = quota/totale
              rat = dataInput()$'Numero Rata'
              tib = tibble('Rapporto' =  rapporto,
                           'Rata' = rat)
                plot_ly(tib[-1,],
                        x = ~ Rata,
                        y = ~ Rapporto) %>% 
                  add_lines()
             
              
            })

})
