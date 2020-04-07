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
        output$distPlot <- renderPlot({
    
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
            
            
            # qui renderizza la tabella sulla base degli input, prima lo faccio senza 
            # gli input dello user
            output$Dataset = DT::renderDataTable({
                
                
                tasso = input$tassoit
                entita_fin = input$entita_finanziamento
                PAit = as.numeric(input$PAit)
                num_ann = input$ANNIit
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
            })
            
            #qui metto la parte del download della struttura del prestito
            
            output$Download <- downloadHandler(
                filename = function() {
                    paste0(output$Dataset, Sys.Date(), ".csv")
                },
                content = function(file) {
                    vroom::vroom_write(output$Dataset, file)
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
        
        })
        
    })

})
