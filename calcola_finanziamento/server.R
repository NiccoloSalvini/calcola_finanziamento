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
    
            # generate bins based on input$bins from ui.R
            x    <- faithful[, 2]
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
            # draw the histogram with the specified number of bins
            hist(x, breaks = bins, col = 'darkgray', border = 'gold')
            
            
            # qui renderizza la tabella sulla base degli input, prima lo faccio senza 
            # gli input dello user
            output$tabella = DT::renderDataTable({
                
                
                tasso = input$tassoit
                entita_fin = input$entita_finanziamento
                anni_rim = input$anniit
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
                              
                DT::datatable(data = dt, 
                              options = list(orderClasses = TRUE))
            })
            
            #qui metto la parte del download della struttura del prestito
            
            output$Download <- downloadHandler(
                filename = function() {
                    paste0(output$tabella, Sys.Date(), ".csv")
                },
                content = function(file) {
                    vroom::vroom_write(output$tabella, file)
                }
            )
            
        
        
        })
        
    })

})
