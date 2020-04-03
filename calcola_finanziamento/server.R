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
shinyServer(function(input, output) {
# qui renderizza l'output del radio buttom sotto il plot
    output$cazzo = renderPrint({ input$radio })
    
# qui renderizza il plot della distributizone    

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'gold')
        
        # radio buttons
        output$value <- renderPrint({ input$radio })
    
# qui renderizza la tabella sulla base degli input, prima lo faccio senza 
# gli input dello user
        
        
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
                      n_rata = 0:numero_rata+1,
                      quota_capitale = rep(quota_capitale,9),
                      debito_residuo = debito_residuo,
                      quota_interessi= quota_interessi[-length(quota_interessi)],
                      rata = rata[-length(rata)]
        )
        
        output$tabella = DT::renderDataTable({
            DT::datatable(data = data, options = list(orderClasses = TRUE))
        })
            
        
        
        
        
    })

})
