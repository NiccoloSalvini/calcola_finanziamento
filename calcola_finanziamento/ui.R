library(shiny)
library(shinythemes)
library(ggplot2)
library(lubridate)
library(plotly)
library(DT)
library(readxl)
library(rvest)
library(stringr)
library(furrr)
library(purrr)
library(magrittr)
library(shinyWidgets)
library(tidyverse)
library(shinydashboard)
library(vroom)
library(janitor)
library(purrr)
library(scales)




    # Define UI for application that draws a histogram
shinyUI(
    fluidPage(theme = shinytheme("cerulean"),
              titlePanel(title=div(img(src="https://www.facile.it/img_a/zuzu/bianche/soldi_big.png",
                                       height = '15%',
                                       width = '15%'),
                                   "Ti Spiego il Finanziamento"),
                         windowTitle = 'CalFin APP'),
              
              
#######################################################################################################
#######################################################################################################
#######################################################################################################
                                #SIDE BAR


    sidebarLayout(
            sidebarPanel(
                
                numericInput(inputId = "entita_finanziamento",
                             label = h3("Entità Finanziamento €"), 
                         value = 20000,
                         step = 100),
            
            selectInput("selection", 
                        label = h3("Che tipo di Ammortamento?"), 
                        choices = c("Ammortamento alla Italiana", "Ammortamento alla Francese")),
            
            selectInput("selectionRegime", 
                        label = h3("Che tipo di Regime?"), 
                        choices = c("Regime Interesse Semplice", "Regime Interesse Composto")),
            
            # condizionale all'ITALIANA
            conditionalPanel( 
                condition = "input.selection == 'Ammortamento alla Italiana'",
                helpText(em("> Ammortamento selezionato:"),strong("All'Italiana")),
                
                # parti anno per fin
                radioButtons("PAit",
                             label = h3("Parti d'Anno"),
                             choices = list("Mensile" = 1/12, "Trimestrale" = 1/4, 'Quadrimestrale' = 1/3,
                                            "Semestrale" = 1/2, "Annuale" = 1),
                             selected = 1),
                    
                # numero di anni che dura finanziamento
                numericInput("ANNIit",
                             label = h3("Durata in Anni"),
                             value = 10),
                
                # tasso che posso valutare di spostare fuori dal conditional 
                numericInput(inputId = "tassoit", 
                             label = h3("TAN % (Tasso di Interesse Annuo) "),
                             value = 1.55,
                             min = -1,
                             max = 10,
                             step = 0.01)
                ),
            
            # condizionale alla FRANCESE 
            conditionalPanel(
                condition = "input.selection == 'Ammortamento alla Francese'",
                helpText("> L'ammortamento alla Francese prevede
                         rate costanti <"),
                
                radioButtons("PAfr",
                             label = h3("Parti d'Anno"),
                             choices = list("Mensile" =1/12, "Trimestrale" = 1/4,'Quadrimestrale' = 1/3, 
                                            "Semestrale" = 1/2, "Annuale" = 1),
                             selected = 1),
                
                numericInput("ANNIfr",
                             label = h3("Durata in Anni"),
                             value = 10),
                
                
                numericInput(inputId = "tassofr",
                             label = h3("TAN %(Tasso di Interesse Annuo) "), 
                             value = 1.55,
                             min = -1,
                             max = 10,
                             step = 0.01)
                ),
            
            ),

            
#######################################################################################################
#######################################################################################################
#######################################################################################################
                                  #MAIN 
            
            
        # Show a plot of the generated distribution
        mainPanel(
                
                #   qui si crea la seconda tab e ci metto le informazioni discorsive
              
                
                #qui si crea la terza tab e ci metto le tabelle da esportare1
                tabPanel("Struttura", 
                         icon = icon('table'),
                         p(),
                         #single download type  
                         DT::dataTableOutput('Struttura'),
                         hr(), 
                         )
                # qui faccio vedere i tassi di confronto EURIBOR 
              
        )
    )
    )
    )
