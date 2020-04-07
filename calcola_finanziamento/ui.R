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




# Define UI for application that draws a histogram
shinyUI(
    fluidPage(theme = shinytheme("cerulean"),
              titlePanel(title=div(img(src="https://www.facile.it/img_a/zuzu/bianche/soldi_big.png",
                                       height = '15%',
                                       width = '15%'),
                                   "Ti Spiego il Finanziamento NICCO")),

    sidebarLayout(
        sidebarPanel(
            
            numericInput(inputId = "entita_finanziamento",
                         label = h3("Entità finanziamento"), 
                         value = 20000),
            
            selectInput("selection", 
                        label = h3("Che tipo di Ammortamento?"), 
                        choices = c("Ammortamento alla Italiana", "Ammortamento alla Francese")),
            
            # condizionale all'ITALIANA
            conditionalPanel( 
                condition = "input.selection == 'Ammortamento alla Italiana'",
                helpText(strong("> L'ammortamento alla Italiana prevede quote capitali costanti <")),
                
                # parti anno per fin
                radioButtons("PAit",
                             label = h3("Parti d'Anno"),
                             choices = list("Mensile" = 12, "Trimestrale" = 4, 'Quadrimestrale' = 3,
                                            "Semestrale" = 2, "Annuale" = 1),
                             selected = 1),
                
                # numero di anni che dura finanziamento
                numericInput("ANNIit",
                             label = h3("N° Anni"),
                             value = 10),
                
                # tasso che posso valutare di spostare fuori dal conditional 
                numericInput(inputId = "tassoit", 
                             label = h3("Tasso di Interesse"),
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
                             choices = list("Mensile" = 12, "Trimestrale" = 4,'Quadrimestrale' = 3, 
                                            "Semestrale" = 2, "Annuale" = 1),
                             selected = 1),
                
                numericInput("ANNIfr",
                             label = h3("N° Anni"),
                             value = 1),
                
                
                numericInput(inputId = "tassofr", label = h3("Tasso di Interesse"), 
                             value = 1.55,
                             min = -1,
                             max = 10,
                             step = 0.01)
                ),
            
            ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                #qui si crea la prima tab e ci metto i plots
                tabPanel("Plot",
                         plotOutput("distPlot"),
                         icon = icon('drafting-compass')
                         ),
                
                #qui si crea la seconda tab e ci metto le informazioni discorsive
                tabPanel("Financial Math basis",
                         icon = icon('square-root-alt'),
                         verbatimTextOutput("square-root-alt"),
                         hr(),
                         # qui con MathJAx butto giù le formule di tutti e due gli ammortamenti
                         # qui all'ITALIANA
                         p(h2(strong("All'ITALIANA")), style = 'color:blue'),
                         helpText("L'ammortamento con quote capitali costanti (ammortamento italiano) 
                                  prevede che ciascuna quota di ammortamento (supposto che le rate siano equintervallate ed n sia il numero di periodi previsti per l'ammortamento) 
                                  sia costante e pagata in via posticipata."),
                         withMathJax(),
                         helpText('$$A = NumeroAnni$$'),
                         helpText('$$PA = PartiAnno$$'),
                         helpText('$$Numero di Rate = PartiAnno \\cdot NumeroAnni$$'),
                         helpText('$$Quota Capitale =  \\frac{Entità Finanziamento}{Numero di Rate}$$'),
                         helpText('$$QuotaInteressi_{t} =  QuotaCapitale_{t-1}\\cdot Tasso$$'),
                         helpText('$$Debito Residuo_{t} =  EntitàFinanziamento - \\sum_{i=1}^t{QuotaCapitale_{t-1}}$$'),
                         helpText('$$Rata_{t} = QuotaCapitale_{t} + Quota Interessi_{t}$$'),
                         hr(),
                         
                         # qui all'FRANCESE 
                         p(h2(strong("Alla FRANCESE")), style = 'color:red'),
                         helpText("L'ammortamento francese prevede che le rate siano posticipate e che la somma ricevuta dal debitore all'inizio (t = 0) sia il valore attuale di una rendita a rate costanti. Ciascuna rata è comprensiva di
                         parte del capitale (quota capitale) ed i relativi interessi (quota interessi) calcolati sul
                                  capitale residuo non ancora restituito (debito residuo). Tale metodo è alternativo ai metodi di calcolocon rata anticipata e ai metodi italiano e tedesco a quota capitale costante e rata variabile."),
                         withMathJax(),
                         helpText('$$A = NumeroAnni$$'),
                         helpText('$$PA = PartiAnno$$'),
                         helpText('$$TAN = tasso \\cdot PA$$'),
                         helpText('$$Rata = Entità Finanziamento\\cdot (1 + \\frac{TAN} {PA})^{PA \\cdot A}  \\cdot \\frac{\\frac{TAN}{PA}}{(1 + \\frac{TAN} {PA})^{PA \\cdot A} -1}$$'),
                         helpText('$$Quota Capitale_{t} = QuotaInteressi_{t} + Rata_{t}$$'),
                         helpText('$$QuotaInteressi_{t} =  Debito Residuo_{t-1}\\cdot Tasso$$'),
                         helpText('$$Debito Residuo_{t} =  EntitàFinanziamento - \\sum_{i=1}^t{QuotaCapitale_{t-1}}$$'),
                         hr(),
                         
                         ),
                
                #qui si crea la terza tab e ci metto le tabelle da esportare1
                tabPanel("Dataset", 
                         icon = icon('table'),
                         p(em('Term structure of the financial operation
                              base on the', strong('left side'), em('inputs'))),
                         hr(),
                         DT::dataTableOutput('Dataset'),
                         hr(),
                         downloadButton(outputId ="Download",
                                        label = "Download .csv",
                                        class = "btn-secondary")
                         ),
                # qui faccio vedere i tassi di confronto EURIBOR 
                tabPanel("Tassi", 
                         icon = icon('sort-numeric-down'),
                         p(h1(strong("EURIBOR"))),
                         p(em("E' Tasso interbancario di riferimento diffuso giornalmente dalla Federazione Bancaria Europea come media ponderata dei
                              tassi di interesse ai quali le Banche operanti nell'Unione Europea cedono i depositi in prestito.
                              È utilizzato come parametro di indicizzazione dei mutui ipotecari a tasso variabile.")),
                         strong('Questa tabella è aggiornata dinamicamente con cadenza giornaliera'),
                         em('Ultimo aggioramento alla data:', Sys.Date()),
                         p(uiOutput("tab")),
                         hr(),
                         DT::dataTableOutput('Tassi'),
                         hr(),
                         helpText("Qui vedi l'evoluzione del tasso")
                        )
        )
    )
    )
    )
    )
