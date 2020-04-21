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
                      tabsetPanel(
                          
                          #   qui si crea la prima tab e ci metto i plots
                          tabPanel("Informazioni",
                                   icon = icon('info-circle'),
                                   fluidRow(column(width = 12,
                                                   valueBoxOutput("vbox", width = 2),
                                                   valueBoxOutput("vbox1", width = 2),
                                                   valueBoxOutput("vbox2", width = 2),
                                                   valueBoxOutput("vbox3", width = 2)
                                                   ),
                                            ),
                          ),
                          
                          #   qui si crea la seconda tab e ci metto le informazioni discorsive
                          tabPanel("Financial Math basis",
                                   icon = icon('square-root-alt'),
                                   hr(),
                                   # qui con MathJAx butto giù le formule di tutti e due gli ammortamenti
                                   
                                   #### qui all'ITALIANA in regime SEMPLICE
                                   p(h2(strong("All'ITALIANA"))),
                                   p(h2(strong("In regime Semplice"))),
                                   helpText("L'ammortamento con quote capitali costanti (ammortamento italiano) 
                                  prevede che ciascuna quota di ammortamento (supposto che le rate siano equintervallate ed n sia il numero di periodi previsti per l'ammortamento) 
                                  sia costante e pagata in via posticipata."),
                                   withMathJax(),
                                   helpText('$$EF = Entità Finaziamento$$'),
                                   helpText('$$A = NumeroAnni$$'),
                                   helpText('$$PA = PartiAnno$$'),
                                   helpText('$$TAN = Tasso Interesse Annuale$$'),
                                   helpText('$$TassoInfra = IntAnn \\cdot PA^{-1} $$'),
                                   helpText('$$Num Anni = NumeroDiAnniFinanziamento$$'),
                                   helpText('$$Num Rat = Num Anni \\cdot PA$$'),
                                   helpText('$$Quota Capitale =  \\frac{EF}{Num Rat}$$'),
                                   helpText('$$QuotaInteressi_{t} =  QuotaCapitale_{t-1}\\cdot Tasso Infra$$'),
                                   helpText('$$Debito Residuo_{t} =  EF - \\sum_{i=1}^t{QuotaCapitale_{t-1}}$$'),
                                   helpText('$$Rata_{t} = QuotaCapitale_{t} + Quota Interessi_{t}$$'),
                                   hr(),
                                   
                                   ### qui all'FRANCESE regime SEMPLICE
                                   p(h2(strong("Alla FRANCESE"))),
                                   p(h2(strong("In regime Semplice"))),
                                   helpText("L'ammortamento francese prevede che le rate siano posticipate e che la somma ricevuta dal debitore all'inizio (t = 0) sia il valore attuale di una rendita a rate costanti. Ciascuna rata è comprensiva di
                         parte del capitale (quota capitale) ed i relativi interessi (quota interessi) calcolati sul
                                  capitale residuo non ancora restituito (debito residuo). Tale metodo è alternativo ai metodi di calcolocon rata anticipata e ai metodi italiano e tedesco a quota capitale costante e rata variabile."),
                                   withMathJax(),
                                   helpText('$$EF = Entità Finaziamento$$'),
                                   helpText('$$A = NumeroAnni$$'),
                                   helpText('$$PA = PartiAnno$$'),
                                   helpText('$$TAN = TassoAnnualeNetto$$'),
                                   helpText('$$TassoInfra = TAN \\cdot PA$$'),
                                   helpText('$$Rata = \\frac{EF}{ \\frac{1 - (1+TassoInfra)^{-A}}{TassoInfra} }$$'),
                                   helpText('$$QuotaInteressi_{t} =  Debito Residuo_{t-1}\\cdot TassoInfra$$'),
                                   helpText('$$Quota Capitale_{t} = Rata  - QuotaInteressi_{t}$$'),
                                   helpText('$$Debito Residuo_{t} =  Debito Residuo_{t-1} - Quota Capitale_{t} $$'),
                                   hr(),
                                   
                                   ### qui ALL' ITALIANA in regime COMPOSTO 
                                   p(h2(strong("All'ITALIANA"))),
                                   p(h2(strong("In regime Composto"))),
                                   helpText("L'ammortamento con quote capitali costanti (ammortamento italiano) 
                                  prevede che ciascuna quota di ammortamento (supposto che le rate siano equintervallate ed n sia il numero di periodi previsti per l'ammortamento) 
                                  sia costante e pagata in via posticipata."),
                                   withMathJax(),
                                   helpText('$$EF = Entità Finaziamento$$'),
                                   helpText('$$A = NumeroAnni$$'),
                                   helpText('$$PA = PartiAnno$$'),
                                   helpText('$$IntAnn = Tasso Interesse Annuale$$'),
                                   helpText('$$TassoInfra = (1 + IntAnn)^{PA} -1  $$'),
                                   helpText('$$Num Anni = NumeroDiAnniFinanziamento$$'),
                                   helpText('$$Num Rat = Num Anni \\cdot PA^{-1}$$'),
                                   helpText('$$Quota Capitale =  \\frac{EA}{Num Rat}$$'),
                                   helpText('$$QuotaInteressi_{t} =  QuotaCapitale_{t-1}\\cdot Tasso Infra$$'),
                                   helpText('$$Debito Residuo_{t} =  EA - \\sum_{i=1}^t{QuotaCapitale_{t-1}}$$'),
                                   helpText('$$Rata_{t} = QuotaCapitale_{t} + Quota Interessi_{t}$$'),
                                   hr(),
                                   
                                   
                                   ### qui all'FRANCESE regime COMPOSTO
                                   p(h2(strong("Alla FRANCESE"))),
                                   p(h2(strong("In regime Composto"))),
                                   helpText("L'ammortamento francese prevede che le rate siano posticipate e che la somma ricevuta dal debitore all'inizio (t = 0) sia il valore attuale di una rendita a rate costanti. Ciascuna rata è comprensiva di
                         parte del capitale (quota capitale) ed i relativi interessi (quota interessi) calcolati sul
                                  capitale residuo non ancora restituito (debito residuo). Tale metodo è alternativo ai metodi di calcolocon rata anticipata e ai metodi italiano e tedesco a quota capitale costante e rata variabile."),
                                   withMathJax(),
                                   helpText('$$EF = Entità Finaziamento$$'),
                                   helpText('$$A = NumeroAnni$$'),
                                   helpText('$$PA = PartiAnno$$'),
                                   helpText('$$TAN = TassoAnnualeNetto$$'),
                                   helpText('$$TassoInfra = (1 + IntAnn)^{PA} -1 $$'),
                                   helpText('$$Rata = \\frac{EF}{ \\frac{1 - (1+TassoInfra)^{-A}}{TassoInfra} }$$'),
                                   helpText('$$QuotaInteressi_{t} =  Debito Residuo_{t-1}\\cdot TassoInfra$$'),
                                   helpText('$$Quota Capitale_{t} = Rata  - QuotaInteressi_{t}$$'),
                                   helpText('$$Debito Residuo_{t} =  Debito Residuo_{t-1} - Quota Capitale_{t} $$'),
                                   hr(),
                                   
                                   
                                   
                                   ### qui ci metto gli indicatori in una sorta di bibliografia
                                   p(h2(strong("TAE"))),
                                   p(h2(strong("In tuttir e due i regimi"))),
                                   helpText('La lezione è che non si possono confrontare direttamente tassi di interesse
                                            con diversi regimi di capitalizzazione, anche se relativi alla stessa scadenzaTassoAnnuo effettivo Inoltre, 
                                            da quest’ultimo esempio si vede chiaramente che il metodo
                                              dell’attualizzazione dei flussi di cassa ipotizza implicitamente il
                                              reinvestimento dei flussi di cassa intermedi alle stesse condizioni
                                              contrattuali di remunerazione … altrimenti non avrebbe alcun valore
                                              percepire un flusso finanziario prima della scadenza. E’ necessario quindi
                                              individuare un TAE che renda confrontabili i diversi regimi di
                                              capitalizzazione'),
                                   helpText("dato che PA sta tra 0 e 1, visto che è definito come il rapporto tra numero delle rate all'anno 
                                            diviso 12"),                                  
                                   helpText('$$TAE = ( 1 +  TAN \\cdot PA )^{PA^{-1}} -1$$'),
                                   

                                   
                          ),
                          
                          #qui si crea la terza tab e ci metto le tabelle da esportare1
                          tabPanel("Struttura", 
                                   icon = icon('table'),
                                   p(),
                                   #single download type  
                                   DT::dataTableOutput('Struttura'),
                                   hr(), 
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
                                   # qui dentro plotly
                                   selectizeInput(
                                       inputId = "tuttitassi", 
                                       label = h1("Seleziona Serie Storica Tasso"),
                                       choices = c('AlGiorno', 'AlMese', 'AllAnno')
                                   ),
                                   plotlyOutput(outputId = "p"),
                                   hr(),
                          ),
                          
                          # qui metto i plot relativi alla struttura
                          tabPanel("Plot", 
                                   icon = icon('drafting-compass'),
                                   p(),
                                   plotlyOutput(outputId = "Plot"),
                                   hr(),
                                   )
                      )
                  )
              )
    )
)
