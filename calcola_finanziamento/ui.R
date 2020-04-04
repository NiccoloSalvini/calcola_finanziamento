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


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"),

    # Application title
    titlePanel("Ti Spiego il Finanziamento APP"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            helpText("Riempi le caselle coi principali dati del finanziamento"),
            numericInput(inputId = "entita_finanziamento", label = h3("Entità del finanziamento"), value = 20000),
            
            selectInput("selection", h3("Che tipo di Ammortamento?"), 
                        choices = c("Ammortamento alla Italiana", "Ammortamento alla Francese")),
            
            conditionalPanel(
                condition = "input.selection == 'Ammortamento alla Italiana'",
                helpText("> L'ammortamento alla Italiana prevede
                         quote capitali costanti <"),
                
                    
                numericInput(inputId = "anniit", label = "Anni rimborso", value = 10),
                radioButtons("rateit", label = h3("Tipo di Rate"),
                             choices = list("Mensile" = 1, "Trimestrale" = 2, "Semestrale" = 3, "Annuale" = 4),
                             selected = 1),
                    numericInput(inputId = "tassoit", label = h3("Tasso di Interesse"),
                                 value = 1.55,
                                 min = -1,
                                 max = 10,
                                 step = 0.01)
                ),
                
            conditionalPanel(
                condition = "input.selection == 'Ammortamento alla Francese'",
                helpText("> L'ammortamento alla Francese prevede
                         rate costanti <"),
                
                numericInput(inputId = "annifr", label = "Anni rimborso", value = 10),
                radioButtons("ratefr", label = h3("Tipo di Rate"),
                             choices = list("Mensile" = 1, "Trimestrale" = 2, "Semestrale" = 3, "Annuale" = 4),
                             selected = 1),
                numericInput(inputId = "tassofr", label = h3("Tasso di Interesse"), 
                             value = 1.55,
                             min = -1,
                             max = 10,
                             step = 0.01)
                ),
            
            
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            hr(),
            hr()
            #fluidRow(column(3, verbatimTextOutput("value")))
            ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                #qui si crea la prima tab e ci metto i plots
                tabPanel("Plot",
                         plotOutput("distPlot"),
                         fluidRow(column(3, verbatimTextOutput("value")))
                         ),
                #qui si crea la seconda tab e ci metto le informazioni discorsive
                tabPanel("Summary",
                         verbatimTextOutput("summary"),
                         p("p creates a paragraph of text."),
                         p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
                         strong("strong() makes bold text."),
                         em("em() creates italicized (i.e, emphasized) text."),
                         br(),
                         code("code displays your text similar to computer code"),
                         div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
                         br(),
                         p("span does the same thing as div, but it works with",
                           span("groups of words", style = "color:blue"),
                           "that appear inside a paragraph."),
                         p('some summary mathematics information about what is behind this',
                           span("here it goes l'ammortamento all'italiana preve quote capitali costanti")),
                         withMathJax(),
                         helpText('$$Quota Capitale =  \\frac{Entità Finanziamento}{Numero di Rate}$$'),
                         helpText('$$QuotaInteressi_{t} =  QuotaCapitale_{t-1}\\cdot Tasso$$'),
                         helpText('$$Debito Residuo_{t} =  EntitàFinanziamento - \\sum_{i=1}^t{QuotaCapitale_{t-1}}$$'),
                         helpText('$$Rata_{t} = QuotaCapitale_{t} + Quota Interessi_{t}$$')
                         ),
                #qui si crea la terza tab e ci metto le tabelle da esportare
                tabPanel("Table", 
                         tableOutput("table"),
                         p("here I am rendering the data table"),
                         strong('on a left input basis'),
                         hr(),
                         DT::dataTableOutput('tabella')
                         )
            )
        )
    )
))
