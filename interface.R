library (shiny)

MESES = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

plotSerie <- function (input) {
  serie <- read.csv2 (input$file$datapath,
                      header = input$header,
                      sep = input$sep,
                      dec = input$dec)
  serie <- serie[, -1]
  serie = matrix (serie, ncol = 12, byrow = T)
  par (lwd = 1, col = 'black')
  plot (NA, main = "Serie Historica", xlim = c (1,12), ylim = c (0, max (serie)), xlab = "", ylab = "Vazoes mensais (m^3/s)", axes = F, type = "n")
  axis (1, 1:12, MESES)
  box ( )
  par (col= 'gray70', pch = 16)
  for (ano in (1: (length (serie) / 12))) {
    for (mes in (1:12))
      points (mes, serie[ano, mes])
  }
  media = apply (serie, 2, mean)
  desvio = apply (serie, 2, sd)
  par (lty = 1)
  par (lwd = 2, lty = 1, col= 'cornflowerblue')
  lines (1:12, media)
  par (lty = 2)
  lines (1:12, media + desvio)
  lines (1:12, media - desvio)
}

ui <- navbarPage ("PMIX (p,q,P,Q)",
                  tabPanel ("Dados Historicos",
                    sidebarPanel (
                      fileInput ("file", "Serie Historica hidrologica de vazoes",
                                 multiple = FALSE,
                                 accept = c ("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                      tags$hr ( ),
                      checkboxInput ("header", "Header", TRUE),
                      selectInput ("sep", label = "Separador de colunas", 
                                   choices = list ("Ponto" = '.', "Virgula" = ",", "Ponto e virgula" = ";", "Tabulacao" = "\t"), 
                                   selected = ";"),
                      selectInput ("dec", label = "Separador decimal", 
                                   choices = list ("Ponto" = '.', "Virgula" = ","), 
                                   selected = ',')
                    ),
                  mainPanel (plotOutput ("dados"))
                  ),
                  
                  tabPanel ("Algoritmo",
                    fluidRow (
                      column (width = 1,
                        numericInput ("p", label = "p", value = 0, min = 0, max = 12),
                        numericInput ("q", label = "q", value = 0, min = 0, max = 12),
                        numericInput ("P", label = "P", value = 0, min = 0, max = 12),
                        numericInput ("Q", label = "Q", value = 0, min = 0, max = 12)
                      ),
                      column (width = 4,
                              radioButtons ("modelo", label = "Estimacao de parametros",
                                choices = list ("Metodo de Powell" = 1,
                                                "Algoritmo Genetico Puro" = 2,
                                                "Algoritmo Genetico com semente" = 3,
                                                "Algoritmo Genetico com data mining" = 4), 
                                          selected = 1),
                              checkboxInput ("novoPowell", "Gerar Powell", TRUE)
                      ),
                      column (width = 2,
                              checkboxInput ("lagSignificativo", "Lag Significativo", TRUE),
                              numericInput ("lagAnual", label = "lag Anual", value = 1, min = 1, max = 12),
                              numericInput ("lagMensal", label = "lag Mensal", value = 1, min = 1, max = 12)
                      )
                    ),
                    tags$hr ( ),
                    fluidRow (
                      column (width = 4,
                              sliderInput ("nPop", label = "Tamanho da populacao", min = 10, max = 100, value = 50),
                              sliderInput ("nsint", label = "Tamanho da serie sintetica", min = 0, max = 50000, value = 10000),
                              numericInput ("pC", label = "Probabilidade de cruzamento", value = 80, min = 0, max = 100),
                              numericInput ("pM", label = "Probabilidade de mutacao", value = 5, min = 0, max = 100)
                      ),
                      column (width = 4,
                              numericInput ("MAPEdiferencaMAX", label = "MAPEdiferencaMAX", value = 5, min = 0, max = 100),
                              numericInput ("MAPEavaliacao", label = "MAPEavaliacao", value = 20, min = 0, max = 100)
                      )
                    ),
                    tags$hr ( ),
                    fluidRow (
                      column (width = 4, checkboxGroupInput ("saidas", label = "Resultados", 
                                                     choices = list ("Calcular Media" = 1,
                                                                     "Calcular Desvio-padrao" = 2,
                                                                     "Calcular Autocorrelacao anual" = 3,
                                                                     "Calcular Autocorrelacao mensal" = 4,
                                                                     "Calcular Somatorio dos Residuos" = 5))
                      ),
                      column (width = 4,
                              checkboxInput ("volume", "Gerar Volume", TRUE),
                              checkboxInput ("hurst", "Gerar Hurst", TRUE)
                      )
                    )
                  ),
                  tabPanel ("Resultados",
                    verbatimTextOutput ("resultados1"),
                    verbatimTextOutput ("resultados2")
                  )
        )

server <- function (input, output) {
  
  output$dados <- renderPlot ({
    req (input$file)
    if (is.null (input$file))
      return (NULL)
    
    plotSerie (input)
  })
  
  output$resultados1 <- renderText ({
    req (input$file)
    if (is.null (input$file))
      return (NULL)
    
    lags <- c (input$p, input$q, input$P, input$Q)
    dados <- input$file
    lags
    lags
    source ('algoritmo.R')
    #NSGA (dados, lags, 1)
  })
  output$resultados1 <- renderPrint ({
    req (input$file)
    if (is.null (input$file))
      return (NULL)
    
    lags <- c (input$p, input$q, input$P, input$Q)
    dados <- input$file
    lags
    lags
    #source ('algoritmo.R')
    #NSGA (dados, lags, 1)
  })
  output$resultados2 <- renderPrint ({
    req (input$file)
    if (is.null (input$file))
      return (NULL)
    
    lags <- c (input$p, input$q, input$P, input$Q)
    dados <- input$file
    
    #isolate (source ('algoritmo.R'))
    #NSGA (dados, lags, 1)
  })
}

shinyApp (ui, server)