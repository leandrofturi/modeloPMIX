source ('algoritmo/algoritmo.R')

if (! ("shiny" %in% rownames (installed.packages ( ))))
  install.packages ("shiny")
if (! ("xlsx" %in% rownames (installed.packages ( ))))
  install.packages ("xlsx")
library (shiny)
library (xlsx)

  
algoritmo = function (input) {
  lags <- c (input$p, input$q, input$P, input$Q)
  dados <- input$file$datapath
  dadosPowell <- input$parametrosIniciais$datapath
  nPOPULACAO <<- input$nPop
  cicloMAX <<- input$cicloMax
  MAPEdiferencaMAX <<- (input$MAPEdiferencaMAX) / 100
  MAPEavaliacao <<- (input$MAPEavaliacao) / 100
  
  nSINTETICA <<- input$nsint
  probCRUZAMENTO <<- (input$pC) / 100
  probMUTACAO <<- (input$pM) / 100
  
  lagSIGNIFICATIVO <<- input$lagSignificativo
  lagANUAL <<- input$lagAnual
  lagMENSAL <<- input$lagMensal
  
  gerarPOWELL <<- input$novoPowell
  
  estacao = substr (dados, start = 1, stop = (nchar (dados)-4))
  ordem = paste0 ("PMIX (", lags[1], ",", lags[2], ",", lags[3], ",", lags[4], ")")
  
  if (input$modelo == 1)
    arquivos = PMIX (dados, lags)
  else if (input$modelo == 2)
    arquivos = NSGA (dados, lags, dadosPowell)
  else if (input$modelo == 3)
    arquivos = NSGA (dados, lags, dadosPowell)
  #else if (input$modelo == 4)
    #NSGAagrupado (dados, lags, 0)
    
  p = 1:(length (arquivos) - 1)
  #criar as celulas
  
  return (arquivos)
}

plotSerie <- function (input) {
  serie <- read.csv2 (input$file$datapath,
                      header = input$header,
                      sep = input$sep,
                      dec = input$dec)
  MESES = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
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
                        numericInput ("Q", label = "Q", value = 0, min = 0, max = 12),
                        checkboxInput ("multiLags", "Multiplos Lags", FALSE)
                      ),
                      column (width = 4,
                              radioButtons ("modelo", label = "Estimacao de parametros",
                                choices = list ("Metodo de Powell" = 1,
                                                "Algoritmo Genetico Puro" = 2,
                                                "Algoritmo Genetico com semente" = 3,
                                                "Algoritmo Genetico com data mining" = 4), 
                                          selected = 1),
                              checkboxInput ("novoPowell", "Gerar Powell", TRUE),
                              fileInput ("parametrosIniciais", "Parametros Iniciais gerados pelo Metodo de Powell",
                                                multiple = FALSE,
                                                accept = c ("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv"))
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
                              sliderInput ("cicloMax", label = "Ciclo Maximo", min = 0, max = 50000, value = 10000),
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
                            sidebarPanel (
                              actionButton ("iniciar", "Iniciar!"),
                              verbatimTextOutput("resultadoGeral"),
                              tags$hr ( ),
                              selectInput ("nSerie", "Serie a ser analisada:", choices = 1:50, selected = 50),
                              tags$hr ( ),
                              downloadButton ("downloadSerie", "Download")
                            ),
                            mainPanel (
                              tabsetPanel (
                                tabPanel("Tabela avaliacoes",
                                         dataTableOutput("tabelaAvaliacao")
                                ),
                                tabPanel("Graficos series",
                                         plotOutput("GraficoSerie")
                                ),
                                tabPanel("Graficos FAC anuais",
                                         plotOutput("FACAnuais")
                                ),
                                tabPanel("Graficos FAC mensais",
                                         selectInput ("lagMensalMAX", "lag mensal analisado:", choices = 1:12, selected = 1),
                                         plotOutput("FACMensais")
                                )
                              )
                            )
                  )
        )

server <- function (input, output, session) {
  
  output$dados <- renderPlot ({
    req (input$file)
    if (is.null (input$file))
      return (NULL)
    
    plotSerie (input)
  })
  
  output$resultadoGeral <- renderPrint ({
    
    if (input$iniciar == 0)
      return ("Aguardando inicio...")
    
    duracao = funcaoAlgoritmo ( )$duracao
    sprintf ("Duracao: %0.3f seg", duracao)
  })
  
  output$tabelaAvaliacao <- renderDataTable ({
    if (input$iniciar)
      funcaoAlgoritmo ( )$arqAvaliacoes
  })
  
  output$GraficoSerie <- renderPlot ({
    if (input$iniciar) {
      serieS = funcaoAlgoritmo ( )$arqSeries
      if (input$modelo != 1)
        serieS = serieS[[input$nSerie]]
      serieH <- read.csv2 (input$file$datapath,
                          header = input$header,
                          sep = input$sep,
                          dec = input$dec)
      serieH <- serieH[, -1]
      serieH = matrix (serieH, ncol = 12, byrow = T)
      inicializaGraficoSERIE (serieH)
      graficoSERIE (serieH, 'blue')
      graficoSERIE (serieS, 'red')
    }
  })
  
  output$FACAnuais <- renderPlot ({
    if (input$iniciar) {
      serieS = funcaoAlgoritmo ( )$arqSeries
      if (input$modelo != 1)
        serieS = serieS[[input$nSerie]]
      serieH <- read.csv2 (input$file$datapath,
                           header = input$header,
                           sep = input$sep,
                           dec = input$dec)
      serieH <- serieH[, -1]
      serieH = matrix (serieH, ncol = 12, byrow = T)
      inicializaGraficoFACANUAL (serieH, 3)
      graficoFACANUAL (serieH, 3, 'blue')
      graficoFACANUAL (serieS, 3, 'red')
    }
  })
  
  output$FACMensais <- renderPlot ({
    if (input$iniciar) {
      serieS = funcaoAlgoritmo ( )$arqSeries
      if (input$modelo != 1)
        serieS = serieS[[input$nSerie]]
      serieH <- read.csv2 (input$file$datapath,
                           header = input$header,
                           sep = input$sep,
                           dec = input$dec)
      serieH <- serieH[, -1]
      serieH = matrix (serieH, ncol = 12, byrow = T)
      inicializaGraficoMENSAL (serieH, input$lagMensalMAX)
      graficoFACMENSAL (serieH, as.numeric (input$lagMensalMAX), 'blue')
      graficoFACMENSAL (serieS, as.numeric (input$lagMensalMAX), 'red')
    }
  })
  
  funcaoAlgoritmo <- reactive({
    if (input$iniciar)
      isolate (algoritmo (input))
  })
  
  observe ({
    if (input$modelo == 1) {
      updateSelectInput(session, "nSerie",
                        choices = 1,
                        selected = 1)
    }
    else {
      updateSelectInput(session, "nSerie",
                        choices = 1:input$nPop,
                        selected = input$nPop
      )
    }
  })
  
  output$downloadSerie <- downloadHandler(
    write.csv2 (funcaoAlgoritmo ( )$arqSeries, "parametros.csv")
  )
  
}

shinyApp (ui = ui, server = server)