source ('analise/graficoFAC_Anual.R')
source('analise/graficoFAC_Mensal.R')
source('analise/graficoSeries.R')

source ('modelo/cenarioAnual.R')
source ('modelo/cenarioSintetico.R')
source ('modelo/correlograma.R')
source ('modelo/entrada.R')
source ('modelo/medidasEstatisticas.R')
source ('modelo/powell.R')
source ('modelo/sumQuadRes.R')
source ('modelo/volumeUtil.R')

source ('otimizacao/avaliacao.R')
source ('otimizacao/inicializaPop.R')
source ('otimizacao/mecanismos.R')
source ('otimizacao/modeloPMIX.R')
source ('otimizacao/tempo.R')

library (shiny)
library (DT)

  
algoritmo = function (input) {
  lags = c (input$p, input$q, input$P, input$Q)
  dados = input$file$datapath
  serieH = entrada (dados)$serieH
  
  if (input$tipo == 1) {
    inicio = Sys.time ( )
    arquivos = PMIX (dados, lags)
    fim = Sys.time ( )
    series = cenarioSintetico (serieH, arquivos$parametros, lags, 10000)
    avaliacoes = arquivos$parametros
    algoritmo = list (ciclos = arquivos$ciclos, somRes = arquivos$somRes)
  }
  else if (input$tipo == 2) {
    if (input$multiLags) {
      inicio = Sys.time ( )
      arquivos = modeloPMIX (dados)
      fim = Sys.time ( )
    }
    else {
      inicio = Sys.time ( )
      arquivos = NSGA (dados, lags,
                       input$nPop, ((input$pC) / 100), ((input$pM) / 100),
                       input$cicloMax, ((input$MAPEdiferencaMAX)/100))
      fim = Sys.time ( )
      algoritmo = list (ciclos = arquivos$ciclos)
    }
  }
  duracao = difftime (fim, inicio, units = c ("secs"))
  
  final = list (arqSeries = series, arqAvaliacoes = avaliacoes, duracao = duracao, algoritmo = algoritmo)
  return (final)
}

plotSerie <- function (serie) {
  par (lwd = 1, col = 'black')
  plot (NA, main = "Serie Historica", xlim = c (1,12), ylim = c (0, max (serie)), xlab = "", ylab = "Vazoes mensais (m^3/s)", axes = F, type = "n")
  axis (1, 1:12, c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
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
                  mainPanel (plotOutput ("dados"),
                             dataTableOutput("tabelaAnualHist"),
                             dataTableOutput("tabelaMensalHist")
                  )
                  ),
                  
                  tabPanel ("Algoritmo",
                    fluidRow (
                      column (width = 1,
                        numericInput ("p", label = "p", value = 1, min = 0, max = 12),
                        numericInput ("q", label = "q", value = 0, min = 0, max = 12),
                        numericInput ("P", label = "P", value = 0, min = 0, max = 12),
                        numericInput ("Q", label = "Q", value = 0, min = 0, max = 12),
                        checkboxInput ("multiLags", "Multiplos Lags", FALSE)
                      ),
                      column (width = 4,
                              radioButtons ("tipo", label = "Estimacao de parametros",
                                choices = list ("Metodo de Powell" = 1,
                                                "Algoritmo Genetico" = 2), 
                                          selected = 1)
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
                              downloadButton ("downloadSerie", "Download", icon ("save"))
                            ),
                            mainPanel (
                              tabsetPanel (
                                tabPanel("Tabela avaliacoes",
                                         dataTableOutput("tabelaAvaliacao"),
                                         verbatimTextOutput("observacoes")
                                ),
                                tabPanel("Graficos series",
                                         plotOutput("GraficoSerie"),
                                         dataTableOutput("tabelaMedias")
                                ),
                                tabPanel("Graficos FAC anuais",
                                         plotOutput("FACAnuais"),
                                         dataTableOutput("tabelaAnual")
                                ),
                                tabPanel("Graficos FAC mensais",
                                         selectInput ("lagMensalMAX", "lag mensal analisado:", choices = 1:12, selected = 1),
                                         plotOutput ("FACMensais"),
                                         dataTableOutput ("tabelaMensal")
                                )
                              )
                            )
                  )
        )

server <- function (input, output, session) {
  
  serieHist = reactive ({
    serieH = read.csv2 (input$file$datapath,
                        header = input$header,
                        sep = input$sep,
                        dec = input$dec)
    serieH = serieH[, -1]
    serieH = matrix (serieH, ncol = 12, byrow = T)
  })
  
  serieHistAnual = reactive ({
    apply (serieHist ( ), 1, sum)
  })
  
  funcaoAlgoritmo = reactive({
    if (input$iniciar)
      isolate (algoritmo (input))
  })
  
  serieEscolhida = reactive ({
    input$nSerie
    serieS = funcaoAlgoritmo ( )$arqSeries
    if (input$tipo != 1)
      serieS = serieS[[as.numeric (input$nSerie)]]
    
    return (serieS)
  })
  
  serieEscolhidaAnual = reactive ({
    apply (serieEscolhida ( ), 1, sum)
  })
  
  output$resultadoGeral <- renderPrint ({
    if (input$iniciar == 0)
      return ("Aguardando inicio...")
    
    duracao = funcaoAlgoritmo ( )$duracao
    print (paste ("Duracao:", duracao, "seg"))
    
    if (input$tipo == 1) {
      ciclos = funcaoAlgoritmo ( )$algoritmo$ciclos
      somRes = funcaoAlgoritmo ( )$algoritmo$somRes
      
      print ("Metodo de Powell")
      print (paste ("ciclos: ", ciclos))
      print (paste ("Somatorio dos residuos:", somRes))
    }
    else {
      ciclos = funcaoAlgoritmo ( )$algoritmo$ciclos
      
      print("Algoritmo Genetico")
      print (paste ("ciclos: ", ciclos))
    }
  })
  
  output$dados <- renderPlot ({
    req (input$file)
    if (is.null (input$file))
      return (NULL)
    
    plotSerie (serieHist ( ))
  })
  
  output$tabelaMedias <- renderDataTable ({
    if (input$iniciar) {
      MediaHist = apply (serieHist ( ), 2, mean)
      MediaSint = apply (serieEscolhida ( ), 2, mean)
      DesvioHist = apply (serieHist ( ), 2, sd)
      DesvioSint = apply (serieEscolhida ( ), 2, sd)
      medidas = data.frame (MediaHist, MediaSint, DesvioHist, DesvioSint)
      rownames (medidas) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      colnames (medidas) = c ("Media Historica", "Media Sintetica", "Desvio-padrao Historico", "Desvio-padrao Sintetico")
      datatable (medidas)
    }
  })
  
  output$tabelaAnualHist <- renderDataTable ({
    if (! (is.null (input$file))) {
      facAnual = data.frame (as.vector (autocorrelacaoAnual (serieHistAnual ( ), 12)[-1]))
      rownames (facAnual) = paste ("lag", 1:12)
      datatable (facAnual, colnames = NULL) # %>% formatStyle (backgroundColor = styleInterval (c (0, 1), c ('gray', 'yellow'))
    }
  })
  
  output$tabelaMensalHist <- renderDataTable ({
    if  (! (is.null (input$file))) {
      facMensal = data.frame (autocorrelacaoMensal (serieHist ( ), 12)[-1, ])
      colnames (facMensal) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      rownames (facMensal) = paste ("lag", 1:12)
      datatable (facMensal)
    }
  })
  
  output$tabelaAvaliacao <- renderDataTable ({
    if (input$iniciar) {
      if (input$tipo == 1) {
        parametros = funcaoAlgoritmo ( )$arqAvaliacoes[-(1:2)]
        
        phi = matrix (0, ncol = 12)
        tht = matrix (0, ncol = 12)
        PHI = matrix (0, ncol = 12)
        THT = matrix (0, ncol = 12)
        
        limInf = 0
        limSup = 0
        
        if (input$p > 0) {
          limInf = 1
          limSup = 12*input$p
          phi = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        if (input$q > 0) {
          limInf = limSup + 1
          limSup = limInf + 12*input$q - 1
          tht = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        if (input$P > 0) {
          limInf = limSup + 1
          limSup = limInf + 12*input$P - 1
          PHI = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        if (input$Q > 0) {
          limInf = limSup + 1
          limSup = limInf + 12*input$Q - 1
          THT = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        
        parametrosPowell = data.frame (t (phi), t (tht), t (PHI), t (THT))
        colnames (parametrosPowell) = c (rep ("phi", max (1, input$p)), rep ("tht", max (1, input$q)), rep ("PHI", max (1, input$P)), rep ("THT", max (1, input$Q)))
        rownames (parametrosPowell) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
        datatable (parametrosPowell)
      }
      else {
        avaliacoes = data.frame (funcaoAlgoritmo ( )$arqAvaliacoes)
        colnames (avaliacoes) = c ("MAPE media", "MAPE desvio", "MAPE FAC anual", "MAPE FAC mensal", "Soma Residual")
        rownames (avaliacoes) = paste ("Serie", 1:input$nPop)
        datatable (avaliacoes)
      }
    }
  })
  
  output$GraficoSerie <- renderPlot ({
    if (input$iniciar) {
      inicializaGraficoSERIE (serieHist ( ))
      graficoSERIE (serieHist ( ), 'cornflowerblue')
      graficoSERIE (serieEscolhida ( ), 'blue')
    }
  })
  
  output$FACAnuais <- renderPlot ({
    if (input$iniciar) {
      inicializaGraficoFACANUAL (serieHistAnual ( ), 12)
      graficoFACANUAL (serieHistAnual ( ), 12, 'cornflowerblue')
      graficoFACANUAL (serieEscolhidaAnual ( ), 12, 'blue')
    }
  })
  
  output$tabelaAnual <- renderDataTable ({
    if (input$iniciar) {
      facAnual = data.frame (as.vector (autocorrelacaoAnual (serieEscolhidaAnual ( ), 12)[-1]))
      rownames (facAnual) = paste ("lag", 1:12)
      datatable (facAnual, colnames = NULL)
    }
  })
  
  output$FACMensais <- renderPlot ({
    if (input$iniciar) {
      inicializaGraficoMENSAL (serieHist ( ), as.numeric (input$lagMensalMAX))
      graficoFACMENSAL (serieHist ( ), as.numeric (input$lagMensalMAX), 'cornflowerblue')
      graficoFACMENSAL (serieEscolhida ( ), as.numeric (input$lagMensalMAX), 'blue')
    }
  })
  
  output$tabelaMensal <- renderDataTable ({
    if (input$iniciar) {
      facMensal = data.frame (autocorrelacaoMensal (serieEscolhida ( ), 12)[-1, ])
      colnames (facMensal) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      rownames (facMensal) = paste ("lag", 1:12)
      datatable (facMensal)
    }
  })
  
  observe ({
    if (input$tipo == 1) {
      updateSelectInput(session, "nSerie",
                        choices = 1,
                        selected = 1)
    }
    else {
      updateSelectInput (session, "nSerie",
                         choices = 1:input$nPop,
                         selected = input$nPop
      )
    }
  })
  
  output$downloadSerie <- downloadHandler (
    filename = function ( ) {
      paste0 ("serie_", input$nSerie, ".csv")
    },
    content = function (file) {
      write.table (data.frame (serieEscolhida ( )), file,
                   col.names = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"),
                   row.names = F,
                   sep = input$sep,
                   dec = input$dec)
    }
  )
  
  
}

shinyApp (ui = ui, server = server)