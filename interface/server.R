function (input, output, session) {
  
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